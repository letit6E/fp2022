open Ast
open Format
open Parser
module BindMap = Map.Make (String)
module BindSet = Set.Make (String)

module type ERROR_MONAD = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val fold : ('a, 'e) t -> ok:('a -> 'b) -> err:('e -> 'b) -> 'b
end

module Interpreter (M : ERROR_MONAD) : sig
  type value
  type r_ok
  type r_err

  val pp_r_ok : formatter -> r_ok -> unit
  val pp_r_err : formatter -> r_err -> unit
  val equal_value : value -> value -> bool
  val pp_value : formatter -> value -> unit
  val vbool : bool -> value
  val vint : int -> value
  val vstring : string -> format_elm list -> value
  val vtpl : value list -> value
  val vlist : value list -> value
  val get_value : r_ok -> id -> (texpr option * value) option
  val run : bnd list -> (r_ok, r_err) M.t
end = struct
  open M

  let ( let* ) m f = bind m ~f

  type r_err =
    | Div0
    | Cmp_fun
    | Unbound of string
    | Incorrect_texpr of value
    | Non_exhaustive of ptrn list

  and value =
    | VInt of int
    | VString of string * format_elm list
    | VBool of bool
    | VTpl of value list
    | VList of value list
    | VFun of ((value, r_err) t Lazy.t -> (value, r_err) t)

  type env = value BindMap.t

  let rec equal_value v1 v2 =
    match v1, v2 with
    | VInt n, VInt m -> n = m
    | VString (s1, _), VString (s2, _) -> s1 = s2
    | VBool b1, VBool b2 -> b1 = b2
    | VTpl vs1, VTpl vs2 -> List.for_all2 (fun v1 v2 -> equal_value v1 v2) vs1 vs2
    | VList vs1, VList vs2 -> List.for_all2 (fun v1 v2 -> equal_value v1 v2) vs1 vs2
    | _ -> false
  ;;

  let vbool b = VBool b
  let vint n = VInt n
  let vstring s frmts = VString (s, frmts)
  let vtpl l = VTpl l
  let vlist l = VList l
  let vlfun f = VFun f
  let vfun f = vlfun (fun x -> Lazy.force x >>= f)
  let vfun2 f = vfun (fun x -> return (vfun (fun y -> f (x, y))))

  let rec pp_value fmt = function
    | VInt n -> fprintf fmt "%d" n
    | VString (s, _) -> fprintf fmt "%S" s
    | VBool b -> fprintf fmt "%b" b
    | VTpl l ->
      fprintf
        fmt
        "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_value)
        l
    | VList l ->
      fprintf
        fmt
        "[%a]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_value)
        l
    | VFun _ -> fprintf fmt "<fun>"
  ;;

  let pp_r_err fmt = function
    | Div0 -> fprintf fmt "Division by zero"
    | Cmp_fun -> fprintf fmt "Compare: functional value"
    | Unbound s -> fprintf fmt "Unbound value %s" s
    | Incorrect_texpr v -> fprintf fmt "Value %a has incorrect type" pp_value v
    | Non_exhaustive ps ->
      fprintf
        fmt
        "This pattern-matching is not exhaustive:\n%a"
        (pp_print_list pp_ptrn)
        ps
  ;;

  let pp_env fmt env =
    BindMap.iter
      (fun k v -> Format.fprintf fmt "%s=%a\n" k (fun fmt v -> pp_value fmt v) v)
      env
  ;;

  let pp_r_ok_elm fmt (name, (texpr, value)) =
    match texpr with
    | Some texpr ->
      Format.fprintf fmt "val %s : %a = %a" name pp_texpr texpr pp_value value
    | None -> Format.fprintf fmt "val %s = %a" name pp_value value
  ;;

  type r_ok = (string * (texpr option * value)) list

  let pp_r_ok = pp_print_list ~pp_sep:pp_force_newline pp_r_ok_elm
  let get_value r_ok name = List.assoc_opt name r_ok

  let int_binop f =
    vfun2 (function
      | VInt x, VInt y ->
        (try return (VInt (f x y)) with
         | Division_by_zero -> fail Div0)
      | x, y -> fail (Incorrect_texpr (VTpl [ x; y ])))
  ;;

  let rec cmp_value = function
    | VBool x, VBool y -> return (Bool.compare x y)
    | VInt x, VInt y -> return (Int.compare x y)
    | VString (x, _), VString (y, _) -> return (String.compare x y)
    | VTpl (x :: xs), VTpl (y :: ys) | VList (x :: xs), VList (y :: ys) ->
      let* cmp = cmp_value (x, y) in
      (match cmp with
       | 0 -> cmp_value (VTpl xs, VTpl ys)
       | _ -> return cmp)
    | VTpl [], VTpl (_ :: _) | VList [], VList (_ :: _) -> return (-1)
    | VTpl (_ :: _), VTpl [] | VList (_ :: _), VList [] -> return 1
    | VTpl [], VTpl [] | VList [], VList [] -> return 0
    | VFun _, VFun _ -> fail Cmp_fun
    | x, y -> fail (Incorrect_texpr (VTpl [ x; y ]))
  ;;

  let cmp_binop f = vfun2 (fun p -> cmp_value p >>| fun cmp -> VBool (f cmp 0))

  let lookup_val name env =
    match BindMap.find_opt name env with
    | None ->
      (match name with
       | "+" -> return (int_binop ( + ))
       | "-" -> return (int_binop ( - ))
       | "*" -> return (int_binop ( * ))
       | "/" -> return (int_binop ( / ))
       | "=" -> return (cmp_binop ( = ))
       | "!=" -> return (cmp_binop ( != ))
       | "<" -> return (cmp_binop ( < ))
       | "<=" -> return (cmp_binop ( <= ))
       | ">" -> return (cmp_binop ( > ))
       | ">=" -> return (cmp_binop ( >= ))
       | "||" ->
         return
           (vfun (function
             | VBool true -> return (vlfun (fun _ -> return (VBool true)))
             | x ->
               return
                 (vfun (fun y ->
                    match x, y with
                    | VBool _, VBool b -> return (VBool b)
                    | _, _ -> fail (Incorrect_texpr (VTpl [ x; y ]))))))
       | "&&" ->
         return
           (vfun (function
             | VBool false -> return (vlfun (fun _ -> return (VBool false)))
             | x ->
               return
                 (vfun (fun y ->
                    match x, y with
                    | VBool _, VBool b -> return (VBool b)
                    | _, _ -> fail (Incorrect_texpr (VTpl [ x; y ]))))))
       | "::" ->
         return
           (vfun2 (function
             | hd, VList tl -> return (VList (hd :: tl))
             | x, y -> fail (Incorrect_texpr (VTpl [ x; y ]))))
       | "~-" ->
         return
           (vfun (function
             | VInt x -> return (VInt (-x))
             | x -> fail (Incorrect_texpr x)))
       | "printf" ->
         return
           (vfun (function
             | VString (_, fmt_elms) ->
               let rec helper scheduled_prints = function
                 | [] -> scheduled_prints () >>| fun () -> VTpl []
                 | PlainText text :: fmt_elms ->
                   helper
                     (fun () -> scheduled_prints () >>| fun () -> printf "%s" text)
                     fmt_elms
                 | IntSpec :: fmt_elms ->
                   return
                     (vfun (function
                       | VInt n ->
                         helper
                           (fun () -> scheduled_prints () >>| fun () -> printf "%d" n)
                           fmt_elms
                       | v -> fail (Incorrect_texpr v)))
                 | StrSpec :: fmt_elms ->
                   return
                     (vfun (function
                       | VString (s, _) ->
                         helper
                           (fun () -> scheduled_prints () >>| fun () -> printf "%s" s)
                           fmt_elms
                       | v -> fail (Incorrect_texpr v)))
                 | BoolSpec :: fmt_elms ->
                   return
                     (vfun (function
                       | VBool b ->
                         helper
                           (fun () -> scheduled_prints () >>| fun () -> printf "%b" b)
                           fmt_elms
                       | v -> fail (Incorrect_texpr v)))
                 | ASpec :: fmt_elms ->
                   return
                     (vfun2 (fun (printer, printed) ->
                        match printer with
                        | VFun printer ->
                          helper
                            (fun () ->
                              scheduled_prints ()
                              >>= (fun () -> printer (lazy (return printed)))
                              >>= function
                              | VTpl [] -> return ()
                              | v -> fail (Incorrect_texpr v))
                            fmt_elms
                        | v -> fail (Incorrect_texpr v)))
               in
               helper (fun () -> return ()) fmt_elms
             | x -> fail (Incorrect_texpr x)))
       | _ -> fail (Unbound name))
    | Some r -> return r
  ;;

  (* None if ptrn doesn't match value *)
  let rec case_env value ptrn =
    match ptrn, value with
    | PVar s, _ -> Some (return (BindMap.add s value BindMap.empty))
    | PConst (CInt c), VInt v when c = v -> Some (return BindMap.empty)
    | PConst (CInt _), VInt _ -> None
    | PConst (CString (c, _)), VString (v, _) when c = v -> Some (return BindMap.empty)
    | PConst (CString _), VString _ -> None
    | PConst (CBool c), VBool v when c = v -> Some (return BindMap.empty)
    | PConst (CBool _), VBool _ -> None
    | PConst CNil, VList [] -> Some (return BindMap.empty)
    | PConst CNil, VList _ -> None
    | PTpl ps, VTpl vs ->
      (match vs, ps with
       | [], [] -> Some (return BindMap.empty)
       | v :: vs, p :: ps ->
         mrg_case_envs (case_env v p) (fun () -> case_env (VTpl vs) (PTpl ps))
       | _ -> Some (fail (Incorrect_texpr value)))
    | PCons (phd, ptl), VList vls ->
      (match vls with
       | [] -> None
       | vhd :: vtl ->
         mrg_case_envs (case_env vhd phd) (fun () -> case_env (VList vtl) ptl))
    | _ -> Some (fail (Incorrect_texpr value))

  and mrg_case_envs env1 env2 =
    Option.bind env1 (fun env1 ->
      fold
        env1
        ~ok:(fun env1 ->
          Option.map
            (fun env2 -> env2 >>| fun env2 -> BindMap.add_seq (BindMap.to_seq env2) env1)
            (env2 ()))
        ~err:(fun err -> Some (fail err)))
  ;;

  let rec eval expr env =
    match expr with
    | EVar name -> lookup_val name env
    | EConst (CInt n) -> return (vint n)
    | EConst (CString (s, frmts)) -> return (vstring s frmts)
    | EConst (CBool b) -> return (vbool b)
    | EConst CNil -> return (vlist [])
    | EApp (fn, arg) ->
      eval fn env
      >>= (function
      | VFun fv -> fv (lazy (eval arg env))
      | v -> fail (Incorrect_texpr v))
    | ETpl l -> all (List.map (fun e -> eval e env) l) >>| vtpl
    | ELet (decl, expr) -> add_bnd decl env >>= eval expr
    | EMatch (scr, cases) ->
      eval scr env
      >>= fun value ->
      (match
         List.find_map
           (fun (ptrn, body) -> Option.map (fun env -> env, body) (case_env value ptrn))
           cases
       with
       | Some (case_env, body) ->
         case_env
         >>= fun case_env -> eval body (BindMap.union (fun _ x _ -> Some x) case_env env)
       | None -> fail (Non_exhaustive (List.map fst cases)))
    | EFun (prm, body) -> return (vfun (fun arg -> eval body (BindMap.add prm arg env)))

  and add_bnd (is_rec, name, expr, t) env =
    if is_rec
    then (
      match expr with
      | EFun (prm_name, body) ->
        let rec eval_self prm_v =
          eval body (BindMap.add prm_name prm_v (BindMap.add name (vfun eval_self) env))
        in
        return (BindMap.add name (vfun eval_self) env)
      | _ -> add_bnd (false, name, expr, t) env)
    else eval expr env >>| fun v -> BindMap.add name v env
  ;;

  let run p =
    List.fold_left
      (fun acc ((_, name, _, texpr) as bnd) ->
        acc
        >>= fun (env, vals) ->
        add_bnd bnd env
        >>= fun env ->
        lookup_val name env
        >>| fun v -> env, (name, (texpr, v)) :: List.remove_assoc name vals)
      (return (BindMap.empty, []))
      p
    >>| fun (_, res) -> List.rev res
  ;;
end

module InterpreterResult = Interpreter (struct
  include Base.Result

  let fold res ~ok ~err =
    match res with
    | Ok v -> ok v
    | Error v -> err v
  ;;
end)

(*TESTS*)
open InterpreterResult

let parse_and_run str =
  let ans =
    match parse_program str with
    | Ok program -> run program
    | Error err ->
      eprintf "Parsing error:%s\n%!" err;
      exit 1
  in
  ans
;;

let test_parse_and_run str vals =
  match parse_and_run str with
  | Error err ->
    printf "%a\n" pp_r_err err;
    false
  | Ok ok ->
    List.for_all
      (fun (name, expected) ->
        match get_value ok name with
        | None ->
          printf "%s is not bound:\n%a\n" name pp_r_ok ok;
          false
        | Some (_, actual) when equal_value expected actual -> true
        | Some (_, actual) ->
          printf
            "Incorrect %s, expected: %a, actual: %a\n"
            name
            pp_value
            expected
            pp_value
            actual;
          false)
      vals
;;

let%test _ =
  test_parse_and_run
    {|
let rec fix = fun f -> fun eta -> f (fix f) eta

let fact =
  fix (fun fact -> fun n ->
    match n with
    | 0 -> 1
    | m -> m * fact (n - 1))

let fact5 = fact 5
|}
    [ "fact5", vint 120 ]
;;

let%test _ =
  test_parse_and_run
    {|
  let rec map = fun f -> fun l -> match l with 
  | [] -> []
  | a::l -> let r = f a in r :: map f l
  let n = 5 :: 7 :: 9:: []
  let sq = fun x -> x * x
  let sqs = map sq n
|}
    [ "sqs", vlist [ vint 25; vint 49; vint 81 ] ]
;;

let%expect_test _ =
  let _ = parse_and_run {|
let ignore = printf "hrt"
|} in
  [%expect {| hrt |}]
;;

let%expect_test _ =
  let _ = parse_and_run {|
let ignore = printf "%d" 1 
|} in
  [%expect {| 1 |}]
;;

let%expect_test _ =
  let _ = parse_and_run {|
let ignore = printf "%b" true 
|} in
  [%expect {| true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_run
      {|
let rec print_int_list = fun l ->
  match l with
  | [] -> ()
  | hd :: tl -> printf "%d; %a" hd print_int_list tl
  
let ignore = printf "[%a]" print_int_list (8 :: 3 :: 7 :: [])
|}
  in
  [%expect {| [8; 3; 7; ] |}]
;;

let%expect_test _ =
  let _ =
    parse_and_run
      {|
let rec print_list = fun print_elm -> fun l ->
  match l with
  | [] -> ()
  | hd :: tl -> printf "%a; %a" print_elm hd (print_list print_elm) tl
  
  let ignore = printf "[%a]" (print_list (printf "%s")) ("a" :: "" :: "dqw" :: [])
|}
  in
  [%expect {| [a; ; dqw; ] |}]
;;
