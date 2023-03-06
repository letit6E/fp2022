open Base
open Ast
module Format = Caml.Format (* silencing a warning *)

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of texpr * texpr
  | Ptrn_rebound of string
[@@deriving eq, show { with_path = false }]

let pp_error ppf : error -> _ = function
  | Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_texpr l pp_texpr r
  | Ptrn_rebound s ->
    Format.fprintf ppf "Variable %s is bound several times in this matching" s
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> (f a) last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind m ~f = m >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module VarSet = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Type = struct
  type t = texpr

  let rec occurs_in v = function
    | TVar b -> b = v
    | TFun (l, r) -> occurs_in v l || occurs_in v r
    | TFmt t | TList t -> occurs_in v t
    | TTpl (hd :: tl) -> occurs_in v hd || occurs_in v (TTpl tl)
    | TInt | TBool | TTpl [] -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TFun (l, r) -> helper (helper acc l) r
      | TFmt t | TList t -> helper acc t
      | TTpl (hd :: tl) -> helper (helper acc hd) (TTpl tl)
      | TInt | TBool | TTpl [] -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val pp : Caml.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> texpr -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> texpr

  val find : fresh -> t -> texpr option
  val apply : t -> texpr -> texpr
  val unify : texpr -> texpr -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh * texpr) list

  let pp ppf subst =
    let open Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pp_texpr v))
      subst
  ;;

  let empty = []
  let mapping k v = if Type.occurs_in k v then fail Occurs_check else return (k, v)

  let singleton k v =
    match v with
    | TVar b when b = k -> return []
    | _ ->
      let* mapping = mapping k v in
      return [ mapping ]
  ;;

  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find_exn b s with
         | exception Not_found_s _ -> ty
         | x -> x)
      | TFun (l, r) -> TFun (helper l, helper r)
      | TList t -> TList (helper t)
      | TFmt t -> TFmt (helper t)
      | TTpl ts -> TTpl (List.map ts ~f:helper)
      | (TInt | TBool) as other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool | TTpl [], TTpl [] -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TFun (l1, r1), TFun (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TList l, TList r | TFmt l, TFmt r -> unify l r
    | TTpl (hd1 :: tl1), TTpl (hd2 :: tl2) ->
      let* subs1 = unify hd1 hd2 in
      let* subs2 = unify (TTpl tl1) (TTpl tl2) in
      compose subs1 subs2
    | _ -> fail (Unification_failed (l, r))

  and extend s (k, v) =
    match List.Assoc.find s ~equal:Int.equal k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RList.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply s2 v in
        let* mapping = mapping k v in
        return (mapping :: acc))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]
type scheme = S of binder_set * texpr [@@deriving show { with_path = false }]

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  let pp = pp_scheme
end

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Caml.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Caml.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Caml.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> texpr R.t =
 fun (S (bs, t)) ->
  let res =
    VarSet.fold_left_m
      (fun typ name ->
        let* f1 = fresh_var in
        let* s = Subst.singleton name f1 in
        return (Subst.apply s typ))
      bs
      (return t)
  in
  res
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Caml.Not_found) | (exception Not_found_s _) -> fail (No_variable e)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer_ptrn =
  let rec helper env = function
    | PVar b when List.Assoc.mem env ~equal:String.equal b -> fail (Ptrn_rebound b)
    | PVar b ->
      let* tv = fresh_var in
      return (TypeEnv.extend env (b, S (VarSet.empty, tv)), tv)
    | PConst (CInt _) -> return (env, TInt)
    | PConst (CBool _) -> return (env, TBool)
    | PConst (CString (_, _)) -> return (env, TFmt (TTpl []))
    | PConst CNil ->
      let* tv = fresh_var in
      return (env, TList tv)
    | PTpl ps ->
      let* env, ts =
        RList.fold_left
          ps
          ~init:(return (env, []))
          ~f:(fun (env, ts) p ->
            let* env, t = helper env p in
            return (env, t :: ts))
      in
      return (env, TTpl ts)
    | PCons (hd, tl) ->
      let* env, hdt = helper env hd in
      let* env, tlt = helper env tl in
      let* s = unify (TList hdt) tlt in
      return (TypeEnv.apply s env, tlt)
  in
  helper TypeEnv.empty
;;

let infer =
  let rec (helper : TypeEnv.t -> expr -> (Subst.t * texpr) R.t) =
   fun env -> function
    | EVar x -> lookup_env x env
    | EFun (x, e1) ->
      let* tv = fresh_var in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s, ty = helper env2 e1 in
      let trez = TFun (Subst.apply s tv, ty) in
      return (s, trez)
    | EApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (TFun (t2, tv)) in
      let trez = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, trez)
    | EConst c ->
      (match c with
       | CInt _ -> return TInt
       | CBool _ -> return TBool
       | CNil -> fresh_var >>| fun v -> TList v
       | CString (_, fmt_elms) ->
         List.fold_right
           ~init:(return (TTpl []))
           ~f:(fun fmt_elm acc ->
             let* acc = acc in
             match fmt_elm with
             | PlainText _ -> return acc
             | IntSpec -> return (TFun (TInt, acc))
             | StrSpec -> return (TFun (TFmt (TTpl []), acc))
             | BoolSpec -> return (TFun (TBool, acc))
             | ASpec -> fresh_var >>| fun v -> TFun (TFun (v, TTpl []), TFun (v, acc)))
           fmt_elms
         >>| fun t -> TFmt t)
      >>| fun t -> Subst.empty, t
    | EMatch (scr, brnchs) ->
      let* s, scr_t = helper env scr in
      let* t = fresh_var in
      RList.fold_left
        brnchs
        ~init:(return (s, t))
        ~f:(fun (s, t) (p, e) ->
          let* p_env, p_t = infer_ptrn p in
          let env =
            List.fold_left p_env ~init:env ~f:(fun env (k, v) ->
              TypeEnv.extend env (k, v))
          in
          let* s2 = unify scr_t p_t in
          let* s3, e_t = helper env e in
          let* s4 = unify t e_t in
          let* final_subst = Subst.compose_all [ s; s2; s3; s4 ] in
          return (final_subst, Subst.apply final_subst t))
    | ELet ((false, x, e1, _), e2) ->
      let* s1, t1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t2 = generalize env2 t1 in
      let* s2, t3 = helper (TypeEnv.extend env2 (x, t2)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t3)
    | ELet ((true, x, e1, _), e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s1, t1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s2 s1 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2 = helper TypeEnv.(extend (apply s env) (x, t2)) e2 in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2)
    | ETpl es ->
      let* subs, ts =
        RList.fold_left
          es
          ~init:(return (Subst.empty, []))
          ~f:(fun (subs, ts) e ->
            let* subs2, t = helper env e in
            let* final_subst = Subst.compose subs subs2 in
            return (final_subst, t :: ts))
      in
      return (subs, TTpl ts)
  in
  helper
;;

let std_lib_env =
  [ "printf", S (VarSet.singleton (-1), TFun (TFmt (TVar (-1)), TVar (-1)))
  ; "+", S (VarSet.empty, TFun (TInt, TFun (TInt, TInt)))
  ; "-", S (VarSet.empty, TFun (TInt, TFun (TInt, TInt)))
  ; "*", S (VarSet.empty, TFun (TInt, TFun (TInt, TInt)))
  ; "/", S (VarSet.empty, TFun (TInt, TFun (TInt, TInt)))
  ; "=", S (VarSet.singleton (-1), TFun (TVar (-1), TFun (TVar (-1), TBool)))
  ; "!=", S (VarSet.singleton (-1), TFun (TVar (-1), TFun (TVar (-1), TBool)))
  ; ">", S (VarSet.singleton (-1), TFun (TVar (-1), TFun (TVar (-1), TBool)))
  ; "<", S (VarSet.singleton (-1), TFun (TVar (-1), TFun (TVar (-1), TBool)))
  ; ">=", S (VarSet.singleton (-1), TFun (TVar (-1), TFun (TVar (-1), TBool)))
  ; "<=", S (VarSet.singleton (-1), TFun (TVar (-1), TFun (TVar (-1), TBool)))
  ; "||", S (VarSet.empty, TFun (TBool, TFun (TBool, TBool)))
  ; "&&", S (VarSet.empty, TFun (TBool, TFun (TBool, TBool)))
  ; ( "::"
    , S
        ( VarSet.singleton (-1)
        , TFun (TVar (-1), TFun (TList (TVar (-1)), TList (TVar (-1)))) ) )
  ; "~-", S (VarSet.empty, TFun (TInt, TInt))
  ]
;;

let infer_program p =
  run
    (let* _, p =
       RList.fold_left
         p
         ~init:(return (std_lib_env, []))
         ~f:(fun (env, acc) ((is_rec, name, expr, _) as decl) ->
           let* _, t = infer env (ELet (decl, EVar name)) in
           return
             ( TypeEnv.extend env (name, generalize env t)
             , (is_rec, name, expr, Some t) :: acc ))
     in
     return (List.rev p))
;;

(** {3} Tests *)

let test_infer_prog_ok prog =
  match infer_program prog with
  | Ok infered_prog ->
    let res = equal_prog infered_prog prog in
    if res
    then ()
    else
      Format.printf
        "Incorrect inferred prog. Expected: %a. Actual: %a."
        pp_prog
        prog
        pp_prog
        infered_prog;
    res
  | Error err ->
    Format.printf "Unexpected error %a" pp_error err;
    false
;;

let test_infer_prog_err prog expected_err =
  match infer_program prog with
  | Error actual_err ->
    let res = equal_error actual_err expected_err in
    if res
    then ()
    else
      Format.printf
        "Incorrect error. Expected: %a. Actual: %a."
        pp_error
        expected_err
        pp_error
        actual_err;
    res
  | Ok actua_prog ->
    Format.printf "Unexpected success %a" pp_prog actua_prog;
    false
;;

let%test _ = test_infer_prog_ok [ false, "x", EConst (CInt 1), Some TInt ]

let%test _ =
  test_infer_prog_ok
    [ false, "x", ETpl [ EConst (CInt 1); EConst (CInt 1) ], Some (TTpl [ TInt; TInt ]) ]
;;

let%test _ =
  test_infer_prog_ok [ false, "x", EFun ("x", EVar "x"), Some (TFun (TVar 1, TVar 1)) ]
;;

let%test _ =
  test_infer_prog_ok
    [ ( false
      , "x"
      , ELet
          ( (false, "x", EConst (CInt 2), None)
          , EApp (EApp (EVar "+", EVar "x"), EConst (CInt 1)) )
      , Some TInt )
    ]
;;

let%test _ =
  test_infer_prog_ok
    [ ( false
      , "x"
      , EMatch (EConst (CInt 1), [ PConst (CInt 1), EConst (CInt 1) ])
      , Some TInt )
    ]
;;
