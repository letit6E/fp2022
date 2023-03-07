open Angstrom
open Ast

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter ch = is_lower ch || is_upper ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

let is_wspace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let pwspace = take_while is_wspace
let pwspace1 = take_while1 is_wspace
let ptkn p = pwspace *> p
let ptkn1 p = pwspace1 *> p
let pstkn s = ptkn @@ string s
let pstkn1 s = ptkn1 @@ string s
let pparens p = pstkn "(" *> p <* pstkn ")"
let cint n = CInt n
let cbool b = CBool b
let cstring s specs = CString (s, specs)
let cnil = CNil
let econst c = EConst c
let evar id = EVar id
let etpl pl = ETpl pl
let elet binds e = ELet (binds, e)
let efunction brnchs = EFun ("match", EMatch (EVar "match", brnchs))
let etpl es = ETpl es
let eunit = etpl []
let eapp = return (fun e1 e2 -> EApp (e1, e2))
let ematch e brnchs = EMatch (e, brnchs)
let eapp fn arg = EApp (fn, arg)
let efun lhs rhs = EFun (lhs, rhs)
let cbrnch p e = p, e
let bbind isrec p e = isrec, p, e
let pvar id = PVar id
let pconst c = PConst c
let plain_text s = PlainText s
let ptpl pl = PTpl pl
let popcons = pstkn "::" *> return (fun p1 p2 -> PCons (p1, p2))
let pcons = return (fun p1 p2 -> PCons (p1, p2))
let plist = List.fold_right ~f:(fun p1 p2 -> PCons (p1, p2)) ~init:(PConst CNil)

let pid =
  ptkn
  @@ lift2
       (fun hd tl -> Char.escaped hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_lower ch))
       (take_while (fun ch -> ch = '_' || is_letter ch || is_digit ch))
  >>= fun s ->
  if is_keyword s
  then fail "keyword reserved"
  else if s = "_"
  then fail "`_` isn't supported"
  else return s
;;

let ptupple pelm = pparens @@ sep_by (pstkn ",") pelm

let psign =
  choice [ pstkn "+" *> return 1; pstkn "-" *> return (-1); pstkn "" *> return 1 ]
;;

let pcint =
  ptkn
  @@ lift2
       (fun sign v -> cint (sign * v))
       psign
       (take_while is_digit
       >>= fun s ->
       match int_of_string_opt s with
       | Some x -> return x
       | None -> fail "not an integer")
;;

let pspec =
  pstkn "%"
  *> choice
       [ string "d" *> return IntSpec
       ; string "i" *> return IntSpec
       ; string "a" *> return ASpec
       ; string "s" *> return StrSpec
       ; string "b" *> return BoolSpec
       ]
;;

let replace_new_lines s =
  let rec helper acc = function
    | '\\' :: 'n' :: s -> helper ('\n' :: acc) s
    | ch :: s -> helper (ch :: acc) s
    | [] -> List.rev acc
  in
  String.of_seq @@ List.to_seq @@ helper [] @@ List.of_seq @@ String.to_seq s
;;

let pplain_text = plain_text <$> (replace_new_lines <$> take_while1 (fun ch -> ch != '%'))
let pformat_elm = pspec <|> pplain_text
let pformat = many pformat_elm
let str_to_spec_tys = Angstrom.parse_string pformat ~consume:Consume.All

let pcstring =
  ptkn @@ (string "\"" *> take_while (fun ch -> ch != '"'))
  <* string "\""
  >>= fun s ->
  Result.fold
    ~ok:(fun specs -> return @@ cstring s specs)
    ~error:(fun err -> fail err)
    (str_to_spec_tys s)
;;

let pcbool = cbool <$> (pstkn "true" *> return true <|> pstkn "false" *> return false)
let pcnil = pstkn "[]" *> return cnil
let pcons p1 p2 = PCons (p1, p2)
let pconst const = PConst const
let p_const = choice [ pcint; pcstring; pcbool; pcnil ]
let ppvar = pvar <$> pid
let ppconst = pconst <$> p_const
let punit = ptpl []

let pptrn =
  fix
  @@ fun pptrn ->
  let term = choice [ pparens pptrn; ppconst; ppvar ] in
  let term = chainr1 term (pstkn "::" *> return (fun p1 p2 -> PCons (p1, p2))) in
  (fun l ->
    match l with
    | [ p ] -> p
    | _ -> ptpl l)
  <$> sep_by1 (pstkn ",") term
  <|> pstkn "()" *> return punit
;;

let pdecl_base pexpr =
  pstkn "let"
  *> lift3
       (fun is_rec name expr -> is_rec, name, expr, None)
       (pstkn1 "rec" *> return true <|> return false)
       (ptkn1 pid)
       (pstkn "=" *> pexpr)
;;

let pbrnch pkey pexpr = lift2 (fun k v -> k, v) (pstkn "|" *> pkey <* pstkn "->") pexpr
let peconst = econst <$> p_const
let pevar = evar <$> pid
let pelet pexpr = lift2 elet (pdecl_base pexpr) (pstkn1 "in" *> ptkn1 pexpr)

let pematch pexpr =
  lift2
    ematch
    (pstkn "match" *> ptkn1 pexpr <* pstkn1 "with")
    (many1 @@ pbrnch pptrn pexpr)
;;

let pefun pexpr =
  pstkn "fun" *> lift2 (fun s e -> efun s e) (ptkn1 pid <* pstkn "->") pexpr
;;

let peapp pexpr = lift2 eapp pexpr (ptkn1 pexpr)

let pebinop chain1 term binops =
  chain1
    term
    ((fun op expr1 expr2 -> eapp (eapp (evar op) expr1) expr2)
    <$> choice (List.map pstkn binops))
;;

let pexpr =
  fix (fun pexpr ->
    let term = choice [ pstkn "()" *> return eunit; pparens pexpr; peconst; pevar ] in
    let term =
      lift2 (fun expr l -> List.fold_left eapp expr l) term (many (ptkn1 term))
    in
    let term =
      lift2
        (fun l expr -> List.fold_left (fun expr _ -> eapp (evar "~-") expr) expr l)
        (many (pstkn "-"))
        term
    in
    let term = pebinop chainl1 term [ "*"; "/" ] in
    let term = pebinop chainl1 term [ "+"; "-" ] in
    let term = pebinop chainr1 term [ "::" ] in
    let term = pebinop chainl1 term [ "!="; "<="; "<"; "="; ">="; ">" ] in
    let term = pebinop chainr1 term [ "&&" ] in
    let term = pebinop chainr1 term [ "||" ] in
    let term =
      (fun l ->
        match l with
        | [ expr ] -> expr
        | _ -> ETpl l)
      <$> sep_by1 (pstkn ",") term
    in
    let term = pebinop chainr1 term [ ":=" ] in
    let term = choice [ pelet pexpr; pematch pexpr; pefun pexpr; term ] in
    term)
;;

let pdecl = ptkn (pdecl_base pexpr)
let pdecl_delim = many (pstkn ";;") *> pwspace
let pprogram = pdecl_delim *> many (pdecl <* pdecl_delim)
let parse_program s = parse_string ~consume:Consume.All pprogram s

(* TESTS *)

let test_parser p eq pp code expected =
  match Angstrom.parse_string ~consume:Consume.All p code with
  | Ok ok ->
    (match eq ok expected with
     | true -> true
     | false ->
       Format.printf "Expected: %a\nActual: %a\n" pp expected pp ok;
       false)
  | Error err ->
    Format.printf "Error: %s\n" err;
    false
;;

let%test _ = test_parser pformat_elm equal_format_elm pp_format_elm {|a|} (PlainText "a")
let%test _ = test_parser pformat_elm equal_format_elm pp_format_elm {|%a|} ASpec
let%test _ = test_parser pformat_elm equal_format_elm pp_format_elm {|%d|} IntSpec
let%test _ = test_parser pformat_elm equal_format_elm pp_format_elm {|%b|} BoolSpec
let%test _ = test_parser pformat_elm equal_format_elm pp_format_elm {|%s|} StrSpec

let%test _ =
  test_parser
    pformat
    (List.equal equal_format_elm)
    (Format.pp_print_list pp_format_elm)
    {|%d|}
    [ IntSpec ]
;;

let%test _ =
  test_parser
    pformat
    (List.equal equal_format_elm)
    (Format.pp_print_list pp_format_elm)
    {|x|}
    [ PlainText "x" ]
;;

let%test _ =
  test_parser pcstring equal_const pp_const {|"xz"|} (cstring "xz" [ PlainText "xz" ])
;;

let%test _ = test_parser pcstring equal_const pp_const {|""|} (cstring "" [])

let%test _ =
  test_parser
    pcstring
    equal_const
    pp_const
    {|"%d%d%d%d%d"|}
    (cstring "%d%d%d%d%d" [ IntSpec; IntSpec; IntSpec; IntSpec; IntSpec ])
;;

let%test _ =
  test_parser
    pcstring
    equal_const
    pp_const
    {|"x%ds%a"|}
    (cstring "x%ds%a" [ PlainText "x"; IntSpec; PlainText "s"; ASpec ])
;;

let%test _ =
  test_parser
    pptrn
    equal_ptrn
    pp_ptrn
    {|1 :: 2 :: []|}
    (PCons (PConst (CInt 1), PCons (PConst (CInt 2), PConst CNil)))
;;

let%test _ =
  test_parser
    pexpr
    equal_expr
    pp_expr
    {|let f = fun x -> match x with | 1 -> 1 | 3 -> match 1 with | 2 -> 0 in 1|}
    (ELet
       ( ( false
         , "f"
         , EFun
             ( "x"
             , EMatch
                 ( EVar "x"
                 , [ PConst (CInt 1), EConst (CInt 1)
                   ; ( PConst (CInt 3)
                     , EMatch (EConst (CInt 1), [ PConst (CInt 2), EConst (CInt 0) ]) )
                   ] ) )
         , None )
       , EConst (CInt 1) ))
;;
