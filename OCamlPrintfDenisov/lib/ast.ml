type id = string [@@deriving eq, show { with_path = false }]

type texpr =
  | TBool
  | TInt
  | TVar of int
  | TFmt of texpr
  | TTpl of texpr list
  | TList of texpr
  | TFun of texpr * texpr
[@@deriving eq, show { with_path = false }]

type format_elm =
  | PlainText of string
  | StrSpec
  | BoolSpec
  | IntSpec
  | ASpec
[@@deriving eq, show { with_path = false }]

type const =
  | CNil
  | CBool of bool
  | CInt of int
  | CString of string * format_elm list
[@@deriving eq, show { with_path = false }]

type ptrn =
  | PConst of const
  | PVar of id
  | PCons of ptrn * ptrn
  | PTpl of ptrn list
[@@deriving eq, show { with_path = false }]

type brnch = ptrn * expr

and expr =
  | EConst of const
  | EVar of id
  | ETpl of expr list
  | ELet of bnd * expr
  | EFun of id * expr
  | EApp of expr * expr
  | EMatch of expr * brnch list
[@@deriving eq, show { with_path = false }]

and bnd = bool * id * expr * texpr option [@@deriving eq, show { with_path = false }]

type prog = bnd list [@@deriving eq, show { with_path = false }]

let pp_const fmt = function
  | CNil -> Format.fprintf fmt "[]"
  | CInt n -> Format.fprintf fmt "%d" n
  | CBool b -> Format.fprintf fmt "%b" b
  | CString (s, _) -> Format.fprintf fmt "%S" s
;;

let rec pp_ptrn fmt = function
  | PConst c -> pp_const fmt c
  | PVar x -> Format.fprintf fmt "%s" x
  | PCons (p1, p2) -> Format.fprintf fmt "(%a :: %a)" pp_ptrn p1 pp_ptrn p2
  | PTpl l ->
    Format.fprintf
      fmt
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_ptrn)
      l
;;

let rec pp_texpr fmt = function
  | TInt -> Format.fprintf fmt "int"
  | TFmt (TTpl []) -> Format.fprintf fmt "string"
  | TBool -> Format.fprintf fmt "bool"
  | TTpl [] -> Format.fprintf fmt "unit"
  | TTpl tys ->
    Format.fprintf
      fmt
      "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ")
         (fun fmt ty -> pp_texpr fmt ty))
      (List.rev tys)
  | TList ty -> Format.fprintf fmt "%a list" pp_texpr ty
  | TVar b -> Format.fprintf fmt "'%d" b
  | TFun (arg_ty, ret_ty) ->
    Format.fprintf fmt "(%a -> %a)" pp_texpr arg_ty pp_texpr ret_ty
  | TFmt f -> Format.fprintf fmt "%a format" pp_texpr f
;;

let pp_brnch fmt (p, e) = Format.fprintf fmt "%a -> %a" pp_ptrn p pp_expr e

let rec pp_expr fmt = function
  | EConst c -> pp_const fmt c
  | EVar x -> Format.fprintf fmt "%s" x
  | ETpl l ->
    Format.fprintf
      fmt
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
      l
  | ELet ((is_rec, name, e1, _), e2) ->
    Format.fprintf
      fmt
      "(let %t%s = %a in %a)"
      (fun fmt -> if is_rec then Format.fprintf fmt "rec " else ())
      name
      pp_expr
      e1
      pp_expr
      e2
  | EFun (arg, body) -> Format.fprintf fmt "(fun %s -> %a)" arg pp_expr body
  | EApp (f, arg) -> Format.fprintf fmt "(%a %a)" pp_expr f pp_expr arg
  | EMatch (scr, brs) ->
    Format.fprintf
      fmt
      "(match %a with | %a)"
      pp_expr
      scr
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ") pp_brnch)
      brs
;;
