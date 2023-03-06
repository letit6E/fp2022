(** variables name [a-z_][0-9a-zA-Z_] *)
type id = string [@@deriving show { with_path = false }]

type bin_op =
  | And (**  and *)
  | Or (**  or  *)
  | Less (**  <   *)
  | Leq (**  <=  *)
  | Gre (**  >   *)
  | Geq (**  >=  *)
  | Eq (** =   *)
  | Neq (**  <>  *)
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  *   *)
  | Div (**  div   *)
[@@deriving show { with_path = false }]

and un_op =
  | Not (**  not  *)
  | Minus (** ~ *)
[@@deriving show { with_path = false }]

and binding = bool * pt * exp [@@deriving show { with_path = false }]
and case = pt * exp [@@deriving show { with_path = false }]
and decl = DLet of binding (**  val y = 256   *) [@@deriving show { with_path = false }]
and prog = decl list [@@deriving show { with_path = false }]

and const =
  | CString of string (**  "xyz"  *)
  | CInt of int (**   256    *)
  | CBool of bool (**  false   *)
[@@deriving show { with_path = false }]

and exp =
  | EConst of const (**    false    *)
  | ENone (**    NONE    *)
  | ENil (** [] *)
  | EUnOp of un_op * exp (**    not x, ~x, !x    *)
  | EVar of id (**    x    *)
  | ECons of exp * exp (**    x :: xs    *)
  | ETuple of exp list (**    x, y, z    *)
  | ELet of binding list * exp (**    let x = 256 in 512    *)
  | EFun of pt * exp (**   fn x => x   *)
  | EMatch of exp * case list (**    case x of 1 => 2 | _ => x * 1337    *)
  | ESome of exp (**    SOME a    *)
  | EArg of exp (**    arg    *)
  | EIf of exp * exp * exp (**    if predicate then x else y    *)
  | EBinOp of bin_op * exp * exp (**    25 div (7 + ~2)    *)
  | EApp of exp * exp (**    fold a list init    *)
[@@deriving show { with_path = false }]

and pt =
  | PtWild (**  _  *)
  | PtVar of id (**  xyz   *)
  | PtConst of const (**  256   *)
  | PtCons of pt * pt (**  hd :: tl  *)
  | PtSome of pt (**   SOME a  *)
  | PtNone (**  NONE  *)
  | PtNil (**  []  *)
  | PtTuple of pt list (**  x, y, z   *)
[@@deriving show { with_path = false }]
