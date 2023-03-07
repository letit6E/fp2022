(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string

val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type bin_op =
  | And
  | Or
  | Less
  | Leq
  | Gre
  | Geq
  | Eq
  | Neq
  | Add
  | Sub
  | Mul
  | Div

and un_op =
  | Not
  | Minus

and binding = bool * pt * exp
and case = pt * exp
and decl = DLet of binding
and prog = decl list

and const =
  | CString of id
  | CInt of int
  | CBool of bool

and exp =
  | EConst of const
  | ENone
  | ENil
  | EUnOp of un_op * exp
  | EVar of id
  | ECons of exp * exp
  | ETuple of exp list
  | ELet of binding list * exp
  | EFun of pt * exp
  | EMatch of exp * case list
  | ESome of exp
  | EArg of exp
  | EIf of exp * exp * exp
  | EBinOp of bin_op * exp * exp
  | EApp of exp * exp

and pt =
  | PtWild
  | PtVar of id
  | PtConst of const
  | PtCons of pt * pt
  | PtSome of pt
  | PtNone
  | PtNil
  | PtTuple of pt list

val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string
val pp_un_op : Format.formatter -> un_op -> unit
val show_un_op : un_op -> string
val pp_binding : Format.formatter -> binding -> unit
val show_binding : binding -> string
val pp_case : Format.formatter -> case -> unit
val show_case : case -> string
val pp_decl : Format.formatter -> decl -> unit
val show_decl : decl -> string
val pp_prog : Format.formatter -> prog -> unit
val show_prog : prog -> string
val pp_const : Format.formatter -> const -> unit
val show_const : const -> string
val pp_exp : Format.formatter -> exp -> unit
val show_exp : exp -> string
val pp_pt : Format.formatter -> pt -> unit
val show_pt : pt -> string
