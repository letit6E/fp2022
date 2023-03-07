(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : 'a Angstrom.t -> string -> ('a, string) result
val is_empty : char -> bool
val not_is_empty : char -> bool
val empty : string Angstrom.t
val empty_lr : 'a Angstrom.t -> 'a Angstrom.t
val is_digit : char -> bool
val is_lower : char -> bool
val is_upper : char -> bool
val is_wild : char -> bool
val is_valid_ident : char -> bool
val is_keyword : string -> bool
val token : string -> string Angstrom.t
val keyword : string -> string Angstrom.t
val between : 'a Angstrom.t -> 'b Angstrom.t -> 'c Angstrom.t -> 'c Angstrom.t
val brackets : 'a Angstrom.t -> 'a Angstrom.t
val cint : int -> Ast.const
val cbool : bool -> Ast.const
val cstring : string -> Ast.const
val econst : Ast.const -> Ast.exp
val eunop : Ast.un_op -> Ast.exp -> Ast.exp
val evar : string -> Ast.exp
val etuple : Ast.exp list -> Ast.exp
val econs : Ast.exp -> Ast.exp -> Ast.exp
val eif : Ast.exp -> Ast.exp -> Ast.exp -> Ast.exp
val elet : Ast.binding list -> Ast.exp -> Ast.exp
val efunction : Ast.case list -> Ast.exp
val efun : Ast.pt -> Ast.exp -> Ast.exp
val eapp : (Ast.exp -> Ast.exp -> Ast.exp) Angstrom.t
val ematch : Ast.exp -> Ast.case list -> Ast.exp
val efun : Ast.pt list -> Ast.exp -> Ast.exp
val ebinop : Ast.bin_op -> Ast.exp -> Ast.exp -> Ast.exp
val elist : Ast.exp list -> Ast.exp
val ccase : 'a -> 'b -> 'a * 'b
val bbind : 'a -> 'b -> 'c -> 'a * 'b * 'c
val ptwild : 'a -> Ast.pt
val ptvar : string -> Ast.pt
val ptconst : Ast.const -> Ast.pt
val pttuple : Ast.pt list -> Ast.pt
val popcons : (Ast.pt -> Ast.pt -> Ast.pt) Angstrom.t
val plist : Ast.pt list -> Ast.pt
val dlet : bool -> Ast.pt -> Ast.exp -> Ast.decl
val procr : ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t -> 'a Angstrom.t -> 'a Angstrom.t
val procl : ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t -> 'a Angstrom.t -> 'a Angstrom.t
val choice_op : (string * Ast.bin_op) list -> (Ast.exp -> Ast.exp -> Ast.exp) Angstrom.t
val cons : (Ast.exp -> Ast.exp -> Ast.exp) Angstrom.t
val apply_unary : Ast.exp Angstrom.t -> Ast.exp Angstrom.t
val id : (char -> bool) -> string Angstrom.t
val ident : string Angstrom.t
val uns : string Angstrom.t
val cunsint : Ast.const Angstrom.t
val cint : Ast.const Angstrom.t
val cbool : Ast.const Angstrom.t
val cstring : Ast.const Angstrom.t
val const : Ast.const Angstrom.t
val uns_const : Ast.const Angstrom.t
val ptvar : Ast.pt Angstrom.t
val ptwild : Ast.pt Angstrom.t
val ptconst : Ast.pt Angstrom.t

type pdispatch =
  { tuple : pdispatch -> Ast.pt Angstrom.t
  ; other : pdispatch -> Ast.pt Angstrom.t
  ; pt : pdispatch -> Ast.pt Angstrom.t
  }

val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val pack : pdispatch
val pt : Ast.pt Angstrom.t

type edispatch =
  { key : edispatch -> Ast.exp Angstrom.t
  ; tuple : edispatch -> Ast.exp Angstrom.t
  ; exp : edispatch -> Ast.exp Angstrom.t
  ; op : edispatch -> Ast.exp Angstrom.t
  }

val chainl1'
  :  'a Angstrom.t
  -> 'b Angstrom.t
  -> ('a -> 'b -> 'a) Angstrom.t
  -> 'a Angstrom.t

val pack : edispatch
val decl : Ast.decl Angstrom.t
val pprog : Ast.decl list -> Ast.prog
val prog : Ast.prog Angstrom.t
