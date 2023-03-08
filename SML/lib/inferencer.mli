(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : Typing.error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, Typing.error) result
end

type fresh = int

module Type : sig
  type t = Typing.typ

  val occurs_in : 'a -> Typing.typ -> bool
  val unpack_vars : Typing.typ -> (fresh, Base.Int.comparator_witness) Base.Set.t
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> Typing.typ -> t R.t
  val find_exn : fresh -> t -> Typing.typ
  val find : fresh -> t -> Typing.typ option
  val apply : t -> Typing.typ -> Typing.typ
  val unify : Typing.typ -> Typing.typ -> t R.t
  val add : t -> fresh -> Typing.typ -> t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end

module VarSet : sig
  val fold : ('a -> 'b -> 'a R.t) -> 'a R.t -> ('b, 'c) Base.Set.t -> 'a R.t
end

module Scheme : sig
  type t = Typing.scheme

  val occurs_in : 'a -> ('a, 'b) Base.Set.t * Typing.typ -> bool

  val unpack_vars
    :  (fresh, Base.Int.comparator_witness) Base.Set.t * Typing.typ
    -> (fresh, Base.Int.comparator_witness) Base.Set.t

  val apply
    :  Subst.t
    -> (fresh, 'a) Base.Set.t * Typing.typ
    -> (fresh, 'a) Base.Set.t * Typing.typ
end

module TypeEnv : sig
  type t = (string, Typing.scheme, Base.String.comparator_witness) Base.Map.t

  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val unpack_vars : t -> (fresh, Base.Int.comparator_witness) Base.Set.t

  val apply
    :  Subst.t
    -> ('a, (fresh, 'b) Base.Set.t * Typing.typ, 'c) Base.Map.t
    -> ('a, (fresh, 'b) Base.Set.t * Typing.typ, 'c) Base.Map.t

  val find_exn : 'a -> ('a, equal:(string -> string -> bool) -> 'b, 'c) Base.Map.t -> 'b
end

val unify : Typing.typ -> Typing.typ -> Subst.t R.t
val fresh_var : Typing.typ R.t
val fresh_eq_var : Typing.typ R.t
val fresh_val : Typing.typ R.t
val is_restricted : Typing.typ -> bool
val instantiate : Typing.scheme -> Typing.typ R.t
val generalize : TypeEnv.t -> Type.t -> Scheme.t

val lookup_env
  :  string
  -> (string, Typing.scheme, 'a) Base.Map.t
  -> (Subst.t * Typing.typ * (string, Typing.scheme, 'a) Base.Map.t) R.t

val find_identifiers : Ast.pt -> (string, Base.String.comparator_witness) Base.Set.t
val restrict : Typing.typ -> Typing.typ
val check_restrict : Ast.exp -> Typing.typ -> Typing.typ
val get_args : Ast.exp -> Ast.exp -> Ast.exp list
val get_name : Ast.exp -> string R.t
val infer : TypeEnv.t -> Ast.exp -> (Subst.t * Typing.typ * TypeEnv.t) R.t

val check_types
  :  TypeEnv.t
  -> Ast.decl
  -> ((string, Typing.scheme, Base.String.comparator_witness) Base.Map.t * Typing.typ) R.t

val run_inference
  :  Ast.decl
  -> TypeEnv.t
  -> ( (string, Typing.scheme, Base.String.comparator_witness) Base.Map.t * Typing.typ
     , Typing.error )
     result
