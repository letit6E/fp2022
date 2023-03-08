(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module EnvrMap : sig
  type key = string
  type 'a t = 'a Map.Make(String).t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

exception Not_bound

val lookup : EnvrMap.key -> 'a EnvrMap.t -> 'a
val empty_env : 'a EnvrMap.t

module type MONAD_FAIL = sig
  type ('a, 'e) t

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'b) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'b) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end

  module Monad_infix : sig
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type exval =
  | TupleVal of exval list
  | ListVal of exval list
  | SomeVal of exval
  | NoneVal
  | FunVal of Ast.pt * Ast.exp * env lazy_t * scan_info
  | IntVal of int
  | StringVal of string
  | BoolVal of bool
  | InternalVal

and error =
  | Match_fail of Ast.pt * exval
  | Match_exhaust of Ast.exp
  | Wrong_arg_pat of Ast.pt
  | Wrong_arg of Ast.exp
  | Wrong_un_op of Ast.un_op * exval
  | Wrong_bin_op of Ast.bin_op * exval * exval
  | Not_bool_condition of Ast.exp
  | Not_bound of string
  | Not_function of Ast.exp
  | Let_rec_only_vars of Ast.pt
  | Division_by_zero

and env = exval EnvrMap.t
and scan_info = Info of env * env * env * string list

val pp_exval : Format.formatter -> exval -> unit
val show_exval : exval -> string
val pp_error : Format.formatter -> error -> unit
val show_error : error -> string
val pp_env : Format.formatter -> env -> unit
val show_env : env -> string
val pp_scan_info : Format.formatter -> scan_info -> unit
val show_scan_info : scan_info -> string

module Interpret : functor (M : MONAD_FAIL) -> sig
  val pp_value : string * exval -> string
  val to_string : exval -> string
  val pp_error : error -> unit
  val exd_env : string -> 'a -> 'a EnvrMap.t -> 'a EnvrMap.t
  val extend_state : 'a EnvrMap.t -> (string * 'a) list -> 'a EnvrMap.t
  val lookup_env : string -> 'a EnvrMap.t -> ('a, error) M.t
  val fold : 'a list -> f:('b -> 'a -> ('b, 'c) M.t) -> init:'b -> ('b, 'c) M.t
  val pt_match : Ast.pt -> exval -> ((string * exval * string) list, error) M.t
  val apply_bin_op : Ast.bin_op -> exval -> exval -> (exval, error) M.t
  val apply_un_op : Ast.un_op -> exval -> (exval, error) M.t

  val efun
    :  exval EnvrMap.t
    -> env
    -> env
    -> exval EnvrMap.t
    -> string list
    -> Ast.exp
    -> (scan_info, error) M.t

  val scan_app
    :  exval EnvrMap.t
    -> Ast.exp
    -> (scan_info * exval EnvrMap.t * Ast.exp, error) M.t

  val eval_expr : exval EnvrMap.t -> Ast.exp -> (exval, error) M.t

  val eval_bind
    :  exval EnvrMap.t
    -> Ast.binding
    -> ((string * exval * string) list * exval EnvrMap.t, error) M.t

  val eval_dec
    :  exval EnvrMap.t
    -> Ast.decl
    -> ((string * exval * string) list * exval EnvrMap.t, error) M.t

  val eval_prog
    :  Ast.decl list
    -> ( (string * exval * string) list
         * ( (string, Typing.scheme, Base.String.comparator_witness) Base.Map.t
             * Typing.typ
           , Typing.error )
           result
           list
       , error )
       M.t
end

module InterpreterResult : sig
  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax = Base.Result.Let_syntax
  module Monad_infix = Base.Result.Monad_infix

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t

  module Error = Base.Result.Error

  val invariant
    :  'a Base__Invariant_intf.inv
    -> 'b Base__Invariant_intf.inv
    -> ('a, 'b) t Base__Invariant_intf.inv

  val fail : 'err -> ('a, 'err) t
  val failf : ('a, unit, string, ('b, string) t) format4 -> 'a
  val is_ok : ('a, 'b) t -> bool
  val is_error : ('a, 'b) t -> bool
  val ok : ('ok, 'a) t -> 'ok option
  val ok_exn : ('ok, exn) t -> 'ok
  val ok_or_failwith : ('ok, string) t -> 'ok
  val error : ('a, 'err) t -> 'err option
  val of_option : 'ok option -> error:'err -> ('ok, 'err) t
  val iter : ('ok, 'a) t -> f:('ok -> unit) -> unit
  val iter_error : ('a, 'err) t -> f:('err -> unit) -> unit
  val map : ('ok, 'err) t -> f:('ok -> 'c) -> ('c, 'err) t
  val map_error : ('ok, 'err) t -> f:('err -> 'c) -> ('ok, 'c) t

  val combine
    :  ('ok1, 'err) t
    -> ('ok2, 'err) t
    -> ok:('ok1 -> 'ok2 -> 'ok3)
    -> err:('err -> 'err -> 'err)
    -> ('ok3, 'err) t

  val combine_errors : ('ok, 'err) t list -> ('ok list, 'err list) t
  val combine_errors_unit : (unit, 'err) t list -> (unit, 'err list) t
  val to_either : ('ok, 'err) t -> ('ok, 'err) Base__Either0.t
  val of_either : ('ok, 'err) Base__Either0.t -> ('ok, 'err) t
  val ok_fst : ('ok, 'err) t -> ('ok, 'err) Base__Either0.t
  val ok_if_true : bool -> error:'err -> (unit, 'err) t
  val try_with : (unit -> 'a) -> ('a, exn) t

  module Export = Base.Result.Export

  val run : ('a, 'b) t -> ok:('a -> 'c) -> err:('b -> 'c) -> 'c
  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end

val eval_pp : 'a -> string -> unit
