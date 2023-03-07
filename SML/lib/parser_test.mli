(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val test_parse : label:string -> code:string -> expected:Ast.prog -> bool
