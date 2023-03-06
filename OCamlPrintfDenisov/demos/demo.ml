open Format
open Ocaml_printf_lib
module Interpreter = Interpreter.InterpreterResult

let () =
  match Parser.parse_program Stdio.In_channel.(input_all stdin) with
  | Error err -> printf "Error: %s\n%!" err
  | Ok p ->
    (match Inferencer.infer_program p with
     | Error err -> printf "Error: %a\n%!" Inferencer.pp_error err
     | Ok p ->
       (match Interpreter.run p with
        | Error err -> printf "Error: %a\n%!" Interpreter.pp_r_err err
        | Ok ok -> printf "%a\n%!" Interpreter.pp_r_ok ok))
;;
