open Interpreter

let filename = Sys.argv.(1);;

if Array.length Sys.argv < 2 then (
  Printf.fprintf stderr "Usage: %s <lua_file>\n" Sys.argv.(0);
  exit 1)
;;

let file = open_in filename in
let lexbuf = Lexing.from_channel file in
try
  let ast = Parser.program Lexer.token lexbuf in
  close_in file;
  (* print_endline "Successfully parsed Lua file:"; *)
  (* Ast.show_program ast |> print_endline; *)
  (* print_newline (); *)
  Execution.execute ast
with
| Failure msg ->
    close_in file;
    Printf.fprintf stderr "Lexer error: %s\n" msg;
    exit 1
| Parser.Error ->
    close_in file;
    let curr_pos = lexbuf.Lexing.lex_curr_p in
    Printf.fprintf stderr "Parser error at line %d, column %d\n"
      curr_pos.Lexing.pos_lnum
      (curr_pos.Lexing.pos_cnum - curr_pos.Lexing.pos_bol);
    exit 1
| e ->
    close_in file;
    Printf.fprintf stderr "Unexpected error: %s\n"
      (Printexc.to_string e);
    exit 1
