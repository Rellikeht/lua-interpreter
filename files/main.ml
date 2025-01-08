open Interpreter
open Interpreter.Ast

(* Pretty printer for AST *)
let rec print_exp indent exp =
  let print_indent = print_string (String.make indent ' ') in
  match exp with
  | Nil ->
      print_indent;
      print_endline "Nil"
  | Boolean b ->
      print_indent;
      Printf.printf "Boolean: %b\n" b
  | Number n ->
      print_indent;
      Printf.printf "Number: %f\n" n
  | String s ->
      print_indent;
      Printf.printf "String: %s\n" s
  | Variable var ->
      print_indent;
      print_endline "Variable:";
      print_var (indent + 2) var
  | BinaryOp (op, left, right) ->
      print_indent;
      Printf.printf "BinaryOp: %s\n"
        (match op with
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
        | Power -> "^"
        | LessThan -> "<"
        | GreaterThan -> ">"
        | LessEqual -> "<="
        | GreaterEqual -> ">="
        | Equal -> "=="
        | NotEqual -> "~="
        | And -> "and"
        | Or -> "or");
      print_exp (indent + 2) left;
      print_exp (indent + 2) right
  | _ ->
      print_indent;
      print_endline "Unhandled expression type"

and print_var indent var =
  let print_indent = print_string (String.make indent ' ') in
  match var with
  | VarName name ->
      print_indent;
      Printf.printf "VarName: %s\n" name
  | _ ->
      print_indent;
      print_endline "Complex variable"

let rec print_statement indent stmt =
  let print_indent = print_string (String.make indent ' ') in
  match stmt with
  | Assignment (vars, exps) ->
      print_indent;
      print_endline "Assignment:";
      List.iter (print_var (indent + 2)) vars;
      List.iter (print_exp (indent + 2)) exps
  | FunctionCallStatement call ->
      print_indent;
      print_endline "Function Call Statement"
  | DoBlock block ->
      print_indent;
      print_endline "Do Block:";
      print_block (indent + 2) block
  | _ ->
      print_indent;
      print_endline "Unhandled statement type"

and print_block indent block =
  let print_indent = print_string (String.make indent ' ') in
  match block with
  | Block stmts ->
      print_indent;
      print_endline "Block:";
      List.iter (print_statement (indent + 2)) stmts
  | Empty ->
      print_indent;
      print_endline "Empty Block"

(* Parse Lua file *)
let parse_lua_file filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in file;
    ast
  with
  | Lexer.SyntaxError msg ->
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
      Printf.fprintf stderr "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

(* Main function *)
let () =
  if Array.length Sys.argv < 2 then (
    Printf.fprintf stderr "Usage: %s <lua_file>\n" Sys.argv.(0);
    exit 1);

  let filename = Sys.argv.(1) in
  try
    let ast = parse_lua_file filename in
    print_endline "Successfully parsed Lua file:";
    print_block 0 ast
  with Sys_error msg ->
    Printf.fprintf stderr "File error: %s\n" msg;
    exit 1
