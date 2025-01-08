(* let string_of_token = function *)
(*   | BREAK -> "BREAK" *)
(*   | IF -> "IF" *)
(*   | WHILE -> "WHILE" *)
(*   | VARARG -> "VARARG" *)
(*   | UNTIL -> "UNTIL" *)
(*   (1* | UMINUS -> "UMINUS" *1) *)
(*   | TRUE -> "TRUE" *)
(*   | THEN -> "THEN" *)
(*   | SEMICOLON -> "SEMICOLON" *)
(*   | RSQUARE -> "RSQUARE" *)
(*   | RPAREN -> "RPAREN" *)
(*   | RETURN -> "RETURN" *)
(*   | REPEAT -> "REPEAT" *)
(*   | RBRACE -> "RBRACE" *)
(*   | POWER -> "POWER" *)
(*   | PLUS -> "PLUS" *)
(*   | OR -> "OR" *)
(*   | NOTEQUAL -> "NOTEQUAL" *)
(*   | NOT -> "NOT" *)
(*   | NIL -> "NIL" *)
(*   | MULTIPLY -> "MULTIPLY" *)
(*   | MODULO -> "MODULO" *)
(*   | MINUS -> "MINUS" *)
(*   | LSQARE -> "LSQARE" *)
(*   | LPAREN -> "LPAREN" *)
(*   | LOCAL -> "LOCAL" *)
(*   | LESSEQUAL -> "LESSEQUAL" *)
(*   | LESS -> "LESS" *)
(*   | LENGTH -> "LENGTH" *)
(*   | LBRACE -> "LBRACE" *)
(*   | IN -> "IN" *)
(*   | GREATEREQUAL -> "GREATEREQUAL" *)
(*   | GREATER -> "GREATER" *)
(*   | FUNCTION -> "FUNCTION" *)
(*   | FOR -> "FOR" *)
(*   | FALSE -> "FALSE" *)
(*   | EQUAL -> "EQUAL" *)
(*   | EOF -> "EOF" *)
(*   | END -> "END" *)
(*   | ELSEIF -> "ELSEIF" *)
(*   | ELSE -> "ELSE" *)
(*   | DOT -> "DOT" *)
(*   | DO -> "DO" *)
(*   | DIVIDE -> "DIVIDE" *)
(*   | CONCAT -> "CONCAT" *)
(*   | COMMA -> "COMMA" *)
(*   | COLON -> "COLON" *)
(*   | ASSIGN -> "ASSIGN" *)
(*   | AND -> "AND" *)
(*   | STRING s -> "(STRING " ^ s ^ ")" *)
(*   | NUMBER n -> "(NUMBER " ^ string_of_float n ^ ")" *)
(*   | NAME n -> "(NAME " ^ n ^ ")" *)

let debug_token lexbuf =
  let token = Lexer.token lexbuf in
  (* token |> string_of_token |> print_endline; *)
  token |> Token.show_token |> print_endline;
  token

let dump_tree filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  try
    (* let ast = Parser.program Printing.debug_token lexbuf in *)
    let ast = Parser.program Lexer.token lexbuf in
    close_in file;
    print_endline "Successfully parsed Lua file:";
    Ast.show_program ast |> print_endline
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
      Printf.fprintf stderr "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
