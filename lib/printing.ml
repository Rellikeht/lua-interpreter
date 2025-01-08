let debug_token lexbuf =
  let token = Lexer.token lexbuf in
  token |> Token.show_token |> print_endline;
  token

let dump_tree ?(tokens = true) filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  try
    let ast =
      if tokens then Parser.program debug_token lexbuf
      else Parser.program Lexer.token lexbuf
    in
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
