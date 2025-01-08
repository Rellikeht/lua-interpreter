{
open Parser
open Lexing
open Token
}

let dig = ['0'-'9']
let sign = ['-' '+']

rule token = parse
  | [' ' '\t' '\n' 'r'] { token lexbuf } (* Skip whitespace *)
  | '-''-'_* { token lexbuf }

  | sign? dig+(['.']dig*)?(['e' 'E']sign?dig+)? as n
    { NUMBER (float_of_string n) }
  | '"' [^'"']* '"' as s { STRING (String.sub s 1 (String.length s - 2)) }

  | "break" { BREAK }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "elseif" { ELSEIF }
  | "end" { END }
  | "while" { WHILE }
  | "do" { DO }
  | "repeat" { REPEAT }
  | "until" { UNTIL }
  | "for" { FOR }
  | "in" { IN }
  | "function" { FUNCTION }
  | "local" { LOCAL }
  | "return" { RETURN }

  | "true" { TRUE }
  | "false" { FALSE }
  | "nil" { NIL }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULTIPLY }
  | '/' { DIVIDE }
  | '%' { MODULO }
  | '^' { POWER }

  | "<" { LESS }
  | "<=" { LESSEQUAL }
  | ">" { GREATER }
  | ">=" { GREATEREQUAL }
  | "==" { EQUAL }
  | "~=" { NOTEQUAL }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LSQARE }
  | ']' { RSQUARE }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | ':' { COLON }

  | '#' { LENGTH }
  | '=' { ASSIGN }
  | "..." { VARARG }
  | ".." { CONCAT }
  | '.' { DOT }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name
    { NAME name }

  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
