{
open Parser
open Lexing
open Token
}

let dig = ['0'-'9']
let sign = ['-' '+']
let intn = sign? dig+
let name_start = ['a'-'z' 'A'-'Z' '_']
let name_rest = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
  (* | [' ' '\t' '\n' 'r'] { token lexbuf } *)
  | [' ' '\t']+ { token lexbuf }
  | '-''-'[^'\n' '\r']* { token lexbuf }

  | intn(['.']dig*)?(['e' 'E']intn)? as n { NUMBER (float_of_string n) }
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

  | "<=" { LESSEQUAL }
  | "<" { LESS }
  | ">=" { GREATEREQUAL }
  | ">" { GREATER }
  | "~=" { NOTEQUAL }
  | "==" { EQUAL }

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

  | name_start name_rest* as name { NAME name }
  (* | ['\n' 'r'] | ('\n' '\r') | ('\r' '\n') *)
  (*   { Lexing.new_line lexbuf; token lexbuf } *)
  | ['\n' 'r'] | ('\n' '\r') | ('\r' '\n') { token lexbuf }

  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
