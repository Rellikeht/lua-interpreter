(* Number = "[+-]?[0-9]+([.][0-9]*)?([eE][+-]?[0-9]+)?" *)
type number_value = float [@@deriving show]

(* Name = "[_a-zA-Z][_a-zA-Z0-9]*" *)
type name = string [@@deriving show]

(* funcname = Name {"." Name} [":" Name] *)
type funcname = name * name list * name option [@@deriving show]

(* Those are just lists, so don't need separate type *)
(* tableconstructor = "{" [fieldlist] "}" *)
(* varlist = var {"," var} *)
(* namelist = Name {"," Name} *)
(* explist = {exp ","} exp *)

(* parlist = namelist ["," "..."] | "..." *)
type parameter_list =
  (* *)
  | List of name list
  | VarargList of name list
  | Varparam
[@@deriving show]

(* unop = "-" | "not" | "#" *)
type unary_op = Negate | Not | Length [@@deriving show]

(* binop = "+" | "-" | "*" | "/" | "^" | "%" | ".." | *)
(*      "<" | "<=" | ">" | ">=" | "==" | "~=" | *)
(*      "and" | "or" *)
type binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Power
  | Modulo
  | Concat
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NotEqual
  | And
  | Or
[@@deriving show]

(* fieldsep = "," | ";" *)
type field_sep = Comma | Semicolon [@@deriving show]

(* chunk = {stat [";"]} [laststat [";"]] *)
type chunk =
  (* *)
  | Statements of statement list
  | Ended of statement list * last_statement
[@@deriving show]

(* block = chunk *)
and block = chunk [@@deriving show]

and field =
  (* field = "[" exp "]" "=" exp | *)
  | Indexed of exp * exp
  (* Name "=" exp | *)
  | Named of name * exp
  (* exp *)
  | Free of exp
[@@deriving show]

(* funcbody = "(" [parlist] ")" block "end" *)
and funcbody = parameter_list * block [@@deriving show]
and args = exp list

(* functioncall =  prefixexp args | prefixexp ":" Name args *)
and function_call = Function of var * args | Method of var * name * args
[@@deriving show]

and prefixexp =
  (* prefixexp = var | *)
  | Var of var
  (* functioncall | *)
  | Call of function_call
  (* "(" exp ")" *)
  | Exp of exp
[@@deriving show]

and exp =
  (* exp = "nil" | *)
  | Nil
  (*  "false" | *)
  | False
  (*  "true" | *)
  | True
  (*  Number | *)
  | Number of number_value
  (*  String | *)
  | String of string
  (*  "..." | *)
  | Vararg
  (*  function | *)
  | Func of funcbody
  (*  prefixexp | *)
  | Prefixexp of prefixexp
  (*  tableconstructor | *)
  | Table of field list
  (*  exp binop exp | *)
  | BinaryOp of binary_op * exp * exp
  (*  unop exp *)
  | UnaryOp of unary_op * exp
[@@deriving show]

(* var = *)
and var =
  (* Name | *)
  | Named of name
  (* prefixexp "[" exp "]" | *)
  | Index of name * exp
  (* prefixexp "." Name *)
  | Prefix of name * name
[@@deriving show]

(* laststat = "return" [explist] | "break" *)
and last_statement = Return of exp list | Break [@@deriving show]

and statement =
  (* stat =  varlist "=" explist | *)
  | Assignment of var list * exp list
  (* functioncall | *)
  | Call of function_call
  (* "do" block "end" | *)
  | Do of block
  (* "while" exp "do" block "end" | *)
  | While of exp * block
  (* "repeat" block "until" exp | *)
  | Repeat of block * exp
  (* "if" exp "then" block {"elseif" exp "then" block} ["else" block] "end" | *)
  | If of exp * block * elseif list * block option
  (* "for" Name "=" exp "," exp ["," exp] "do" block "end" | *)
  | For of name * exp * exp * exp option * block
  (* "for" namelist "in" explist "do" block "end" | *)
  | ForIn of name list * exp list * block
  (* "function" funcname funcbody | *)
  | Function of funcname * funcbody
  (* "local" "function" Name funcbody | *)
  | LocalFunction of name * funcbody
  (* "local" namelist ["=" explist] *)
  | Local of name list * exp list
[@@deriving show]

(* "if" exp "then" block {"elseif" exp "then" block} ["else" block] "end" | *)
and elseif = exp * block [@@deriving show]

type program = chunk [@@deriving show]
