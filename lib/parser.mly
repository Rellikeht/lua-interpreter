%{
  open Ast
  open Token
%}

%token <float> NUMBER
%token <string> STRING
%token <string> NAME

%token BREAK IF THEN ELSE ELSEIF END WHILE DO REPEAT UNTIL FOR IN FUNCTION LOCAL RETURN
%token TRUE FALSE NIL AND OR NOT
%token PLUS MINUS MULTIPLY DIVIDE MODULO POWER
%token LESS LESSEQUAL GREATER GREATEREQUAL EQUAL NOTEQUAL
%token LPAREN RPAREN LBRACE RBRACE LSQARE RSQUARE
%token COMMA COLON
%token SEMICOLON
%token LENGTH ASSIGN DOT CONCAT VARARG
%token EOF

%left OR
%left AND
%left LESS LESSEQUAL GREATER GREATEREQUAL EQUAL NOTEQUAL
%right CONCAT
%left PLUS MINUS
%left MULTIPLY MODULO DIVIDE
%right NOT LENGTH
%right POWER

%start <Ast.program> program

%%

program:
  | c = chunk EOF { c }
;

(* can be inline *)
chunk:
  | s = list(terminated(stat, option(SEMICOLON))) { Statements s }
  | s = list(terminated(stat, option(SEMICOLON)))
    l = laststat option(SEMICOLON) { Ended (s, l) }
;

(* can be inline *)
block:
  | b = chunk { b }
;

(* can be inline *)
laststat:
  | RETURN e = separated_list(COMMA, exp) { Return e }
  | BREAK { Break }
;

stat:
  | v = separated_nonempty_list(COMMA, var) ASSIGN
    e = separated_nonempty_list(COMMA, exp) { Assignment (v, e) }
  | f = functioncall { Call f }
  | DO b = block END { Do b }
  | WHILE e = exp DO b = block END { While (e, b) }
  | REPEAT b = block UNTIL e = exp { Repeat (b, e) }
  | IF e = exp THEN b = block el = list(elseif_block) els = option(else_block) END
    { If (e, b, el, els) }
  | FOR n = NAME ASSIGN e1 = exp COMMA e2 = exp e3 = option(preceded(COMMA, exp)) DO b = block END
    { For (n, e1, e2, e3, b) }
  | FOR nl = separated_nonempty_list(COMMA, NAME)
    IN el = separated_nonempty_list(COMMA, exp)
    DO b = block END
    { ForIn (nl, el, b) }
  | FUNCTION fn = funcname fb = funcbody { Function (fn, fb) }
  | LOCAL FUNCTION n = NAME fb = funcbody { LocalFunction (n, fb) }
  | LOCAL nl = separated_nonempty_list(COMMA, NAME) ASSIGN el = separated_nonempty_list(COMMA, exp)
    { Local (nl, el) }
;

(* can be inline *)
elseif_block:
  | ELSEIF e = exp THEN b = block { (e, b) }
;

(* can be inline *)
else_block:
  | ELSE b = block { b }
;

(* can be inline *)
funcname:
  | n = NAME nl = list(preceded(DOT, NAME)) mn = option(preceded(COLON, NAME))
    { (n, nl, mn) }
;

var:
  | n = NAME { Named n }
  | n = NAME LSQARE e = exp RSQUARE { Index (n, e) }
  | n1 = NAME DOT n2 = NAME { Prefix (n1, n2) }
;

exp:
  | NIL { Nil }
  | FALSE { False }
  | TRUE { True }
  | n = NUMBER { Number n }
  | s = STRING { String s }
  | VARARG { Vararg }
  | FUNCTION fb = funcbody { Func fb }
  | p = prefixexp { Prefixexp p }
  | t = tableconstructor { Table t }
  | e1 = exp op = binop e2 = exp { BinaryOp (op, e1, e2) }
  | op = unop e = exp { UnaryOp (op, e) }
;

prefixexp:
  | v = var { Var v }
  | f = functioncall { Call f }
  | LPAREN e = exp RPAREN { Exp e }
;

functioncall:
  | p = var a = args { Function (p, a) }
  | p = var COLON n = NAME a = args { Method (p, n, a) }
;

args:
  | LPAREN el = separated_list(COMMA, exp) RPAREN { el }
  | t = tableconstructor { [Table t] }
  | s = STRING { [String s] }
;

(* can be inline *)
funcbody:
  | LPAREN pl = parlist RPAREN b = block END { (pl, b) }
;

(* can be inline *)
parlist:
  | nl = nonempty_list(terminated(NAME, COMMA)) VARARG { VarargList nl }
  | nl = separated_list(COMMA, NAME) { List nl }
  | VARARG { Varparam }
;

(* can be inline *)
tableconstructor:
  | LBRACE fl = separated_list(fieldsep, field) RBRACE { fl }
;

field:
  | LSQARE e1 = exp RSQUARE ASSIGN e2 = exp { Indexed (e1, e2) }
  | n = NAME ASSIGN e = exp { Named (n, e) }
  | e = exp { Free e }
;

(* can be inline *)
fieldsep:
  | COMMA { Comma }
  | SEMICOLON { Semicolon }
;

%inline binop:
  | PLUS { Add }
  | MINUS { Subtract }
  | MULTIPLY { Multiply }
  | DIVIDE { Divide }
  | POWER { Power }
  | MODULO { Power }
  | CONCAT { Concat }
  | LESS { Less }
  | LESSEQUAL { LessEqual }
  | GREATER { Greater }
  | GREATEREQUAL { GreaterEqual }
  | EQUAL { Equal }
  | NOTEQUAL { NotEqual }
  | AND { And }
  | OR { Or }
;

%inline unop:
  | MINUS { Negate }
  | NOT { Not }
  | LENGTH { Length }
;
