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

chunk:
  | s = list(terminated(stat, option(SEMICOLON))) { Statements s }
  | s = list(terminated(stat, option(SEMICOLON)))
    l = laststat option(SEMICOLON) { Ended (s, l) }
;

%inline block:
  | b = chunk { b }
;

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
  | FOR n = NAME ASSIGN e1 = exp COMMA e2 = exp COMMA e3 = option(exp) DO b = block END
    { For (n, e1, e2, e3, b) }
  | FOR nl = separated_nonempty_list(COMMA, NAME) IN el = separated_nonempty_list(COMMA, exp) DO b = block END
    { ForIn (nl, el, b) }
  | FUNCTION fn = funcname fb = funcbody { Function (fn, fb) }
  | LOCAL FUNCTION n = NAME fb = funcbody { LocalFunction (n, fb) }
  | LOCAL nl = separated_nonempty_list(COMMA, NAME) ASSIGN el = separated_nonempty_list(COMMA, exp)
    { Local (nl, el) }
;

elseif_block:
  | ELSEIF e = exp THEN b = block { (e, b) }
;

else_block:
  | ELSE b = block { b }
;

%inline funcname:
  | n = NAME nl = list(preceded(DOT, NAME)) mn = option(preceded(COLON, NAME))
    { (n, nl, mn) }
;

var:
  | n = NAME { Named n }
  | p = prefixexp LSQARE e = exp RSQUARE { Index (p, e) }
  | p = prefixexp DOT n = NAME { Prefix (p, n) }
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
  | p = prefixexp a = args { Function (p, a) }
  | p = prefixexp COLON n = NAME a = args { Method (p, n, a) }
;

%inline args:
  | LPAREN el = separated_list(COMMA, exp) option(COMMA) RPAREN { el }
  | t = tableconstructor { [Table t] }
  | s = STRING { [String s] }
;

(* args: *)
(*   | LPAREN el = separated_list(COMMA, exp) option(COMMA) RPAREN { Args el } *)
(*   | t = tableconstructor { Table t } *)
(*   | s = STRING { String s } *)
(* ; *)

%inline funcbody:
  | LPAREN pl = parlist RPAREN b = block END { (pl, b) }
;

%inline parlist:
  | nl = separated_nonempty_list(COMMA, NAME) COMMA VARARG { VarargList nl }
  | nl = separated_list(COMMA, NAME) option(COMMA) { List nl }
  | VARARG { Varparam }
;

%inline tableconstructor:
  | LBRACE fl = separated_list(fieldsep, field) option(fieldsep) RBRACE { fl }
;

field:
  | LSQARE e1 = exp RSQUARE ASSIGN e2 = exp { Indexed (e1, e2) }
  | n = NAME ASSIGN e = exp { Named (n, e) }
  | e = exp { Free e }
;

%inline fieldsep:
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
