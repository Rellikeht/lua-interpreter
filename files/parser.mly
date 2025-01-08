%{
open Ast
%}

(* Token Declarations *)
%token <string> STRING
%token <float> NUMBER
%token <string> NAME
%token PLUS MINUS TIMES DIVIDE MODULO
%token POWER
%token EQUALS NOT_EQUALS LESS_THAN GREATER_THAN 
%token LESS_EQUAL GREATER_EQUAL
%token AND OR NOT
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACE RIGHT_BRACE
%token LEFT_BRACKET RIGHT_BRACKET
%token COMMA SEMICOLON COLON DOT
%token ASSIGN
%token IF ELSE ELSEIF THEN END
%token WHILE DO
%token FOR IN
%token FUNCTION
%token LOCAL
%token RETURN
%token NIL TRUE FALSE
%token VARARG
%token EOF

(* Precedence and Associativity *)
%left OR
%left AND
%left LESS_THAN GREATER_THAN LESS_EQUAL GREATER_EQUAL EQUALS NOT_EQUALS
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT UNARY_MINUS
%right POWER
%left DOT LEFT_BRACKET

(* Start Symbol *)
%start <Ast.program> program

%%

(* Top-level program *)
program:
  | block EOF { $1 }
  ;

(* Block of code *)
block:
  | statement_list { Block $1 }
  ;

(* List of statements *)
statement_list:
  | (* empty *)                { [] }
  | statement statement_list   { $1 :: $2 }
  ;

(* Statements *)
statement:
  | assignment_statement  { $1 }
  | function_call_statement  { $1 }
  | do_block              { $1 }
  | while_statement       { $1 }
  | if_statement          { $1 }
  | for_statement         { $1 }
  | function_declaration  { $1 }
  | local_declaration     { $1 }
  | return_statement      { $1 }
  ;

(* Function Call Statement *)
function_call_statement:
  | function_call { FunctionCallStatement $1 }
  ;

(* Assignment Statement *)
assignment_statement:
  | var_list ASSIGN exp_list 
    { Assignment ($1, $3) }
  ;

(* Variable List *)
var_list:
  | var                      { [$1] }
  | var COMMA var_list       { $1 :: $3 }
  ;

(* Variables *)
var:
  | NAME                     { VarName $1 }
  | prefixexp LEFT_BRACKET exp RIGHT_BRACKET 
    { VarIndex ($1, $3) }
  | prefixexp DOT NAME       { VarField ($1, $3) }
  ;

(* Function Call *)
function_call:
  | prefixexp LEFT_PAREN opt_args RIGHT_PAREN 
    { FunctionCall ($1, $3) }
  | prefixexp COLON NAME LEFT_PAREN opt_args RIGHT_PAREN 
    { MethodCall ($1, $3, $5) }
  ;

(* Optional Arguments *)
opt_args:
  | (* empty *)               { [] }
  | exp_list                  { $1 }
  ;

(* Do Block *)
do_block:
  | DO block END              { DoBlock $2 }
  ;

(* While Statement *)
while_statement:
  | WHILE exp DO block END    { WhileLoop ($2, $4) }
  ;

(* If Statement *)
if_statement:
  | IF exp THEN block elseif_list else_part END 
    { IfStatement ($2, $4, $5, $6) }
  ;

(* Else-if List *)
elseif_list:
  | (* empty *)               { [] }
  | ELSEIF exp THEN block elseif_list 
    { (Elseif ($2, $4)) :: $5 }
  ;

(* Optional Else Part *)
else_part:
  | (* empty *)               { None }
  | ELSE block                { Some $2 }
  ;

(* For Statement *)
for_statement:
  | FOR NAME ASSIGN exp COMMA exp opt_step DO block END 
    { NumericFor ($2, $4, $6, $7, $9) }
  | FOR name_list IN exp_list DO block END 
    { GenericFor ($2, $4, $6) }
  ;

(* Optional Step in Numeric For *)
opt_step:
  | (* empty *)               { None }
  | COMMA exp                 { Some $2 }
  ;

(* Name List *)
name_list:
  | NAME                      { [$1] }
  | NAME COMMA name_list      { $1 :: $3 }
  ;

(* Function Declaration *)
function_declaration:
  | FUNCTION func_name function_body 
    { FunctionDeclaration ($2, $3) }
  ;

(* Local Function or Variable Declaration *)

local_declaration:
  | LOCAL name_list ASSIGN exp_list 
    { LocalAssignment ($2, $4) }
  | LOCAL FUNCTION NAME function_body 
    { LocalFunctionDeclaration ($3, $4) }
  ;

(* Function Name *)
func_name:
  | NAME                      { SimpleName $1 }
  | NAME DOT func_name        { NestedName ($1, $3) }
  | NAME COLON NAME           { MethodName ($1, $3) }
  ;

(* Function Body *)
function_body:
  | LEFT_PAREN opt_param_list RIGHT_PAREN block END 
    { { params = $2; body = $4 } }
  ;

(* Optional Parameter List *)
opt_param_list:
  | (* empty *)               { [] }
  | param_list                { $1 }
  ;

(* Parameter List *)
param_list:
  | NAME                      { [$1] }
  | NAME COMMA param_list     { $1 :: $3 }
  | VARARG                    { ["..."] }
  ;

(* Return Statement *)
return_statement:
  | RETURN                    { Return None }
  | RETURN exp_list           { Return (Some $2) }
  ;

(* Expression List *)
exp_list:
  | exp                       { [$1] }
  | exp COMMA exp_list        { $1 :: $3 }
  ;

(* Expressions *)
exp:
  | NIL                       { Nil }
  | TRUE                      { Boolean true }
  | FALSE                     { Boolean false }
  | NUMBER                    { Number $1 }
  | STRING                    { String $1 }
  | VARARG                    { Vararg }
  | function_exp              { $1 }
  | prefixexp                 { PrefixExp $1 }
  | table_constructor         { $1 }
  
  (* Binary Operators *)
  | exp PLUS exp              { BinaryOp (Add, $1, $3) }
  | exp MINUS exp             { BinaryOp (Subtract, $1, $3) }
  | exp TIMES exp             { BinaryOp (Multiply, $1, $3) }
  | exp DIVIDE exp            { BinaryOp (Divide, $1, $3) }
  | exp MODULO exp            { BinaryOp (Modulo, $1, $3) }
  | exp POWER exp             { BinaryOp (Power, $1, $3) }
  | exp LESS_THAN exp         { BinaryOp (LessThan, $1, $3) }
  | exp GREATER_THAN exp      { BinaryOp (GreaterThan, $1, $3) }
  | exp LESS_EQUAL exp        { BinaryOp (LessEqual, $1, $3) }
  | exp GREATER_EQUAL exp     { BinaryOp (GreaterEqual, $1, $3) }
  | exp EQUALS exp            { BinaryOp (Equal, $1, $3) }
  | exp NOT_EQUALS exp        { BinaryOp (NotEqual, $1, $3) }
  | exp AND exp               { BinaryOp (And, $1, $3) }
  | exp OR exp                { BinaryOp (Or, $1, $3) }
  
  (* Unary Operators *)
  | NOT exp                   { UnaryOp (Not, $2) }
  | MINUS exp %prec UNARY_MINUS 
    { UnaryOp (Negate, $2) }
  ;

(* Prefix Expressions *)
prefixexp:
  | var                       { Variable $1 }
  | function_call             { $1 }
  | LEFT_PAREN exp RIGHT_PAREN { Parenthesized $2 }
  ;

(* Function Expression *)
function_exp:
  | FUNCTION function_body     { FunctionExpression $2 }
  ;

(* Table Constructor *)
table_constructor:
  | LEFT_BRACE opt_field_list RIGHT_BRACE 
    { TableConstructor $2 }
  ;

(* Optional Field List *)
opt_field_list:
  | (* empty *)               { [] }
  | field_list                { $1 }
  ;

(* Field List *)
field_list:
  | field                     { [$1] }
  | field COMMA field_list    { $1 :: $3 }
  ;

(* Field *)
field:
  | LEFT_BRACKET exp RIGHT_BRACKET ASSIGN exp 
    { KeyValueField ($2, $5) }
  | NAME ASSIGN exp            { NamedField ($1, $3) }
  | exp                        { ImplicitIndexField $1 }
  ;


