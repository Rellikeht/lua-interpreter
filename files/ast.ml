type binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Power
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | Equal
  | NotEqual
  | And
  | Or

type unary_op = Not | Negate

type var =
  | VarName of string
  | VarIndex of prefixexp * exp
  | VarField of prefixexp * string

and exp =
  | Nil
  | Boolean of bool
  | Number of float
  | String of string
  | Vararg
  | Variable of var
  | PrefixExp of prefixexp
  | BinaryOp of binary_op * exp * exp
  | UnaryOp of unary_op * exp
  | FunctionExpression of function_body
  | FunctionCall of prefixexp * exp list
  | MethodCall of prefixexp * string * exp list
  | TableConstructor of field list
  | Parenthesized of exp

and prefixexp =
  | Variable of var
  | FunctionCall of prefixexp * exp list
  | MethodCall of prefixexp * string * exp list
  | Parenthesized of exp

and field =
  | KeyValueField of exp * exp
  | NamedField of string * exp
  | ImplicitIndexField of exp

and function_body = { params : string list; body : block }

and func_name =
  | SimpleName of string
  | NestedName of string * func_name
  | MethodName of string * string

and statement =
  | Assignment of var list * exp list
  | FunctionCallStatement of prefixexp
  | DoBlock of block
  | WhileLoop of exp * block
  | IfStatement of exp * block * elseif list * block option
  | Elseif of exp * block
  | NumericFor of string * exp * exp * exp option * block
  | GenericFor of string list * exp list * block
  | FunctionDeclaration of func_name * function_body
  | LocalAssignment of string list * exp list
  | LocalFunctionDeclaration of string * function_body
  | Return of exp list option

and block = Block of statement list | Empty
and elseif = Elseif of exp * block

type program = block
