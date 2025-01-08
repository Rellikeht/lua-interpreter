open Ast
open State

let exec_statements
  (s : state) (**)
  (ss : statement list)
  (e : last_statement option) =
  ()

let exec_chunk (s : state) (c : chunk) = function
  | Statements ss -> exec_statements s ss None
  | Ended (ss, e) -> exec_statements s ss (Some e)

let execute = exec_chunk (initial_state ())
