open Ast
open State

let add_function (s : state) (n : funcname) (b : parameter_list * chunk) = ()

let rec call_func (s : state) (funcall : function_call) = ()

and exec_last (s : state) = function
  (* *)
  | Break -> ()
  | Return _ -> ()

and exec_statement (state : state) =
 (function
 | Assignment (names, vals) -> ()
 | Call funcall -> call_func state funcall
 (* | Do block -> ( *)
 (*     (1* *1) *)
 (*     match block with *)
 (*     | Statements statements -> exec_chunk state statements None *)
 (*     | Ended (statements, last) -> exec_chunk state statements (Some last)) *)
 (* | While of exp * block *)
 (* | Repeat of block * exp *)
 (* | If of exp * block * elseif list * block option *)
 (* | For of name * exp * exp * exp option * block *)
 (* | ForIn of name list * exp list * block *)
 | Function (name, body) -> add_function state name body
 (* | LocalFunction of name * funcbody *)
 (* | Local of name list * exp list *)
 | _ -> ())

and exec_statements (state : state) (ss : statement list)
    (last : last_statement option) =
  match ss with
  | statement :: rest ->
      exec_statement state statement;
      exec_statements state rest last
  (* | [] -> begin match last with *)
  (*   | None -> () *)
  (*   | Some last -> exec_last state last *)
  (* end *)
  | [] -> Option.bind last (fun x -> Some (exec_last state x))

and exec_chunk (state : state) = function
  | Statements ss -> exec_statements state ss None
  | Ended (ss, e) -> exec_statements state ss (Some e)

and exec_block s c = exec_chunk s c

let execute = exec_chunk (initial_state ())
