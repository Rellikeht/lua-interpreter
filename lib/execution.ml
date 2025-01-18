open Ast
open State
open Base

exception Unimplemented

let rec parse_table (state : state) (table : field list) = raise Unimplemented

and exec_exp (state : state) (e : exp) =
  match e with
  | Nil | Vararg -> Value Nil
  | False -> Value False
  | True -> Value True
  | Number n -> Value (Number n)
  | String s -> Value (String s)
  | Table t -> Value (Table (parse_table state t))
  (* | Func f -> call_func s f *)
  | _ -> raise Unimplemented

and call_builtin (func : builtin_func) (state : state) (args : exp list) =
  let args = List.map ~f:(exec_exp state) args in
  func state args

and call_func (state : state) (funcall : function_call) =
  match funcall with
  (* *)
  | Function (var, args) -> (
      match var with
      (* *)
      | Named name -> (
          match Hashtbl.find state.symbols name with
          | None -> raise Unimplemented (* TODO raise *)
          | Some (Builtin f) -> call_builtin f state args
          | _ -> ())
      | _ -> ())
  | _ -> ()

and exec_last (state : state) = function
  (* *)
  | Break -> raise Unimplemented
  | Return _ -> raise Unimplemented

and exec_statement (state : state) (stmt : statement) =
  match stmt with
  | Assignment (names, vals) -> raise Unimplemented
  | Call funcall -> call_func state funcall
  | Do block -> exec_chunk state block
  | While (cond, body) -> raise Unimplemented
  | Repeat (body, cond) -> raise Unimplemented
  | If (cond, body, elseifs, elseblock) -> raise Unimplemented
  | For (name, start, last, step, body) -> raise Unimplemented
  | ForIn (names, exps, body) -> raise Unimplemented
  | Function (name, body) -> add_function state name body
  | LocalFunction (name, funcbody) ->
      update_locals state [ name ] [ Func funcbody ]
  | Local (names, exps) -> update_locals state names exps

and exec_statements (state : state) (stmts : statement list)
    (last : last_statement option) =
  match stmts with
  | statement :: rest ->
      exec_statement state statement;
      exec_statements state rest last
  | [] -> (
      match last with
      (* *)
      | None -> ()
      | Some last -> exec_last state last)

and exec_chunk (state : state) = function
  | Statements stmts -> exec_statements state stmts None
  | Ended (stmts, last) -> exec_statements state stmts (Some last)

and exec_block s c = exec_chunk s c

let execute = exec_chunk (initial_state ())
