open Ast
open Values
open State
open Base

let rec parse_table (state : state) (fields : field list) : table =
  let initial : table = Hashtbl.create (module Value) in
  match fields with
  (* *)
  | [] -> initial
  | _ -> raise Unimplemented

and update_locals
    (state : state)
    (names : name list)
    (exps : exp list) =
  let rec update_names names exps =
    begin
      match names with
      | [] ->
          List.iter exps ~f:(fun e ->
              let _ = exec_exp state e in
              ())
      | name :: names_left ->
          let v, exps_left =
            match exps with
            | [] -> (Value Nil, [])
            | e :: exps_left -> (exec_exp state e, exps_left)
          in
          set_local state name v;
          update_names names_left exps_left
    end
  in
  update_names names exps

and update_globals
    (state : state)
    (names : name list)
    (exps : exp list) =
  raise Unimplemented
(*   match state.locals |> List.hd with *)
(*   | None -> () *)
(*   | Some locals -> begin *)
(*       match exps with *)
(*       | [] -> () *)
(*       | exp :: exps -> begin *)
(*           match names with *)
(*           | [] -> *)
(*               List.iter exps ~f:(fun e -> *)
(*                   let _ = exec_exp state e in *)
(*                   ()) *)
(*           | name :: rest -> *)
(*           let value = exec_exp state in *)
(*           Hashtbl. *)
(*         end *)
(*     end *)

and update_vars (state : state) (vars : var list) (vals : exp list) =
  raise Unimplemented

and get_var (state : state) (var : var) =
  match var with
  | Named name -> begin
      match get_value state name with
      | None -> Value Nil
      | Some other -> other
    end
  | Index (name, exp) -> raise Unimplemented
  | Prefix (n1, n2) -> raise Unimplemented

and exec_exp (state : state) (e : exp) =
  match e with
  | Nil | Vararg -> Value Nil
  | False -> Value False
  | True -> Value True
  | Number n -> Value (Number n)
  | String s -> Value (String s)
  | Table t -> Value (Table (parse_table state t))
  | BinaryOp (op, e1, e2) ->
      exec_binop op (exec_exp state e1) (exec_exp state e2)
  | UnaryOp (op, ex) -> exec_unop op (exec_exp state ex)
  (* | Func f -> call_func state f *)
  | Func f -> raise Unimplemented
  | Prefixexp p -> begin
      match p with
      | Var name -> get_var state name
      | Call f -> raise Unimplemented
      | Exp e -> exec_exp state e
    end

and call_builtin
    (func : builtin_func)
    (state : state)
    (args : exp list) =
  let args = List.map ~f:(exec_exp state) args in
  func state args

and call_func (state : state) (funcall : function_call) =
  match funcall with
  | Function (var, args) -> begin
      match get_var state var with
      | Value Nil -> raise Nil_Call
      | Value v -> raise (Value_call v)
      | Builtin f -> call_builtin f state args
      | Function f -> raise Unimplemented
    end
  | Method _ -> raise Unimplemented

and exec_last (state : state) = function
  | Break -> state.breaking <- true (* TODO is that it ? *)
  | Return _ -> state.returning <- true

and exec_statement (state : state) (stmt : statement) =
  match stmt with
  | Assignment (vars, vals) -> update_vars state vars vals
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

and exec_statements
    (state : state)
    (stmts : statement list)
    (last : last_statement option) =
  match stmts with
  | statement :: rest ->
      exec_statement state statement;
      exec_statements state rest last
  | [] -> begin
      match last with None -> () | Some last -> exec_last state last
    end

and exec_chunk (state : state) = function
  | Statements stmts -> exec_statements state stmts None
  | Ended (stmts, last) -> exec_statements state stmts (Some last)

and exec_block s c = exec_chunk s c

let execute = exec_chunk (initial_state ())
