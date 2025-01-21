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

(* and update_locals *)
(*     (state : state) *)
(*     (names : name list) *)
(*     (exps : exp list) = *)
(*   let rec update_names names exps = *)
(*     begin *)
(*       match names with *)
(*       | [] -> List.map exps ~f:(exec_exp state) |> drop *)
(*       | name :: names_left -> *)
(*           let v, exps_left = *)
(*             match exps with *)
(*             | [] -> (Value Nil, []) *)
(*             | e :: exps_left -> (exec_exp state e, exps_left) *)
(*           in *)
(*           set_local state name v; *)
(*           update_names names_left exps_left *)
(*     end *)
(*   in *)
(*   update_names names exps *)

(* and update_globals *)
(*     (state : state) *)
(*     (names : name list) *)
(*     (exps : exp list) = *)
(*   match names with *)
(*   | [] -> List.map exps ~f:(exec_exp state) |> drop *)
(*   | name :: names_left -> begin *)
(*       match exps with *)
(*       | [] -> () *)
(*       | exp :: exps_left -> begin *)
(*           exec_exp state exp |> set_global state name; *)
(*           update_globals state names_left exps_left *)
(*         end *)
(*     end *)

and update_vars (state : state) (vars : var list) (vals : value list)
    : unit =
  match vars with
  | [] -> ()
  | var :: vars_left -> begin
      match vals with
      | [] -> ()
      | exp :: exps_left -> begin
          update_var state var exp;
          update_vars state vars_left exps_left
        end
    end

and update_var (state : state) (var : var) (value : value) : unit =
  match var with
  | Named name -> set_value state name value
  | Index (name, exp) -> raise Unimplemented
  | Prefix (n1, n2) -> raise Unimplemented

and get_var (state : state) (var : var) : value =
  match var with
  | Named name -> begin
      match get_value state name with
      | None -> Value Nil
      | Some other -> other
    end
  | Index (name, exp) -> raise Unimplemented
  | Prefix (n1, n2) -> raise Unimplemented

and exec_exp (state : state) (e : exp) : value =
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
  | Func (params, body) ->
      Function body (* TODO wtf parameters omitting *)
  | Prefixexp p -> begin
      match p with
      | Var name -> get_var state name
      | Call f -> call_func state f
      | Exp e -> exec_exp state e
    end

and call_builtin
    (func : builtin_func)
    (state : state)
    (args : exp list) : value =
  let args = List.map ~f:(exec_exp state) args in
  func state args

and call_func (state : state) (funcall : function_call) : value =
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

and exec_ifs (state : state) (clauses : elseif list) : bool =
  let rec exec_clauses = function
    | [] -> false
    | (cond, body) :: rest ->
        if exec_exp state cond |> bool_of_val then begin
          exec_chunk state body;
          true
        end
        else
          exec_clauses rest
  in
  exec_clauses clauses

and exec_loop (state : state) (cond : exp) (body : chunk) =
  (* TODO breaking *)
  if exec_exp state cond |> bool_of_val then begin
    exec_chunk state body;
    exec_loop state cond body
  end
  else
    ()

and exec_statement (state : state) (stmt : statement) =
  match stmt with
  | Assignment (vars, exps) ->
      let vals = List.map ~f:(exec_exp state) exps in
      update_vars state vars vals
  | Call funcall ->
      let _ = call_func state funcall in
      ()
  | Do block -> exec_chunk state block
  | While (cond, body) -> exec_loop state cond body
  | Repeat (body, cond) -> begin
      exec_chunk state body;
      exec_loop state cond body
    end
  | If (cond, body, elseifs, elseblock) ->
      if exec_ifs state ((cond, body) :: elseifs) then
        ()
      else begin
        match elseblock with
        | None -> ()
        | Some block -> exec_chunk state block
      end
  | For (name, start, last, step, body) -> raise Unimplemented
  | ForIn (names, exps, body) -> raise Unimplemented
  | Function (name, body) -> add_function state name body
  | LocalFunction (name, funcbody) ->
      raise Unimplemented
      (* update_locals state [ name ] [ Func funcbody ] *)
  | Local (names, exps) -> raise Unimplemented
(* update_locals state names exps *)

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
