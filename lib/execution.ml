open Ast
open Values
open State
open Base

(* for debug *)
open Stdio

let rec parse_table (state : state) (fields : field list) : table =
  let initial : table = Hashtbl.create (module Value) in
  match fields with
  (* *)
  | [] -> initial
  | _ -> raise Unimplemented

and update_vars
    (state : state)
    (vars : var list)
    (values : value list) : unit =
  match vars with
  | [] -> ()
  | var :: vars_left -> begin
      match values with
      | [] -> ()
      | value :: values_left -> begin
          update_var state var value;
          update_vars state vars_left values_left
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
  | BinaryOp (op, e1, e2) -> begin
      let v1 = exec_exp state e1 in
      let v2 = exec_exp state e2 in
      let result = exec_binop op v1 v2 in
      result
    end
  | UnaryOp (op, ex) -> exec_unop op (exec_exp state ex)
  | Func (params, body) -> Function (params, body)
  | Prefixexp p -> begin
      match p with
      | Var name -> get_var state name
      | Call f ->
          if call_func state f then begin
            state.returning <- false;
            let returned = state.returned in
            state.returned <- [];
            match List.hd returned with
            | None -> Value Nil
            | Some v -> v
          end
          else
            Value Nil
      | Exp e -> exec_exp state e
    end

and call_builtin
    (func : builtin_func)
    (state : state)
    (args : exp list) : bool =
  let args = List.map ~f:(exec_exp state) args in
  func state args

and call_func (state : state) (funcall : function_call) : bool =
  match funcall with
  | Function (var, args) -> begin
      match get_var state var with
      | Value Nil -> raise Nil_Call
      | Value v -> raise (Value_call v)
      | Builtin f -> call_builtin f state args
      | Function (params, body) -> begin
          let level = fresh_level () in
          let args = List.map ~f:(exec_exp state) args in
          begin
            match params with
            | List names -> update_level level names args
            | VarargList _ -> raise Unimplemented
            | Varparam -> raise Unimplemented
          end;
          exec_chunk ~level state body;
          let returning = state.returning in
          (* state.returning <- false; *)
          returning
        end
    end
  | Method _ -> raise Unimplemented

and exec_last (state : state) = function
  | Break -> state.breaking <- true (* TODO implement that *)
  | Return values -> begin
      state.returned <- List.map ~f:(exec_exp state) values;
      state.returning <- true
    end

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
  begin
    if exec_exp state cond |> bool_of_val then begin
      exec_chunk state body;
      exec_loop state cond body
    end
    else
      ()
  end

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
  | LocalFunction (name, (params, body)) ->
      let locals = List.hd_exn state.locals in
      update_level locals [ name ] [ Function (params, body) ]
  | Local (names, exps) ->
      let locals = List.hd_exn state.locals in
      let vals = List.map ~f:(exec_exp state) exps in
      update_level locals names vals

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

and exec_chunk
    ?(level = fresh_level ())
    (state : state)
    (statements : chunk) : unit =
  begin
    state.locals <- level :: state.locals;
    begin
      match statements with
      | Statements stmts -> exec_statements state stmts None
      | Ended (stmts, last) -> exec_statements state stmts (Some last)
    end;
    state.locals <- List.tl_exn state.locals
  end

and exec_block s c = exec_chunk s c

let execute = exec_chunk (initial_state ())
