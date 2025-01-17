open Ast
open State
open Base

let add_function (s : state) (n : funcname) (b : parameter_list * chunk) = ()
let update_locals (s : state) (ns : name list) (es : exp list) = ()

let rec exec_exp (e : exp) =
  match e with
  | Nil -> Value Nil
  | False -> Value False
  | True -> Value True
  | Number n -> Value (Number n)
  | Vararg -> raise (Invalid_argument "Vararg")
  | _ -> Value Nil

and call_builtin (f : builtin_func) (s : state) (a : exp list) =
  let args = List.map ~f:exec_exp a in
  f s args

and call_func (s : state) (funcall : function_call) =
  match funcall with
  (* *)
  | Function (prefix, args) -> (
      match prefix with
      (* | Var var -> ( *)
      (*     match var with *)
      (*     (1* *1) *)
      (*     | Named name -> ( *)
      (*         match Hashtbl.find s.symbols name with *)
      (*         | None -> () (1* TODO raise *1) *)
      (*         | Some (Builtin f) -> call_builtin f s args *)
      (*         | _ -> ()) *)
      (*     | _ -> ()) *)
      | _ -> ())
  | _ -> ()

and exec_last (s : state) = function
  (* *)
  | Break -> ()
  | Return _ -> ()

and exec_statement (state : state) =
 (function
 | Assignment (names, vals) -> ()
 (* | Call funcall -> call_func state funcall *)
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
 (* | LocalFunction (name, funcbody) -> *)
 (*     update_locals state [ name ] [ Func funcbody ] *)
 (* | Local (names, exps) -> update_locals state names exps *)
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
  | [] -> Option.bind last ~f:(fun x -> Some (exec_last state x))

and exec_chunk (state : state) = function
  | Statements ss -> exec_statements state ss None
  | Ended (ss, e) -> exec_statements state ss (Some e)

and exec_block s c = exec_chunk s c

let execute = exec_chunk (initial_state ())
