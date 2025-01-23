open Ast
open Base
open Stdio

type simple_value =
  | Nil
  | True
  | False
  | Number of number_value
  | String of string
  | Table of table

and builtin_func = state -> value list -> bool

and value =
  | Value of simple_value
  | Function of parameter_list * chunk
  | Builtin of builtin_func

and table = (value, value) Hashtbl.t
and symbols = (name, value) Hashtbl.t

and state = {
  (* *)
  (* mutable line : int; *)
  globals : symbols;
  mutable locals : symbols list;
  mutable breaking : bool;
  mutable returning : bool;
  mutable returned : value list;
}

module Value = struct
  type t = value

  let sexp_of_t (v : t) = Sexp.List []

  (* TODO this may not even work *)
  let compare v1 v2 = 0

  (* TODO this will be slow as fuck *)
  let hash v = 0
end

type numop = number_value -> number_value -> bool
type strop = string -> string -> bool

exception Unimplemented
exception Nil_Call
exception Nil_Index
exception Value_call of simple_value
exception Invalid_value
exception Unreachable
exception Lua_error of value

let drop x = ()

let get_returned (state : state) : value list =
  let returned = state.returned in
  begin
    state.returned <- [];
    returned
  end

let rec lua_print (state : state) (vals : value list) : bool =
  match vals with
  | v :: rest -> begin
      begin
        match v with
        | Value v -> (
            match v with
            | Nil -> print_string "nil"
            | True -> print_string "true"
            | False -> print_string "false"
            | Number n ->
                if Float.is_integer n then
                  Float.to_int n |> Int.to_string |> print_string
                else
                  Float.to_string n |> print_string
            | String s -> print_string s
            | Table t -> print_string "table")
        | Function _ | Builtin _ -> print_string "Function"
      end;
      if List.length rest > 0 then
        print_string "\t"
      else
        ();
      lua_print state rest
    end
  | [] -> begin
      print_endline "";
      false
    end

let lua_error (state : state) (vals : value list) : bool =
  let value = match vals with [] -> Value Nil | v :: _ -> v in
  raise (Lua_error value)

let get_num_val = function
  | Value (Number n) -> n
  | Value (String s) -> Float.of_string s
  | _ -> raise Invalid_value

let get_bool_val = function
  | Value Nil | Value False -> Value False
  | other -> other

let neg_val = function
  | Value False | Value Nil -> Value True
  | _ -> Value False

let bool_of_val = function
  | Value Nil | Value False -> false
  | other -> true

let rec exec_binop (op : binary_op) (e1 : value) (e2 : value) : value
    =
  match op with
  | Add ->
      let v1 = get_num_val e1 in
      let v2 = get_num_val e2 in
      Value (Number (v1 +. v2))
  | Subtract ->
      let v1 = get_num_val e1 in
      let v2 = get_num_val e2 in
      Value (Number (v1 -. v2))
  | Multiply ->
      let v1 = get_num_val e1 in
      let v2 = get_num_val e2 in
      Value (Number (v1 *. v2))
  | Divide ->
      let v1 = get_num_val e1 in
      let v2 = get_num_val e2 in
      Value (Number (v1 /. v2))
  | Equal -> is_op Float.( = ) String.( = ) e1 e2
  | NotEqual -> exec_binop Equal e1 e2 |> neg_val
  | Less -> is_op Float.( < ) String.( < ) e1 e2
  | LessEqual -> is_op Float.( <= ) String.( <= ) e1 e2
  | Greater -> exec_binop LessEqual e2 e1
  | GreaterEqual -> exec_binop Less e2 e1
  | And -> begin
      match get_bool_val e1 with
      | Value False -> Value False
      | _ -> get_bool_val e2
    end
  | Or -> begin
      match get_bool_val e1 with
      | Value False -> get_bool_val e2
      | other -> other
    end
  | Concat -> begin
      match (e1, e2) with
      | Value (String v1), Value (String v2) ->
          Value (String (v1 ^ v2))
      | _ -> raise Invalid_value
    end
  | _ -> raise Unimplemented

and is_op (numop : numop) (strop : strop) (e1 : value) (e2 : value) :
    value =
  match (e1, e2) with
  | Value (Number v1), Value (Number v2) ->
      Value (if numop v1 v2 then True else False)
  | Value (String v1), Value (String v2) ->
      Value (if strop v1 v2 then True else False)
  | Value (Number v1), Value (String v2) -> begin
      match Float.of_string_opt v2 with
      | None -> raise Invalid_value
      | Some n -> Value (if numop v1 n then True else False)
    end
  | Value (String v1), Value (Number v2) ->
      is_op numop strop e2 e1 |> neg_val
  | _ -> raise Invalid_value

let exec_unop (op : unary_op) (v : value) : value =
  match op with
  | Not -> neg_val v
  | Negate -> begin
      match v with
      | Value (Number n) -> Value (Number (-.n))
      | _ -> raise Invalid_value
    end
  | Length -> raise Unimplemented

(* *)
