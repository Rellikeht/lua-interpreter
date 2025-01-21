open Ast
open Base

(* open State *)

type simple_value =
  | Nil
  | True
  | False
  | Number of number_value
  | String of string
  | Table of (value, value) Hashtbl.t

and builtin_func = state -> value list -> unit

and value =
  | Value of simple_value
  | Function of chunk
  | Builtin of builtin_func

and state = {
  (* *)
  mutable line : int;
  symbols : (name, value) Hashtbl.t;
}

exception Unimplemented
exception Nil_Call
exception Nil_Index
exception Value_call of simple_value
exception Invalid_value

let get_num_val = function
  | Value (Number n) -> n
  | Value (String s) -> Float.of_string s
  | _ -> raise Invalid_value

let exec_binop (op : binary_op) (e1 : value) (e2 : value) :
    number_value =
  match op with
  | Add ->
      let v1 = get_num_val e1 in
      let v2 = get_num_val e2 in
      v1 +. v2
  | Subtract ->
      let v1 = get_num_val e1 in
      let v2 = get_num_val e2 in
      v1 -. v2
  | _ -> raise Unimplemented

(* *)
