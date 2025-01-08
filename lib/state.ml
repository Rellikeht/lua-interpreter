open Ast
open Base
module Name = String

type value =
  | Nil
  | True
  | False
  | Number of number_value
  | String of string
  | Table of (value, value) Hashtbl.t

and vtype =
  | Value of value
  | Function of chunk
  | Builtin of (state * vtype list -> state)

and state = {
  (* *)
  mutable line : int;
  symbols : (name, vtype) Hashtbl.t;
}

let initial_symbols : (name, vtype) Hashtbl.t =
  let symbols = Hashtbl.create (module Name) in
  symbols

let initial_state () =
  {
    (* *)
    line = 0;
    symbols = Hashtbl.copy initial_symbols;
  }
