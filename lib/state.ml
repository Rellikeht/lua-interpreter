open Ast
open Base
module Name = String

type simple_value =
  | Nil
  | True
  | False
  | Number of number_value
  | String of string
  | Table of (value, value) Hashtbl.t

and value =
  | Value of simple_value
  | Function of chunk
  | Builtin of (state * value list -> state)

and state = {
  (* *)
  mutable line : int;
  symbols : (name, value) Hashtbl.t;
}

let initial_symbols : (name, value) Hashtbl.t =
  let symbols = Hashtbl.create (module Name) in
  symbols

let initial_state () =
  {
    (* *)
    line = 0;
    symbols = Hashtbl.copy initial_symbols;
  }
