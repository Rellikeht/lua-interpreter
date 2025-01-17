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

let lua_print (s : state) (vals : value list) = ()

let initial_symbols : (name, value) Hashtbl.t =
  let symbols = Hashtbl.create (module Name) in
  let _ = Hashtbl.add symbols ~key:"print" ~data:(Builtin lua_print) in
  symbols

let initial_state () =
  {
    (* *)
    line = 0;
    symbols = Hashtbl.copy initial_symbols;
  }

let add_function (s : state) (n : funcname) (b : parameter_list * chunk) = ()
let update_locals (s : state) (ns : name list) (es : exp list) = ()
