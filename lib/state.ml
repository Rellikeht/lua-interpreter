open Ast
open Values
open Base
module Name = String

let initial_symbols : (name, value) Hashtbl.t =
  let symbols = Hashtbl.create (module Name) in
  let _ =
    Hashtbl.add symbols ~key:"print" ~data:(Builtin lua_print)
  in
  symbols

let initial_state () =
  {
    (* *)
    line = 0;
    symbols = Hashtbl.copy initial_symbols;
  }

let add_function
    (state : state)
    (name : funcname)
    (body : parameter_list * chunk) =
  ()

let update_locals
    (state : state)
    (names : name list)
    (exps : exp list) =
  ()
