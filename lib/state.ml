open Ast
open Values
open Base
open Stdio
module Name = String

let rec lua_print (state : state) (vals : value list) =
  match vals with
  | v :: rest -> begin
      begin
        match v with
        | Value v -> (
            match v with
            | Nil -> print_string "nil"
            | True -> print_string "true"
            | False -> print_string "false"
            | Number n -> Float.to_string n |> print_endline
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
  | [] -> print_endline ""

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
    (s : state)
    (n : funcname)
    (b : parameter_list * chunk) =
  ()

let update_locals (s : state) (ns : name list) (es : exp list) = ()
