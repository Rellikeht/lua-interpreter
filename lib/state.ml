open Ast
open Values
open Base
module Name = String

(* for debug *)
open Stdio

let fresh_level () = Hashtbl.create (module Name)

let initial_symbols : (name, value) Hashtbl.t =
  let symbols = fresh_level () in
  let _ =
    Hashtbl.add symbols ~key:"print" ~data:(Builtin lua_print)
  in
  symbols

let initial_state () =
  {
    (* *)
    (* line = 0; *)
    globals = Hashtbl.copy initial_symbols;
    locals = [];
    breaking = false;
    returning = false;
  }

let get_value (state : state) (name : name) : value option =
  let rec get_local = function
    | [] -> None
    | level :: rest -> begin
        match Hashtbl.find level name with
        | Some v -> Some v
        | None -> get_local rest
      end
  in
  match get_local state.locals with
  | Some v -> Some v
  | None -> Hashtbl.find state.globals name

let set_value (state : state) (name : name) (value : value) : unit =
  let rec set_local = function
    | [] -> false
    | level :: rest -> begin
        match Hashtbl.find level name with
        | Some v -> begin
            Hashtbl.set level ~key:name ~data:value;
            true
          end
        | None -> set_local rest
      end
  in
  if set_local state.locals then
    ()
  else
    Hashtbl.set state.globals ~key:name ~data:value

let set_local (state : state) (name : name) (v : value) =
  match state.locals |> List.hd with
  | None -> raise Unreachable
  | Some level -> Hashtbl.set level ~key:name ~data:v

let add_function
    (state : state)
    (name : funcname)
    (body : parameter_list * chunk) =
  ()
