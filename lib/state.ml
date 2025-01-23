open Ast
open Values
open Base
module Name = String

let fresh_level () = Hashtbl.create (module Name)

let initial_symbols : (name, value) Hashtbl.t =
  let symbols = fresh_level () in
  begin
    Hashtbl.add symbols ~key:"print" ~data:(Builtin lua_print) |> drop;
    Hashtbl.add symbols ~key:"error" ~data:(Builtin lua_error) |> drop;
    symbols
  end

let initial_state () =
  {
    (* line = 0; *)
    globals = Hashtbl.copy initial_symbols;
    locals = [];
    breaking = false;
    returning = false;
    returned = [];
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

let rec update_level
    (level : symbols)
    (names : name list)
    (values : value list) : unit =
  match names with
  | [] -> ()
  | name :: names_left -> begin
      match values with
      | [] -> ()
      | value :: values_left -> begin
          Hashtbl.set level ~key:name ~data:value;
          update_level level names_left values_left
        end
    end

let add_function
    (state : state)
    ((base, fields, mname) : funcname)
    ((params, body) : funcbody) =
  Hashtbl.set state.globals ~key:base ~data:(Function (params, body))

(* *)
