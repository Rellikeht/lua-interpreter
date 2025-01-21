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

let set_local (state : state) (name : name) (v : value) =
  let rec setl = function
    | [] -> false
    | level :: rest -> begin
        match Hashtbl.find level name with
        | Some _ -> begin
            Hashtbl.set level ~key:name ~data:v;
            true
          end
        | None -> setl rest
      end
  in
  if setl state.locals then
    ()
  else
    let level =
      match state.locals with
      | level :: _ -> level
      | [] -> begin
          let fresh = Hashtbl.create (module Name) in
          state.locals <- [ fresh ];
          fresh
        end
    in
    Hashtbl.set level ~key:name ~data:v

let set_global (state : state) (name : name) (v : value) =
  Hashtbl.set state.globals ~key:name ~data:v

let add_function
    (state : state)
    (name : funcname)
    (body : parameter_list * chunk) =
  ()
