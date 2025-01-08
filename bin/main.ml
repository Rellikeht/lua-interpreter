open Interpreter

let filename = Sys.argv.(1);;

if Array.length Sys.argv < 2 then (
  Printf.fprintf stderr "Usage: %s <lua_file>\n" Sys.argv.(0);
  exit 1)
;;

Printing.dump_tree ~tokens:true filename
