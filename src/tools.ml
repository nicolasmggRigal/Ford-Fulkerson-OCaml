(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f =
  let gr1 = clone_nodes gr in
  let ff graph id1 id2 pd = new_arc graph id1 id2 (f pd) in
  e_fold gr ff gr1

let add_arc gr id1 id2 pd = 
  let a = find_arc gr id1 id2 in
  match a with 
  | Some x -> new_arc gr id1 id2 (x + pd)
  | None -> new_arc gr id1 id2 pd

let graph_string_of_int gr = gmap gr (fun pd -> string_of_int pd)

let graph_int_of_string gr = gmap gr (fun pd -> int_of_string pd)