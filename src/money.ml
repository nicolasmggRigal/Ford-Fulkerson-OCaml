open Graph
open Tools
open Fordfulkerson
open Printf

type path = string


let get_amounts path =
  let infile = open_in path in

    let rec loop n list =
      try
        let line = input_line infile in
        let line = String.trim line in
        let list2 =
          (* Ignore empty and comment lines *)
          if (line = "") || (line.[0] = '#') then list
          else
            try Scanf.sscanf line "%s %d" (fun _ don -> (n, don) :: list)
          with e ->
            Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
            failwith "from_file"
        in
        loop (n+1) list2
      with End_of_file -> list (* Done *)
    in

    let amounts = loop 1 [] in

    close_in infile;
    amounts

let init_nodes path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2) =
        (* Ignore empty and comment lines *)
        if (line = "") || (line.[0] = '#') then (n, graph)
        else (n+1, new_node graph n)
      in      
      loop n2 graph2

    with End_of_file -> graph (* Done *)
  in
  let initial_graph = new_node (new_node empty_graph 0) 1 in

  let res = loop 2 initial_graph in
  close_in infile ;
  res


let init_arcs graph total amounts =
  (* On doit tout multiplier par le nombre de participants pour travailler avec des int *)
  let buffed_amounts = List.map (fun (n, x) -> (n, (List.length amounts) * x) ) amounts
  and buffed_total = total * (List.length amounts) in

  let moy = buffed_total / List.length buffed_amounts in
  let due = List.map ( fun (n, x) -> Printf.printf "noeud : %d, du : %d\n" n (x-moy) ; (n, x-moy) ) buffed_amounts in
  (* boucle pour lier les noeuds aux puits/source *)
  let rec loop gr = function
    | [] -> gr
    | (_, 0) :: tail -> loop gr tail
    | (n, x) :: tail -> if x < 0 then loop (new_arc gr 0 n (-x)) tail else loop (new_arc gr n 1 x) tail
  in
  (* boucle pour lier les noeuds entre eux *)
  n_fold graph (
    fun gr1 id1 -> if id1 <> 0 && id1 <> 1 then
      n_fold graph (
        fun gr2 id2 -> if id2 <> 0 && id2 <> 1 && id2 <> id1 then new_arc gr2 id1 id2 buffed_total else gr2
        ) gr1
    else
      gr1
  ) (loop graph due)
 
let get_exchange_graph path =

  let amounts = get_amounts path in
  let total_money =
    let rec loop total = function
      | [] -> total
      | (_, amount) :: tail -> loop (total+amount) tail
    in
    let total = loop 0 amounts in
    Printf.printf "\nTotal = %d\n" total ;
    Printf.printf "Path = %s\n" path ;
    total
  in

  let initial_graph = init_nodes path in
  let total = total_money in

  (* On doit revenir aux valeurs initiales *)
  let step_graph = float_ford_fulkerson (graph_string_of_int (init_arcs initial_graph total amounts)) 0 1 (List.length amounts) in

  let final_graph = e_fold step_graph (fun gr id1 id2 pd -> if id1 = 0 || id1 = 1 || id2 = 0 || id2 = 1 then gr else new_arc gr id1 id2 pd) (n_fold step_graph (fun gr id -> if id = 0 || id = 1 then gr else new_node gr id) empty_graph) in

  final_graph  

let get_names path = 
  let infile = open_in path in
  let rec loop names =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in
      
      (* Ignore empty and comment lines *)
      if (line = "") || (line.[0] = '#') then loop names
      else 
        let name = Scanf.sscanf line "%s %d" (fun name _ -> printf "%s\n" name ; name) in
        loop (name :: names)

    with End_of_file -> List.rev names (* Done *)
  in
  loop [] 

let export_money gr path =
  (* Open a write-file. *)
  let ff = open_out "graph.gv" in
  let names = get_names path in

  (* Write in this file. *)
  fprintf ff "digraph outgraph {
  \tfontname=\"Helvetica,Arial,sans-serif\"
  \tnode [fontname=\"Helvetica,Arial,sans-serif\"]
  \tedge [fontname=\"Helvetica,Arial,sans-serif\"]
  \trankdir=LR;
  \tnode [shape = circle];" ;

  n_iter_sorted gr (fun id -> fprintf ff " %s" (List.nth names (id - 2))) ;

  (* Write all arcs *)
  e_iter gr (fun id1 id2 lbl -> printf "%d %d\n" id1 id2 ; fprintf ff "\n\t%s -> %s [label = \"%s\"];" (List.nth names (id1 - 2)) (List.nth names (id2 - 2)) lbl) ;

  fprintf ff "\n}\n" ;

  close_out ff ;
  ()