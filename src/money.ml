open Graph

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

    let amounts = loop 2 [] in

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

  close_in infile ;
  loop 2 initial_graph


let init_arcs graph total amounts =
  let moy = total / List.length amounts in
  let due = List.map ( fun (n, x) -> (n, x-moy) ) amounts in
  let rec loop gr = function
    | [] | (n, 0) -> graph
    | (n, x) :: tail -> if x < 0 then loop (add_arc gr 0 n (-x)) tail else loop (add_arc gr n 1 x) tail
  in
  loop graph due
(* TODO VERIFIER ^^ *)
 
let get_exchange_graph path =

  let amounts = get_amounts path in
  let total_money =
    let rec loop total = function
      | [] -> total
      | (_, amount) :: tail -> loop (total+amount) tail
    in
    let total = loop 0 amounts in
    Printf.printf "\nTotal = %d\n" total ;
    total
  in

  let initial_graph = init_nodes path in
  let total = total_money in
  let final_graph = init_arcs initial_graph total amounts in

  final_graph