open Graph
open Tools

(*Concept : 
FordFulkerson mais en plus il y a des coûts pour chaque arc...

Graph de début :
1) Recherche de chemin avec coût le plus petit -> choix du chemin
2) Update des flots
3) Update des coûts : si tu peux toujours mettre : laisser la flèche, si tu peux mettre de l'autre côté : mettre une flèche de coût négatif. Si on ne peut plus mettre on enlève...
4) On reboucle*)
 
(* string graph -> (flow x cost) graph *)
let create_graph gr = 
  let empty = clone_nodes gr in
  e_fold gr (fun newgr id1 id2 pd -> new_arc newgr id1 id2 (Scanf.sscanf "%d (%d)" (fun flow cost -> (flow, cost)))) empty

(* finds a path and the associated max flow to add between two nodes, without the first node and it's flow *)
(* based on a depth path search *)
(* A modifier pour Busacker-Gowen *)
let rec step_path_and_flow gr visited id1 id2 =
  let succ = out_arcs gr id1 in
  let rec loop acu max_flow pathsize = function
    | [] -> None
    | (_, 0) :: tail -> loop acu max_flow tail
    | (head, (flow, cost)) :: tail when head = id2 -> Some ((id2 :: acu), min max_flow flow, pathsize + cost)
    | (head, (flow, cost)) :: tail when List.mem head visited -> loop acu max_flow pathsize tail
    | (head, (flow, cost)) :: tail -> (
      match step_path_and_flow gr (head :: visited) head id2 with
        | None -> loop acu max_flow tail
        | Some (chemin, f, c) -> Some ((head :: chemin), min flow f, cost + c)
    )
  in
  loop [] max_int succ

(* finds a path and the associated max flow to add between two nodes *)
let find_path_and_flow gr id1 id2 = assert false

(* adds the given flow to every arc along the given path *)
let add_flow_to_path gr (path, flow) = assert false

(* gives the corresponding residual graph when given the initial capacity graph *)
let init_residual_graph gr = assert false

(* gives the flow graph when given a residual graph and it's initial capacity graph *)
let get_flow_graph flow_gr base_gr = assert false

(* maximises flow through a residual graph *)
let ff_iterate gr start finish = assert false

(* gives the associated max flow graph when given a capity graph *)
let ford_fulkerson graph start finish = assert false