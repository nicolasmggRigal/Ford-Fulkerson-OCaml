open Graph
open Tools

type path = id list

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
  e_fold gr (fun newgr id1 id2 label -> new_arc newgr id1 id2 (Scanf.sscanf label "%d (%d)" (fun flow cost -> (flow, cost)))) empty

(* finds a path and the associated max flow to add between two nodes, without the first node and it's flow *)
(* based on a depth path search *)
(* A modifier pour Busacker-Gowen *)
(* Ca me soule, bonne chance Nico *)
let rec step_path_and_flow gr visited id1 id2 =
  let succ = out_arcs gr id1 in
  let rec loop acu path max_flow pathsize = function
    | [] -> path
    | (_, (0, _)) :: tail -> loop acu path max_flow pathsize tail
    | (head, (flow, cost)) :: tail when head = id2 -> Some ((id2 :: acu), min max_flow flow, pathsize + cost)
    | (head, (flow, cost)) :: tail when List.mem head visited -> loop acu path max_flow pathsize tail
    | (head, (flow, cost)) :: tail -> (
      match step_path_and_flow gr (head :: visited) head id2 with
        | None -> loop paths acu max_flow pathsize tail
        | Some (chemin, f, c) -> if (cost + c) < fraise then Some ((head :: chemin), min flow f, cost + c) else Some path
    )
  in
  loop [] None max_int max_int succ

(* finds a path and the associated max flow to add between two nodes *)
let find_path_and_flow gr id1 id2 =
  let get_first = function
    | e :: _ -> e
    | _ -> 0 (* inutile car toujours utilise avec au moins 1 element mais besoin pour pattern matching *)
  in
  match step_path_and_flow gr [] id1 id2 with
    | Some (p, max_flow, min_cost) -> (
      match find_arc gr id1 (get_first p) with
        | Some (flow, cost) -> Some (id1 :: p, min max_flow flow, min_cost + cost)
        | None -> Some (id1 :: p, max_flow, min_cost) (* inutile puisque l'arc existera toujours mais besoin pour pattern matching *)
    )
    | None -> None

(* adds the given flow to every arc along the given path *)
(* A changer totalement *)
let rec add_flow_to_path gr (path, flow) =
  match path with
    | [] -> gr
    | _ :: [] -> gr
    | id1 :: id2 :: tail -> add_flow_to_path (add_arc (add_arc gr id1 id2 (-flow)) id2 id1 flow) ((id2 :: tail), flow)

(* gives the corresponding residual graph when given the initial capacity graph *)
let init_residual_graph gr = assert false

(* gives the flow graph when given a residual graph and it's initial capacity graph *)
let get_flow_graph flow_gr base_gr = assert false

(* maximises flow through a residual graph *)
let ff_iterate gr start finish = assert false

(* gives the associated max flow graph when given a capity graph *)
let busacker_gowen graph start finish = assert false