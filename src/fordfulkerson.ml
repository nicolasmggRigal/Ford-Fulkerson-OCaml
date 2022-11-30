open Graph
open Tools

type path = id list



(********************************************************************************)
(************* PRIVATE FUNCTIONS, DO NOT USE OUTSIDE OF THIS PACKAGE ************)
(********************************************************************************)


(* finds a path between two nodes, without the first node *)
let rec step_path gr visited id1 id2 =
  let succ = out_arcs gr id1 in
  let rec loop acu = function
    | [] -> None
    | (head, _) :: tail when head = id2 -> Some (id2 :: acu)
    | (head, _) :: tail when List.mem head visited -> loop acu tail
    | (head, _) :: tail -> (
      match step_path gr (head :: visited) head id2 with
        | None -> loop acu tail
        | Some chemin -> Some (head :: chemin)
    )
  in
  loop [] succ


(* finds a path and the associated max flow to add between two nodes, without the first node and it's flow *)
(* based on a depth path search *)
  let rec step_path_and_flow gr visited id1 id2 =
    let succ = out_arcs gr id1 in
    let rec loop acu max_flow = function
      | [] -> None
      | (_, 0) :: tail -> loop acu max_flow tail
      | (head, flow) :: tail when head = id2 -> Some ((id2 :: acu), min max_flow flow)
      | (head, flow) :: tail when List.mem head visited -> loop acu max_flow tail 
      | (head, flow) :: tail -> (
        match step_path_and_flow gr (head :: visited) head id2 with
          | None -> loop acu max_flow tail
          | Some (chemin, f) -> Some ((head :: chemin), min flow f)
      )
    in
    loop [] max_int succ




(********************************************************************************)
(*************************** INTERMEDIATE FUNCTIONS *****************************)
(********************************************************************************)

(* finds a path and the associated max flow to add between two nodes *)
let find_path_and_flow gr id1 id2 =
  let get_first = function
    | e :: _ -> e
    | _ -> 0 (* inutile car toujours utilise avec au moins 1 element mais besoin pour pattern matching *)
  in
  match step_path_and_flow gr [] id1 id2 with
    | Some (p, max_flow) -> (
      match find_arc gr id1 (get_first p) with
        | Some flow -> Some (id1 :: p, min max_flow flow)
        | None -> Some (id1 :: p, max_flow) (* inutile puisque l'arc existera toujours mais besoin pour pattern matching *)
    )
    | None -> None


(* adds the given flow to every arc along the given path *)
let rec add_flow_to_path gr (path, flow) =
  match path with
    | [] -> gr
    | _ :: [] -> gr
    | id1 :: id2 :: tail -> add_flow_to_path (add_arc (add_arc gr id1 id2 (-flow)) id2 id1 flow) ((id2 :: tail), flow)
    

(* gives the corresponding residual graph when given the initial capacity graph *)
let init_residual_graph gr =
  let gr1 = graph_int_of_string gr in
  let tmp gr id1 id2 pd = add_arc gr id2 id1 0 in
  let gr2 = gr1 in
  e_fold gr1 tmp gr2


(* gives the flow graph when given a residual graph and it's initial capacity graph *)
let get_flow_graph flow_gr base_gr =
  let new_gr = clone_nodes flow_gr in
  let beautifulise gr id1 id2 pd =
    match find_arc flow_gr id1 id2 with
      | None -> gr
      | Some s ->
        let value = string_of_int (max 0 (int_of_string pd - s)) in
        new_arc gr id1 id2 (value ^ "/" ^ pd)
  in
  e_fold base_gr beautifulise new_gr


(* maximises flow through a residual graph *)
let rec ff_iterate gr start finish = match find_path_and_flow gr start finish with
  | None -> gr
  | Some res -> ff_iterate (add_flow_to_path gr res) start finish 



let ford_fulkerson graph start finish =
  let residual_graph = init_residual_graph graph in
  let graphe_resultat = ff_iterate residual_graph start finish in
  get_flow_graph graphe_resultat graph
