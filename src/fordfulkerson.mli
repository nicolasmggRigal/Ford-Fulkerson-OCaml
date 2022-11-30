open Graph

type path = id list



(********************************************************************************)
(************* PRIVATE FUNCTIONS, DO NOT USE OUTSIDE OF THIS PACKAGE ************)
(********************************************************************************)

(* finds a path between two nodes, without the first node *)
val step_path: int graph -> id list -> id -> id -> path option

(* finds a path and the associated max flow to add between two nodes, without the first node and it's flow *)
val step_path_and_flow: int graph -> id list -> id -> id -> (path * int) option



(********************************************************************************)
(*************************** INTERMEDIATE FUNCTIONS *****************************)
(********************************************************************************)

(* finds a path and the associated max flow to add between two nodes *)
val find_path_and_flow: int graph -> id -> id -> (path * int) option

(* adds the given flow to every arc along the given path *)
val add_flow_to_path: int graph -> (path * int) -> int graph

(* gives the corresponding residual graph when given the initial capacity graph *)
val init_residual_graph: string graph -> int graph

(* gives the flow graph when given a residual graph and it's initial capacity graph *)
val get_flow_graph: int graph -> string graph -> string graph

(* maximises flow through a residual graph *)
val ff_iterate: int graph -> id -> id -> int graph



(* gives the associated max flow graph when given a capity graph *)
val ford_fulkerson: string graph -> id -> id -> string graph