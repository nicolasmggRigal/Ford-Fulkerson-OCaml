open Graph

type path = string

val get_amounts: path -> (id * int) list
val init_nodes: path -> int graph
val init_arcs: int graph -> int -> (id * int) list -> int graph
val get_exchange_graph: path -> string graph