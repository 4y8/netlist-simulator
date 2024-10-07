type mark

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

(** [mk_graph ()] returns an empty graph. *)
val mk_graph : unit -> 'a graph

(** [add_node g x] adds [x] to [g] as a node. *)
val add_node : 'a graph -> 'a -> unit

(** [add_edge g e1 e2] adds ([e1], [e2]) to [g] as an edge. *)
val add_edge : 'a graph -> 'a -> 'a -> unit 

(** [has_cycle g] checks if [g] has a cycle. *)
val has_cycle : 'a graph -> bool

(** [topological g] computes a topological order of the vertices of [g].

    @raise Cycle if such an order doesn't exist. *)
val topological : 'a graph -> 'a list

exception Cycle
