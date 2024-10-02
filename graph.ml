exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let rec dfs g s =
  if s.n_mark = InProgress then
    true
  else if s.n_mark = NotVisited then begin
    s.n_mark <- InProgress;
    let b = List.fold_left (||) false (List.map (dfs g) s.n_link_to) in
    s.n_mark <- Visited; b
  end else
    false

let rec erase =
  let rec dfs g s =
    if s.n_mark = Visited then begin
      s.n_mark <- NotVisited;
      List.iter (dfs g) (s.n_link_to)
    end
  in dfs

let has_cycle g = 
  let b = List.fold_left (||) false (List.map (dfs g) g.g_nodes) in
  List.iter (erase g) (g.g_nodes);
  b

let topological g =
  let t = ref [] in
  let rec tri g s =
    if s.n_mark = NotVisited then begin
      s.n_mark <- InProgress;
      List.iter (tri g) s.n_link_to;
      t := s.n_label :: !t;
      s.n_mark <- Visited
    end
    else if s.n_mark = InProgress then raise Cycle
  in List.iter (tri g) g.g_nodes; List.iter (erase g) g.g_nodes; !t
