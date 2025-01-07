open Netlist_ast
open Graph

exception Combinational_cycle

let free_var_arg = function Avar s -> [s] | Aconst _ -> []

(** [nec_var e] returns the list of nodes on which the output of the
    expression [e] depends*)
let nec_var = function
    Earg a -> free_var_arg a
  | Ereg _ -> []
  | Enot a -> free_var_arg a
  | Ebinop (_, a, a') -> free_var_arg a @ free_var_arg a'
  | Emux (a, a', a'') ->
    free_var_arg a @ free_var_arg a' @ free_var_arg a''
  | Erom (_, _, a) -> free_var_arg a
  | Eram (_, _, a, wen, wad, _) ->
    free_var_arg a @ free_var_arg wen @ free_var_arg wad
  | Econcat (a, a') -> free_var_arg a @ free_var_arg a'
  | Eslice (_, _, a) -> free_var_arg a
  | Eselect (_, a) -> free_var_arg a

let read_exp (x, e) =
  x :: nec_var e

(** [build_graph p] returns the graph of dependencies of [p] *)
let build_graph p =
  let l = p.p_eqs in
  let g = mk_graph () in
  List.iter (fun (x, e) -> add_node g (x)) l;
  List.iter (add_node g) p.p_inputs;
  List.iter (add_node g) p.p_outputs;
  let add (x, e) =
    List.iter (add_edge g x) (nec_var e)
  in
  List.iter add l;
  g

let schedule p =
  let g = build_graph p in
  try
    let l = topological g in
    let get_def v =
      match List.assoc_opt v p.p_eqs with
        None -> None
      | Some e -> Some (v, e)
    in
    { p with p_eqs = Utils.map_opt get_def l }
  with
    Cycle -> raise Combinational_cycle
