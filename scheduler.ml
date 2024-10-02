open Netlist_ast
open Graph

exception Combinational_cycle

let free_var_arg = function Avar s -> [s] | Aconst _ -> []

let free_var = function
    Earg a -> free_var_arg a
  | Ereg _ -> []
  | Enot a -> free_var_arg a
  | Ebinop (_, a, a') -> free_var_arg a @ free_var_arg a'
  | Emux (a, a', a'') ->
    free_var_arg a @ free_var_arg a' @ free_var_arg a''
  | Erom (_, _, a) -> free_var_arg a
  | Eram (_, _, a, _, _, _) ->
    free_var_arg a
  | Econcat (a, a') -> free_var_arg a @ free_var_arg a'
  | Eslice (_, _, a) -> free_var_arg a
  | Eselect (_, a) -> free_var_arg a

let read_exp (x, e) =
  x :: free_var e

let build_graph p =
  let l = p.p_eqs in
  let g = mk_graph () in
  List.iter (fun (x, e) -> add_node g (x)) l;
  List.iter (add_node g) p.p_inputs;
  List.iter (add_node g) p.p_outputs;
  let add (x, e) =
    List.iter (add_edge g x) (free_var e)
  in
  List.iter add l;
  g

let rec map_opt f = function
    [] -> []
  | hd :: tl -> match f hd with None -> map_opt f tl
                              | Some x -> x :: map_opt f tl

let schedule p =
  let g = build_graph p in
  try
    let l = List.rev @@ topological g in
    { p_eqs = map_opt (fun v -> match List.assoc_opt v p.p_eqs with
            None -> None
          | Some e -> Some (v, e)) l ; p_inputs = p.p_inputs ; p_outputs = p.p_outputs ;
      p_vars = p.p_vars }
  with
  Cycle -> raise Combinational_cycle
