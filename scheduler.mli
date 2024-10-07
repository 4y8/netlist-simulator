(** [schedule p] modifies the order of the equations in [p] so that each
    expressions depends only on previously defined nodes.
    
    @raise Combinational_cycle if such an order doesn't exist. *)
val schedule : Netlist_ast.program -> Netlist_ast.program

exception Combinational_cycle
