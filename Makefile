all: netlist_simulator

netlist_simulator: netlist_simulator.ml graph.ml graph.mli scheduler.ml scheduler.mli netlist_ast.ml netlist_parser.mly netlist_lexer.mll netlist.ml
	dune build ./netlist_simulator.exe
	cp ./_build/default/netlist_simulator.exe ./netlist_simulator
