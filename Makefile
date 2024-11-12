all: netlist_simulator

netlist_simulator: netlist_simulator.ml 
	dune build ./netlist_simulator.exe
	cp ./_build/default/netlist_simulator.exe ./netlist_simulator
