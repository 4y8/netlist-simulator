# Netlist Simulator
## Presentation
A small transpiler from netlist to
C. [Here](https://github.com/4y8/netlist-simulator) is a link to the github
repository.

To build run :
```
make
```

It will create an executable `netlist_simulator` which can be run with
`./netlist_simulator [-s] <file>` to output a file `out.c`. Once compiled it can
be run with `./a.out [n]` where n is the optional number of steps. By default
the C program expects a `rom` directory where each `x=ROM(...)` has a
corresponding file consiting in a sequence of zeros and ones. This behaviour can
be changed by the `-s` option of netlist\_simulator which toggles static
mode. The rom files are now expected by the netlist\_simulator program directly
(in the same format) and are put in the C file. This has been implemented hoping
that an optimizing C compiler could use the ROMs to further optimize the
program.

## Behaviour of the simulator
The `netlist_simulator` creates, given a netlist, a C program which when
compiled runs a loop which reads inputs, apply the logic given by the netlist
and prints the value of the outputs. To be precise, initially registers and RAM
are set to 0, then at each step, the following happen:
* the inputs are read,
* then the netlist is executed, omitting registers and RAM writes (the values to
be written in registers and RAM are just stored),
* the RAM is updated,
* the registers are updated,
* the outputs are printed. 

Everithing is stored on unsigned 64-bits integers as it is the standard size on
modern computers, bus of size 65 or plus are thus not supported. It should not
be a problem as the CPU which will be designed shouldn't use too wide buses.

## Difficulties encountered
The main difficulty was to understand well the memory model which is different
from the way we interact with RAM and ROM on a higher-level.
