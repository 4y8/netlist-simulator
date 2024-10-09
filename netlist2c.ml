open Netlist_ast

let read_rom = ref ""

let build_mem fd p =
  let treat (x, e) = match e with
      Eram (ads, ws, _, _, _ ,_) ->
      Printf.fprintf fd "uint64_t ram_%s[%d] = {0}; uint64_t %s = 0; uint64_t wad_%s = 0; uint64_t wda_%s = 0;" x (1 lsl ads) x x x
    | Erom (ads, ws, _) ->
      Printf.fprintf fd "uint64_t rom_%s[%d] = {0}; uint64_t %s = 0;" x (1 lsl ads) x;
      read_rom :=
        !read_rom ^ Printf.sprintf
          "f_ = fopen(\"rom/%s\", \"r\"); for (int j_ = 0; (c_ = getc(f_)) != EOF; ++j_)
rom_%s[j_/%d] |= (c_ == '1') << (%d - (j_ %% %d) - 1); fclose(f_);" x x ws ws ws
    | Ereg _ -> Printf.fprintf fd "uint64_t %s = 0; uint64_t tmp_%s_ = 0;" x x
    | _ -> Printf.fprintf fd "uint64_t %s = 0;" x
  in
  List.iter treat p

let compile f =
  let p = Netlist.read_file f in
  let p = Scheduler.schedule p in
  let fd = open_out "out.c" in
  Printf.fprintf fd "#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <unistd.h>\n";
  build_mem fd p.p_eqs;
  List.iter (Printf.fprintf fd "uint64_t %s;") p.p_inputs;
  Printf.fprintf fd
    "int main(){char buf[100]; FILE *f_; int c_; %s for (int i_ = 0; 1; ++i_){printf(\"cycle : %%d\\n\", i_);" !read_rom;
  let read_input x =
    Printf.fprintf fd "printf(\"%s? \");fgets(buf, 100, stdin);%s=strtoul(buf, NULL, 2);" x x
  in
  List.iter read_input p.p_inputs;
  let print = function
      Avar s -> s
    | Aconst (VBit b) -> if b then "1" else "0"
    | Aconst (VBitArray a) ->
      "0b" ^ (Array.fold_right (^) (Array.map (fun b -> if b then "1" else "0") a) "")
  in
  let load = function
      Avar s -> output_string fd s
    | Aconst (VBit b) -> output_string fd @@ if b then "1" else "0"
    | Aconst (VBitArray a) ->
      output_string fd "0b";
      Array.iter (fun b -> output_string fd @@ if b then "1" else "0") a
  in
  let size = function
    Avar x -> begin
      match Env.find x p.p_vars with
        TBit -> 1
      | TBitArray n -> n
    end
    | Aconst (VBit _) -> 1
    | Aconst (VBitArray a) -> Array.length a
  in
  let end_loop_ram = ref "" in
  let end_loop_reg = ref "" in
  let emit_equ (x, e) = match e with
      Earg a -> Printf.fprintf fd "%s=" x; load a; Printf.fprintf fd ";"
    | Ereg y ->
      end_loop_ram := Printf.sprintf "tmp_%s_=%s;" x y ^ !end_loop_ram;
      end_loop_reg := Printf.sprintf "%s=tmp_%s_;" x x ^ !end_loop_reg
    | Enot a -> Printf.fprintf fd "%s=~" x; load a; Printf.fprintf fd ";"
    | Ebinop (Or, a, a') ->
      Printf.(fprintf fd "%s=" x; load a; fprintf fd "|"; load a'; fprintf fd ";")
    | Ebinop (And, a, a') ->
      Printf.(fprintf fd "%s=" x; load a; fprintf fd "&"; load a'; fprintf fd ";")
    | Ebinop (Xor, a, a') ->
      Printf.(fprintf fd "%s=" x; load a; fprintf fd "^"; load a'; fprintf fd ";")
    | Ebinop (Nand, a, a') ->
      Printf.(fprintf fd "%s=~(" x; load a; fprintf fd "&"; load a'; fprintf fd ");")
    | Emux (c, f, t) ->
      Printf.(fprintf fd "%s=(" x; load c; fprintf fd ") ?(";
              load t; fprintf fd ") : ("; load f; fprintf fd ");")
    | Erom (ads, _, a) ->
      Printf.(fprintf fd "%s=rom_%s[" x x; load a; fprintf fd "&((1<<%d)-1)];" ads)
    | Eram (ads, _, a, wen, wad, wda) ->
      end_loop_ram :=
        !end_loop_ram ^ Printf.(sprintf "if(wda_%s)ram_%s[wad_%s&((1<<%d)-1)]=%s;"
                              x x x ads (print wda)); 
      Printf.(fprintf fd "%s=ram_%s[" x x; load a; fprintf fd "&((1<<%d)-1)];" ads;
              fprintf fd "wda_%s=" x; load wen; fprintf fd ";wad_%s=" x;
              load wad; fprintf fd ";")
    | Econcat (a, a') ->
      Printf.(fprintf fd "%s=(" x; load a; fprintf fd "<<%d" (size a');
              fprintf fd ")|("; load a'; fprintf fd "&((1<<%d)-1));" (size a'))
    | Eselect (i, a) ->
      Printf.(fprintf fd "%s=((" x; load a; fprintf fd ")>>%d)&1;" (size a - i - 1))
    | Eslice (i, j, a) ->
      Printf.(fprintf fd "%s=((" x; load a; fprintf fd ")>>%d)&((1<<%d)-1);" (size a - j - 1) (j - i + 1))

  in
  List.iter emit_equ p.p_eqs;
  let out x =
    Printf.fprintf fd "printf(\"%s: %%ld\\n\", %s &((1 << %d) - 1));" x x (size (Avar x))
  in
  List.iter out p.p_outputs;
  output_string fd !end_loop_ram;
  output_string fd !end_loop_reg;
  Printf.fprintf fd "sleep(1);";
  Printf.fprintf fd "}}"
  
let _ = Arg.parse [] compile ""
