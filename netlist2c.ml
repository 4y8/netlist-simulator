open Netlist_ast
open Printf

let read_rom = ref ""

let build_mem fd p =
  let treat (x, e) = fprintf fd "uint64_t %s=0;" x; match e with
      Eram (ads, ws, _, _, _ ,_) ->
      fprintf fd "uint64_t ram_%s[%d]={0};uint64_t wad_%s=0;uint64_t wda_%s=0;"
        x (1 lsl ads) x x
    | Erom (ads, ws, _) ->
      fprintf fd "uint64_t rom_%s[%d]={0};" x (1 lsl ads);
      read_rom :=
        !read_rom ^ sprintf
          "f_ = fopen(\"rom/%s\",\"r\"); 
for (int j_=0;(c_=getc(f_))!=EOF;++j_)
rom_%s[j_/%d]|=(c_=='1')<<(%d-(j_ %% %d)-1);fclose(f_);" x x ws ws ws
    | Ereg _ -> fprintf fd "uint64_t tmp_%s_=0;" x
    | _ -> ()
  in
  List.iter treat p

let compile f =
  let p = Netlist.read_file f in
  let p = Scheduler.schedule p in
  let fd = open_out "out.c" in
  fprintf fd "#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>
#include <unistd.h>\n#include <string.h>\n";
  build_mem fd p.p_eqs;
  List.iter (fprintf fd "uint64_t %s;") p.p_inputs;
  fprintf fd
    "int main(){char buf_[65];FILE *f_;int c_; %s 
for (int i_=0;1;++i_){printf(\"cycle : %%d\\n\", i_);" !read_rom;
  let var_size x = match Env.find x p.p_vars with
      TBit -> 1
    | TBitArray n -> n
  in
  let size = function
      Avar x -> var_size x
    | Aconst (VBit _) -> 1
    | Aconst (VBitArray a) -> Array.length a
  in
  let read_input x =
    let n = var_size x in
    fprintf fd "printf(\"%s:%d? \");fgets(buf_,65,stdin);
if(strlen(buf_) != %d){fprintf(stderr, \"size mismatch\");exit(1);}
%s=strtoul(buf_,NULL,2);" x n n x
  in
  List.iter read_input p.p_inputs;
  let print = let string_of_bool b = if b then "1" else "0" in function
      Avar s -> s
    | Aconst (VBit b) -> string_of_bool b
    | Aconst (VBitArray a) ->
      "0b" ^ (Array.fold_right (^) (Array.map string_of_bool a) "")
  in
  let end_loop_ram = ref "" in
  let end_loop_reg = ref "" in
  let mask a =
    sprintf "(%s&((1<<%d)-1))" (print a) (size a)
  in
  let emit_equ (x, e) = match e with
      Earg a -> fprintf fd "%s=%s;" x (print a)
    | Ereg y ->
      end_loop_ram := sprintf "tmp_%s_=%s;%s" x y !end_loop_ram;
      end_loop_reg := sprintf "%s=tmp_%s_;%s" x x !end_loop_reg
    | Enot a -> fprintf fd "%s=~%s;" x (print a)
    | Ebinop (Or, a, a') -> fprintf fd "%s=%s|%s;" x (print a) (print a')
    | Ebinop (And, a, a') -> fprintf fd "%s=%s&%s;" x (print a) (print a')
    | Ebinop (Xor, a, a') -> fprintf fd "%s=%s^%s;" x (print a) (print a')
    | Ebinop (Nand, a, a') -> fprintf fd "%s=~(%s&%s);" x (print a) (print a')
    | Emux (c, f, t) ->
      fprintf fd "%s=(%s)?(%s):(%s);" x (print c) (print t) (print f)
    | Erom (ads, ws, a) ->
      assert (ws = var_size x);
      fprintf fd "%s=rom_%s[%s&((1<<%d)-1)];" x x (mask a) ws
    | Eram (ads, ws, a, wen, wad, wda) ->
      assert (ws = var_size x);
      end_loop_ram :=
        !end_loop_ram ^ (sprintf "if(wda_%s)ram_%s[wad_%s&((1<<%d)-1)]=%s;"
                              x x x ads (print wda)); 
      fprintf fd "%s=ram_%s[%s]&((1<<%d)-1);" x x (mask a) ws;
      fprintf fd "wda_%s=%s;wad_%s=%s;" x (print wen) x (print wad)
    | Econcat (a, a') ->
      fprintf fd "%s=(%s<<%d)|(%s);" x (print a) (size a') (mask a')
    | Eselect (i, a) ->
      fprintf fd "%s=((%s)>>%d)&1;" x (print a) (size a - i - 1)
    | Eslice (i, j, a) ->
      fprintf fd "%s=((%s)>>%d)&((1<<%d)-1);" x (print a) (size a - j - 1)
        (j - i + 1)
  in
  List.iter emit_equ p.p_eqs;
  let out x =
    fprintf fd "printf(\"%s: );" x;
    fprintf fd "for(int k_=%d;k_>=0;--k)printf(\"%%ld\", (%s>>_k)&1); 
puts(\"\");" (var_size x - 1) x
  in
  List.iter out p.p_outputs;
  output_string fd !end_loop_ram;
  output_string fd !end_loop_reg;
  fprintf fd "}}"
  
let _ = Arg.parse [] compile ""
