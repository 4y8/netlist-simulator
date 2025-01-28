open Netlist_ast
open Printf

let read_rom = ref ""
let clock_mode = ref false
let static_rom = ref false
let fast_mode = ref false

let read_rom_file x ads ws =
  if x <> "tick" then
    let fd = open_in ("rom/" ^ x) in
    let s = input_line fd in
    let n = String.length s in
    assert ((1 lsl ads) * ws >= n && n mod ws = 0);
    let out = ref "{" in
    for i = 0 to n / ws - 1 do
      out := !out ^ "0b";
      for j = 0 to ws - 1 do
        assert (s.[i * ws + j] = '0' || s.[i * ws + j] = '1');
        out := !out ^ sprintf "%c" s.[i * ws + j]
      done;
      out := !out ^ "ull,";
    done;
    out := !out ^ "}";
    close_in fd; !out
  else "{0}"

let build_mem fd p =
  let treat (x, e) = fprintf fd "uint64_t nl_%s=0;" x; match e with
      Eram (ads, ws, _, _, _ ,_) ->
      fprintf fd "uint64_t ram_%s[%d]={0ull};uint64_t wad_%s=0;uint64_t wda_%s=0ull;"
        x (1 lsl ads) x x
    | Erom (ads, ws, _) ->
      fprintf fd "uint64_t rom_%s[%d]=%s;" x (1 lsl ads)
        (if !static_rom then read_rom_file x ads ws else "{0}");
      if not !static_rom && x <> "tick" then
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
#include <time.h>\n#include <unistd.h>\n#include <string.h>
#include <inttypes.h>\n";
  build_mem fd p.p_eqs;
  List.iter (fprintf fd "uint64_t nl_%s;") p.p_inputs;
  let clock_prelude =
    if !clock_mode then
      "time_t rtime = time(NULL);struct tm *ptm = localtime(&rtime);
ram_date[0]=ptm->tm_year+1900;ram_date[1]=ptm->tm_mon+1;ram_date[2]=ptm->tm_mday;
ram_date[3]=ptm->tm_hour;ram_date[4]=ptm->tm_min;ram_date[5]=ptm->tm_sec;"
    else ""
  in
  fprintf fd
    "int main(int argc, char **argv){char buf_[65];FILE *f_;int c_;int inf_=1;
unsigned long long int n_;
%s;if(argc==2){n_=strtoull(argv[1],NULL,10);inf_=0;}%s
for(int i_=0;inf_||i_<n_;++i_){printf(\"cycle : %%d\\n\", i_);"
    !read_rom clock_prelude;
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
nl_%s=strtoul(buf_,NULL,2);" x n n x
  in
  List.iter read_input p.p_inputs;
  let print = let string_of_bool b = if b then "1" else "0" in function
      Avar s -> "nl_" ^  s
    | Aconst (VBit b) -> string_of_bool b
    | Aconst (VBitArray a) ->
      "0b" ^ (Array.fold_right (^) (Array.map string_of_bool a) "")
  in
  let end_loop_ram = ref "" in
  let end_loop_reg = ref "" in
  let mask_of_one n =
    "0b" ^ (String.make n '1') ^ "ull"
  in
  let mask a =
    sprintf "(%s&%s)" (print a) (mask_of_one @@ size a)
  in
  let emit_equ (x, e) = match e with
    | Earg a -> fprintf fd "nl_%s=%s;" x (print a)
    | Ereg y ->
      end_loop_ram := sprintf "tmp_%s_=nl_%s;%s" x y !end_loop_ram;
      end_loop_reg := sprintf "nl_%s=tmp_%s_;%s" x x !end_loop_reg
    | Enot a -> fprintf fd "nl_%s=(~%s)&%s;" x (print a) (mask_of_one @@ size a)
    | Ebinop (Or, a, a') -> fprintf fd "nl_%s=%s|%s;" x (print a) (print a')
    | Ebinop (And, a, a') -> fprintf fd "nl_%s=%s&%s;" x (print a) (print a')
    | Ebinop (Xor, a, a') -> fprintf fd "nl_%s=%s^%s;" x (print a) (print a')
    | Ebinop (Nand, a, a') ->
      fprintf fd "nl_%s=~(%s&%s);" x (print a) (print a')
    | Emux (c, f, t) ->
      fprintf fd "nl_%s=((%s)&1ull)?(%s):(%s);" x (print c) (print t) (print f)
    | Erom (ads, ws, a) ->
      assert (ws = var_size x);
      fprintf fd "nl_%s=rom_%s[%s&%s];" x x (mask a) (mask_of_one ws)
    | Eram (ads, ws, a, wen, wad, wda) ->
      assert (ws = var_size x);
      end_loop_ram := sprintf "%sif(wda_%s&1)ram_%s[wad_%s&%s]=%s;"
          !end_loop_ram x x x (mask_of_one ads) (print wda);
      fprintf fd "nl_%s=ram_%s[%s]&%s;" x x (mask a) (mask_of_one ws);
      fprintf fd "wda_%s=%s;wad_%s=%s;" x (print wen) x (print wad)
    | Econcat (a, a') ->
      fprintf fd "nl_%s=(%s<<%d)|(%s);" x (print a') (size a) (mask a)
    | Eselect (i, a) ->
      fprintf fd "nl_%s=((%s)>>%d)&1;" x (print a) i
    | Eslice (i, j, a) ->
      fprintf fd "nl_%s=((%s)>>%d)&%s;" x (print a) i
        (mask_of_one @@ j - i + 1)
  in
  List.iter emit_equ p.p_eqs;
  let out x =
    fprintf fd "printf(\"%s: \");" x;
    fprintf fd "for(int k_=%d;k_>=0;--k_)printf(\"%%\" PRIu64 \"\", (nl_%s>>k_)&1);
puts(\"\");" (var_size x - 1) x
  in
  output_string fd !end_loop_ram;
  output_string fd !end_loop_reg;
  List.iter out p.p_outputs;
  if !clock_mode then 
    fprintf fd "rom_tick[0]=%s%%2;" (if !fast_mode then "i_" else "time(NULL)");
  fprintf fd "}}"

let _ =
  Arg.parse [("-s", Set static_rom, "Set static mode");
             ("-c", Set clock_mode, "Enable options dedicated to the clock");
             ("-f", Set fast_mode, "Set competition mode (seconds aren't real seconds)")]
    compile "netlist_simulator [-s] <file>"
