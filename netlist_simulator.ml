open Netlist_ast

let number_steps = ref max_int
let print_only = ref false
let rom_file = ref "rom"

(** [l_op op v1 v2] computes the result of [op] on [v1] and [v2]. *)
let l_op op = function
    VBit b, VBit b' -> VBit (op b b')
  | VBitArray a, VBitArray a' ->
    begin
      try 
        VBitArray (Array.map2 op a a')
      with
        Invalid_argument _ -> failwith "size mismatch: mixed bus sizes with \
                                      logical operations"
    end
  | _, _ -> failwith "type mismatch: mixed bus and bits with logical operations"

let fun_of_op = function
    Or -> (||)
  | And -> (&&)
  | Xor -> (<>)
  | Nand -> fun b1 b2 -> not (b1 && b2)

(** [array_to_int n a] converts the array [a] of [n] bits to an integer in
    big-endian. *)
let array_to_int n a =
  Array.mapi (fun i b -> (1 lsl (n - 1 - i)) * (if b then 1 else 0)) a |>
  Array.fold_left (+) 0

(** [build_ram p] builds a map which associates to each x =RAM(...), an array of
    the right size. *)
let build_ram =
  let add_var env (x, e) = match e with
      Eram (ads, ws, _, _, _, _) ->
      Env.add x (Array.init (1 lsl ads)
                   (fun _ -> Array.make ws false))
        env
    | _ -> env
  in List.fold_left add_var Env.empty

let bool_of_char = function
    '0' -> false
  | '1' -> true
  | c ->
    failwith @@ Printf.sprintf "wrong format: expected 0 or 1, got %c" c

(** [read_input x ty] reads the value of the input [x] of type [ty]. *)
let read_input x ty =
  let print_type = function
    TBit -> ""
  | TBitArray n -> Printf.sprintf " : %d" n
  in
  Printf.printf "%s%s ? " x (print_type ty);
  let s = read_line () in
  match ty with
    TBit ->
    if String.length s <> 1 then
      failwith @@
      Printf.sprintf "size mismatch: expected a bit (size 1), got %s" s
    else
      VBit (bool_of_char s.[0])
  | TBitArray n ->
    if String.length s <> n then
      failwith @@
      Printf.sprintf "size mismatch: expected a bus of size %d, got %s" n s
    else
      VBitArray (Array.init n (fun i -> bool_of_char s.[i]))

let simulator program number_steps =
  let ram = build_ram program.p_eqs in
  let reg = Hashtbl.create 4096 in

  let fd = open_in !rom_file in
  let s = input_line fd in
  let rom = Array.init (String.length s) (fun i -> bool_of_char s.[i]) in
  close_in fd;

  let load_arg = function
      Avar i -> Hashtbl.find reg i
    | Aconst v -> v
  in
  let load_arr n a = match load_arg a with
      VBit _ -> failwith "size mismatch: expected a bus, got a bit"
    | VBitArray a ->
      if Array.length a = n then
        a
      else failwith @@
        Printf.sprintf
          "size mismatch: expected a bus of size %d, got one of size %d"
          n (Array.length a)
  in
  let load_arrno a = match load_arg a with
      VBit _ -> failwith "size mismatch: expected a bus, got a bit"
    | VBitArray a -> a
  in

  let calc (x, e) = match e with
      Earg a ->
      load_arg a
    | Ereg y -> Hashtbl.find reg y
    | Enot a -> begin
        match load_arg a with
          VBit b -> VBit (not b)
        | VBitArray a ->
          VBitArray (Array.map (not) a)
      end
    | Ebinop (o, a, a') ->
      let v = load_arg a in
      let v' = load_arg a' in
      l_op (fun_of_op o) (v, v')
    | Emux (c, t, f) ->
      let v = load_arg c in
      if v = VBit true then
        load_arg t
      else if v = VBit false then
        load_arg f
      else
        failwith "type mismatch: used a multiplexer on a bus, expected a bit"
    | Erom (ss, ws, ad) ->
      let ad = array_to_int ss (load_arr ss ad) in
      VBitArray (Array.sub rom ad ws)
    | Eram (ss, ws, rad, wen, wad, wa) ->
      let ad = array_to_int ss (load_arr ss rad) in
      let lram = Env.find x ram in
      let v = VBitArray (Array.copy lram.(ad)) in
      Hashtbl.replace reg x v;
      (if load_arg wen = VBit true then
         let ad = array_to_int ss (load_arr ss wad) in
         let da = load_arr ws rad in
         Array.blit da 0 lram.(ad) 0 ws); 
      v
    | Econcat (a, a') ->
      let a = load_arrno a in
      let a' = load_arrno a' in
      VBitArray (Array.concat [a; a'])
    | Eslice (i, j, a) ->
      let a = load_arrno a in
      VBitArray (Array.sub a i (j - i + 1))
    | Eselect (i, a) ->
      match load_arg a with
        VBitArray a -> VBit a.(i)
      | _ -> failwith "type mismatch: called select on a bit instead of a bus"
  in
  (* x =RAM(...) is treated aside because calculating the value x should have,
     is not the last thing it does (it can also write) *)
  let exec (x, e) =
    match e with
      Eram (_, _, _, _, _, _) -> let _ = calc (x, e) in ()
    | _ -> Hashtbl.replace reg x (calc (x, e))
  in

  for i = 1 to number_steps do
    Printf.printf "cycle : %d\n" i;
    let read_input x =
      Hashtbl.replace reg x @@ read_input x (Env.find x program.p_vars)
    in
    let out x =
      Printf.printf "%s : " x;
      match Hashtbl.find reg x with
        VBit b ->
        if b then print_endline "1" else print_endline "0"
      | VBitArray a ->
        Array.iter (fun b -> if b then print_int 1 else print_int 0) a;
        print_newline ()
    in
    List.iter read_input program.p_inputs;
    List.iter exec program.p_eqs;
    List.iter out program.p_outputs
  done

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    [ "-n", Arg.Set_int number_steps, "Number of steps to simulate"
    ; "-r", Arg.Set_string rom_file, "Name of the rom file"]
    compile
    ""
;;

main ()
