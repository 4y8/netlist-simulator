open Netlist_ast

let number_steps = ref (-1)
let print_only = ref false
let rom = [||]

let l_op op = function
    VBit b, VBit b' -> VBit (op b b')
  | VBitArray a, VBitArray a' ->
    VBitArray (Array.map2 op a a')
  | _, _ -> failwith "erreur de type"

let array_to_int n a =
  Array.mapi (fun i b -> (1 lsl (n - 1 - i)) * (if b then 1 else 0)) a |>
  Array.fold_left (+) 0

let build_ram =
  let add_var env (x, e) = match e with
      Eram (ads, ws, _, _, _, _) ->
      Env.add x (Array.init (1 lsl ads)
                   (fun _ -> Array.make ws false))
        env
    | _ -> env
  in List.fold_left add_var Env.empty

let print_type = function
    TBit -> ""
  | TBitArray n -> Printf.sprintf " : %d" n

let read_input x ty =
  Printf.printf "%s%s ? " x (print_type ty);
  let s = read_line () in
  match ty with
    TBit ->
    if String.length s <> 1 then
      failwith
        (Printf.sprintf "size mismatch: expected a bit (size 1), got %s" s)
    else if s <> "0" && s <> "1" then
      failwith
        (Printf.sprintf "wrong format: expected 0 or 1, got %s" s)
    else
      (VBit (s = "1"))
  | TBitArray n ->
    if String.length s <> n then
      failwith @@
      Printf.sprintf "size mismatch: expected a bus of size %d, got %s" n s
    else
      let a = Array.make n false in
      let treat_char i c =
        if c <> '0' && c <> '1' then
          failwith @@
          (Printf.sprintf "wrong format: expected 0 or 1, got %c" c)
        else a.(i) <- c = '1'
      in
      String.iteri treat_char s;
      VBitArray a

let simulator program number_steps =
  let ram = build_ram program.p_eqs in
  let reg = Hashtbl.create 4096 in
  let load_arg = function
      Avar i -> Hashtbl.find reg i
    | Aconst v -> v
  in
  let load_arr n a = match load_arg a with
      VBit _ -> failwith "size mismatch"
    | VBitArray a -> assert (Array.length a = n); a
  in
  let load_arrno a = match load_arg a with
      VBit _ -> failwith "size mismatch"
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
    | Ebinop (o, a, a') -> begin
        let v = load_arg a in
        let v' = load_arg a' in
        match o with
          Or -> l_op (||) (v, v')
        | Xor -> l_op (<>) (v, v')
        | And -> l_op (&&) (v, v')
        | Nand -> l_op (fun b b' -> not (b && b')) (v, v')
      end
    | Emux (c, t, f) ->
      if load_arg c = VBit true then
        load_arg t
      else
        load_arg f
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
      | _ -> failwith "called select on a bit instead of a bus"
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
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
