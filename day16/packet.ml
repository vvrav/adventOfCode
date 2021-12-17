type packet =
  | Literal of int * int
  | Operator of int * int * packet list

let get_option s = function
  | Some x -> x
  | None -> failwith s

let bin_of_hex = function
  |  '0' -> "0000"
  |  '1' -> "0001"
  |  '2' -> "0010"
  |  '3' -> "0011"
  |  '4' -> "0100"
  |  '5' -> "0101"
  |  '6' -> "0110"
  |  '7' -> "0111"
  |  '8' -> "1000"
  |  '9' -> "1001"
  |  'A' -> "1010"
  |  'B' -> "1011"
  |  'C' -> "1100"
  |  'D' -> "1101"
  |  'E' -> "1110"
  |  'F' -> "1111"
  | c -> raise (Invalid_argument (Printf.sprintf "%c" c))


let packet_of_string s =
  let sb = ref "" in
  String.iter 
    (fun c -> sb := !sb ^ (bin_of_hex c))
    s;
  !sb

  let read_file fn =
  let ic = open_in fn in
  let s = input_line ic in
  close_in ic;
  packet_of_string s

let int_of_binstring name s =
  get_option name (int_of_string_opt ("0b" ^ s))

let rec read_packet s =
  let sv = String.sub s 0 3 in
  let version = int_of_binstring "version" sv in
  let sid = String.sub s 3 3 in
  let id = int_of_binstring "id" sid in
  let rest = String.sub s 6 (String.length s -6) in
  if id = 4 then
    let value = ref "" in
    let i = ref 0 in
    while rest.[!i*5] = '1' do
      value := !value ^ (String.sub rest (!i*5+1) 4);
      incr i
    done;
    value := !value ^ (String.sub rest (!i*5+1) 4);
    let nbread = !i * 5 + 5 in
    let rest = String.sub rest nbread (String.length rest - nbread) in 
    Literal (version, int_of_binstring "literal value" !value), rest
  else
    match rest.[0] with
    | '0' ->
      let len = int_of_binstring "packet length" (String.sub rest 1 15) in
      let sub = ref (String.sub rest 16 len) in
      let rest = String.sub rest (16+len) (String.length rest - 16 - len) in
      let ops = ref [] in
      while String.length !sub > 0 do
        let op, r = read_packet !sub in
        ops := op :: !ops;
        sub := r
      done;
      Operator (version, id, List.rev !ops),rest
    | '1' ->
      let len = int_of_binstring "packet length" (String.sub rest 1 11) in
      let ops = ref [] in
      let rest = ref (String.sub rest 12 (String.length rest -12)) in
      for i = 0 to len -1 do
        let op, r = read_packet !rest in
        ops := op :: !ops;
        rest := r
      done;
      Operator (version, id, List.rev !ops), !rest
    | c ->
      failwith (Printf.sprintf "Unknown len type '%c'" c)

let rec sum_v = function
  | Literal(v, _) -> v
  | Operator(v, _, subs) ->
    let sums = List.map sum_v subs in
    List.fold_left (+) v sums

let rec process = function
  | Literal(_, value) -> value
  | Operator(_, op_type, subs) ->
    begin
      let subs = List.map process subs in
      match op_type with
      | 0 -> List.fold_left (+) 0 subs
      | 1 -> List.fold_left ( * ) 1 subs
      | 2 -> List.fold_left min max_int subs
      | 3 -> List.fold_left max min_int subs
      | 5 -> let x1 = List.hd subs and x2 = List.nth subs 1 in
        if x1 > x2 then 1 else 0
      | 6 -> let x1 = List.hd subs and x2 = List.nth subs 1 in
        if x1 < x2 then 1 else 0
      | 7 -> let x1 = List.hd subs and x2 = List.nth subs 1 in
        if x1 = x2 then 1 else 0
      | n -> failwith (Printf.sprintf "unsupported operator: %d" n)
    end
  
let run fn =
  let f = read_file fn in
  let p, r = read_packet f in
  Printf.printf "Versions sum: %d\n" (sum_v p);
  process p, r

let test s = let p, _ = read_packet (packet_of_string s) in process p