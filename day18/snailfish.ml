(* #load "str.cma";; *)
let top_pair_r = Str.regexp "\\[[0-9]+,[0-9]+\\]"
let int_r = Str.regexp "[0-9]+"

exception Explode of int * string
exception Split of int * string

let search_last r s =
  let i = ref 0 in
  let res = ref None in
  begin try
    while !i < String.length s do
      let pos = Str.search_forward r s !i in
      res := Some (pos, Str.matched_string s);
      i := Str.match_end ()
    done;
  with Not_found -> () end;
  match !res with
  | None -> raise Not_found
  | Some (pos, m) -> pos, m

let debug_mode = ref false
let debug s = if !debug_mode then print_endline s

let check_explodable n =
  let level = ref 0 in
  String.iteri
    (fun i c -> match c with
    | '[' -> incr level;
      if !level > 4 && Str.string_match top_pair_r n i then
        raise (Explode (i, Str.matched_string n))
    | ']' -> decr level
    | _ -> ())
    n

let explode n pos pair =
  let a, b = Scanf.sscanf pair "[%d,%d]" (fun a b -> a, b) in
  let n_start = Str.string_before n pos in
  let n_end = Str.string_after n (pos + String.length pair) in
  let n_start = try
    let prec_pos, prec_match = search_last int_r n_start in
    let prec_end = prec_pos + String.length prec_match in
    let prec_num = int_of_string prec_match in
    (Str.string_before n_start prec_pos ^ string_of_int (prec_num + a) ^ Str.string_after n_start prec_end)
  with Not_found -> n_start
  in
  let n_end = try
    let next_pos = Str.search_forward int_r n_end 0 in
    let next_end = Str.match_end () in
    let next_num = int_of_string (Str.matched_string n_end) in
    (Str.string_before n_end next_pos ^ string_of_int (next_num + b) ^ Str.string_after n_end next_end)
  with Not_found -> n_end
  in
  n_start ^ "0" ^ n_end

let check_split n =
  try
    let pos = Str.search_forward (Str.regexp "[0-9][0-9]+") n 0 in
    raise (Split (pos, Str.matched_string n))
  with
    Not_found -> ()

let split n pos s =
  let num = int_of_string s in
  let mid = num / 2 in
  let npair = Printf.sprintf "[%d,%d]" mid (num-mid) in
  Str.replace_first (Str.regexp_string s) npair n

let reduce n =
  let n = ref n in
  let cont = ref true in
  while !cont do
    try
      debug !n;
      check_explodable !n;
      check_split !n;
      cont := false
    with
    | Explode (pos, pair) -> n := explode !n pos pair
    | Split (pos, s) -> n := split !n pos s
  done;
  !n

let sum a b =
  if a = "" then
    b
  else if b = "" then
    a
  else
    let nnum = Printf.sprintf "[%s,%s]" a b in
    let r = reduce nnum in
    debug (Printf.sprintf "%s -> %s\n" nnum r);
    r

let tests =[
  ["[1,1]"; "[2,2]"; "[3,3]"; "[4,4]"], "[[[[1,1],[2,2]],[3,3]],[4,4]]";
  ["[1,1]"; "[2,2]"; "[3,3]"; "[4,4]"; "[5,5]"], "[[[[3,0],[5,3]],[4,4]],[5,5]]";
  ["[1,1]"; "[2,2]"; "[3,3]"; "[4,4]"; "[5,5]"; "[6,6]"], "[[[[5,0],[7,4]],[5,5]],[6,6]]"
] 

let () = List.iter
  (fun (input, expected) -> assert (List.fold_left sum "" input = expected))
  tests

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  List.rev ls

let get_magnitude n =
  let n = ref n in
  try while true do
    let _ = Str.search_forward top_pair_r !n 0 in
    let pair = Str.matched_string !n in
    let magnitude = Scanf.sscanf pair "[%d,%d]" (fun a b -> 3*a + 2*b) in
    n := Str.replace_first (Str.regexp_string pair) (string_of_int magnitude) !n
  done; !n
  with
    Not_found -> !n

let run1 fn =
  let content = read_file fn in
  let result = List.fold_left sum "" content in
  get_magnitude result

let run2 fn =
  let magnitudes = ref [] in
  let content = read_file fn in
  List.iter
    (fun a ->
      List.iter
        (fun b -> if a <> b then
          let m = int_of_string (get_magnitude (sum a b)) in
           magnitudes := m :: !magnitudes)
        content)
    content;
  List.fold_left max min_int !magnitudes
