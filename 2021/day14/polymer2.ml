module CharPairMap = Map.Make(struct type t = char * char let compare = compare end)
module CharMap = Map.Make(struct type t = char let compare = compare end)

let debug = ref false


let addPair c1 c2 n map =
  map := CharPairMap.update
    (c1, c2)
    (function
      | None -> Some n
      | Some x -> Some (x+n))
    !map



let get_template s =
  let pairs = ref CharPairMap.empty in
  let len = String.length s in
  for i = 0 to len - 2 do
    addPair s.[i] s.[i+1] 1 pairs
  done;
  !pairs, s.[len - 1]

let print_pairs p =
  CharPairMap.iter
    (fun (c1, c2) c3 -> Printf.printf "%c%c -> %c\n" c1 c2 c3)
    p

let read_file fn =
  let ic = open_in fn in
  let pairs, last = get_template (input_line ic) in
  ignore (input_line ic);
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  let rules = ref CharPairMap.empty in
  List.iter
    (fun s ->
      if String.length s > 0 then
        Scanf.sscanf
          s
          "%c%c -> %c"
          (fun c1 c2 c3 -> rules := CharPairMap.add (c1, c2) c3 !rules))
    ls;
  pairs, last, !rules

let one_step pairs rules =
  let npairs = ref CharPairMap.empty in
  CharPairMap.iter
    (fun (c1, c2) n ->
        let nc = CharPairMap.find (c1, c2) rules in
        addPair c1 nc n npairs;
        addPair nc c2 n npairs)
    pairs;
  !npairs

let count pairs last =
  let map = ref (CharMap.singleton last 1) in
  CharPairMap.iter
    (fun (c, _) n ->
      map := CharMap.update
        c
        (function None -> Some n | Some x -> Some (n+x))
        !map)
    pairs;
  !map

let print_counts c =
  CharMap.iter
    (fun k i -> Printf.printf "%c -> %d\n" k i)
    c

let get_minmax counts =
  let _, s = CharMap.choose counts in
  CharMap.fold
    (fun  _ x (min, max) ->
      if x < min then
        (x, max)
      else if x > max then
        (min, x)
      else
        (min, max))
    counts
    (s, s)

let run1 fn steps =
  let pairs, last, rules = read_file fn in
  let pairs = ref pairs in
  for i = 0 to steps - 1 do
    pairs := one_step !pairs rules;
    Printf.printf "step %d\n" i;
    flush stdout;
    (* if !debug then (Array.iter print_char !t; print_newline()) *)
  done;
  let counts = count !pairs last in
  if !debug then print_counts counts;
  let min, max = get_minmax counts in
  max - min

(* let () =
  let result = run1 "test" 30 in
  print_int result;
  print_newline () *)
