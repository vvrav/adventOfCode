module CharPairMap = Map.Make(struct type t = char * char let compare = compare end)
module CharMap = Map.Make(struct type t = char let compare = compare end)

let debug = ref false

let get_template s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let print_pairs p =
  CharPairMap.iter
    (fun (c1, c2) c3 -> Printf.printf "%c%c -> %c\n" c1 c2 c3)
    p

let read_file fn =
  let ic = open_in fn in
  let template = get_template (input_line ic) in
  ignore (input_line ic);
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  let pairs = ref CharPairMap.empty in
  List.iter
    (fun s ->
      if String.length s > 0 then
        Scanf.sscanf
          s
          "%c%c -> %c"
          (fun c1 c2 c3 -> pairs := CharPairMap.add (c1, c2) c3 !pairs))
    ls;
  template, !pairs

let one_step t p =
  let rec aux acc s =
    match s with
    | c1 :: c2 :: tail ->
        let c3 = CharPairMap.find (c1, c2) p in
        aux (c3 :: c1 :: acc) (c2 :: tail)
    | c :: [] -> List.rev (c :: acc)
    | _ -> failwith "oops"
  in
  aux [] t

let count t =
  let map = ref CharMap.empty in
  List.iter
    (fun c ->
      map := CharMap.update
        c
        (function None -> Some 1 | Some n -> Some (n+1))
        !map)
    t;
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
  let t, map = read_file fn in
  let t = ref t in
  for i = 0 to steps - 1 do
    t := one_step !t map;
    Printf.printf "step %d\n" i;
    flush stdout;
    if !debug then (List.iter print_char !t; print_newline())
  done;
  let counts = count !t in
  if !debug then print_counts counts;
  let min, max = get_minmax counts in
  max - min

let () =
  let result = run1 "test" 30 in
  print_int result;
  print_newline ()
