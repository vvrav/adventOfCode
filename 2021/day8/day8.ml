module IntSet = Set.Make(struct type t = int let compare = compare end)
module CharSet = Set.Make(struct type t = char let compare = compare end)
module PatternMap = Map.Make(struct type t = CharSet.t let compare = CharSet.compare end)

let list_of_pattern cs =
  CharSet.fold (List.cons) cs []

let size_to_digits = [|
  [];
  [];
  [1];
  [7];
  [4];
  [2; 3; 5];
  [0; 6; 9];
  [8]
|]

let digits_to_seg = Array.of_list (
  List.map
    IntSet.of_list
    [
      [1; 2; 3; 5; 6; 7];
      [3; 6];
      [1; 3; 4; 5; 7];
      [1; 3; 4; 6; 7];
      [2; 3; 4; 6];
      [1; 2; 4; 6; 7];
      [1; 2; 4; 5; 6; 7];
      [1; 3; 6];
      [1; 2; 3; 4; 5; 6; 7];
      [1; 2; 3; 4; 6; 7];
    ]
  )

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  ls

let read_line l =
  match String.split_on_char '|' l with
  | [p; d] ->
      let pl = String.split_on_char ' ' (String.trim p) in
      let dl = String.split_on_char ' ' (String.trim d) in
      pl, dl
  | _ -> failwith "failed to read line"

let run1 fn =
  let f = read_file fn in
  let digits = List.flatten (List.map (fun l -> snd (read_line l)) f) in
  let count = ref 0 in
  List.iter (fun x -> if List.length size_to_digits.(String.length x) = 1 then (print_endline x; incr count)) digits;
  !count

let pattern_of_string s =
  let cl = ref [] in
  String.iter (fun c -> cl := c :: !cl) s;
  CharSet.of_list !cl

let get_patterns pl =
  let a = Array.make 10 CharSet.empty in
  let l5 = ref [] in
  let l6 = ref [] in
  List.iter
    (fun p ->
      let ps = pattern_of_string p in
      match CharSet.cardinal ps with
      | 2 -> a.(1) <- ps
      | 3 -> a.(7) <- ps
      | 4 -> a.(4) <- ps
      | 5 -> l5 := ps :: !l5
      | 6 -> l6 := ps :: !l6
      | 7 -> a.(8) <- ps
      | _ -> failwith "incorrect!"  
      )
    pl;
  List.iter
    (fun ps ->
      if CharSet.cardinal (CharSet.inter ps a.(1)) = 2 then
        a.(3) <- ps
      else if CharSet.cardinal (CharSet.inter ps a.(4)) = 2 then
        a.(2) <- ps
      else
        a.(5) <- ps)
    !l5;
  List.iter
    (fun ps ->
      if CharSet.cardinal (CharSet.inter ps a.(1)) = 1 then
        a.(6) <- ps
      else if CharSet.cardinal (CharSet.inter ps a.(4)) = 4 then
        a.(9) <- ps
      else
        a.(0) <- ps)
    !l6;
  let map = ref PatternMap.empty in
  Array.iteri (fun i p -> map := PatternMap.add p i !map) a;
  !map

let get_number pm d =
  let l = List.map (fun i -> PatternMap.find i pm) d in
  List.fold_left (fun acc i -> 10 * acc + i) 0 l

let run2 fn =
  let f = read_file fn in
  let l = List.map read_line f in
  let pl = List.map (fun (p, d) -> get_patterns p, List.map pattern_of_string d) l in
  let nl = List.map (fun (pm, d) -> get_number pm d) pl in
  List.fold_left (+) 0 nl
