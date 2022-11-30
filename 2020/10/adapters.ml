module IntMap = Map.Make(struct type t = int let compare = compare end)

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux ((int_of_string s) :: acc)
    | exception End_of_file -> List.rev acc
  in
  let s = aux [] in
  close_in ic;
  s

let process l =
  let sl = List.sort compare l in
  let a = Array.make 4 0 in
  ignore(
    List.fold_left
    (fun prev cur ->
      a.(cur-prev) <- a.(cur-prev) + 1;
      cur)
    0
    sl);
  a.(3) <- a.(3) + 1;
  a.(1) * a.(3)

let find_0 key map =
  try
    IntMap.find key map
  with
    Not_found -> 0

let process2 l =
  let sl = List.sort compare l in
  let map = IntMap.singleton 0 1 in
  let filled_map =
    List.fold_left
      (fun map v -> 
        let count = find_0 (v-1) map + find_0 (v-2) map + find_0 (v-3) map in
        IntMap.add v count map)
      map
      sl
    in
    IntMap.max_binding filled_map
