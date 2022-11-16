module StringMap = Map.Make(struct type t = string let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = compare end)

let add_path map a b =
  let m1 = StringMap.update a (function None -> Some (StringSet.singleton b) | Some x -> Some (StringSet.add b x)) map in
  StringMap.update b (function None -> Some (StringSet.singleton a) | Some x -> Some (StringSet.add a x)) m1

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

let map_of_input l =
  let one_line map s =
    match String.split_on_char '-' s with
    | [o; d] -> add_path map o d
    | _ -> failwith "pouet"
    in
  List.fold_left one_line StringMap.empty l

let print_map map =
  StringMap.iter
    (fun k l ->
      print_string k;
      print_string " -> ";
      List.iter (fun s -> print_string s; print_string "; ") l;
      print_newline ())
    map

let get_paths1 map =
  let paths = ref [] in
  let rec aux path seen pos =
    if pos = "end" then
      paths := (List.rev path) :: !paths
    else
      let seen = if String.uppercase_ascii pos = pos then
        seen
      else
        StringSet.add pos seen in
      StringSet.iter
        (fun pos ->
          if not (StringSet.mem pos seen) then aux (pos :: path) seen pos)
        (StringMap.find pos map)
  in
  aux ["start"] StringSet.empty "start";
  !paths

let run1 fn =
  let f = read_file fn in
  let map = map_of_input f in
  List.length (get_paths1 map)

let get_paths2 map =
  let paths = ref [] in
  let rec aux path seen pos doubled =
    if pos = "end" then
      paths := (List.rev path) :: !paths
    else
      let seen = if String.uppercase_ascii pos = pos then
        seen
      else
        StringSet.add pos seen in
      StringSet.iter
        (fun pos ->
          if not (StringSet.mem pos seen) then
            aux (pos :: path) seen pos doubled
          else if not doubled && pos <> "start" then
            aux (pos :: path) seen pos true)
        (StringMap.find pos map)
  in
  aux ["start"] StringSet.empty "start" false;
  !paths

  let run2 fn =
    let f = read_file fn in
    let map = map_of_input f in
    List.length (get_paths2 map)
