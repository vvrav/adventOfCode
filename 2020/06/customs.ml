module CharSet = Set.Make(struct type t = char let compare = compare end)

let charset_of_string =
  String.fold_left (fun acc c -> CharSet.add c acc) CharSet.empty

let read_file fn =
  let ic = open_in fn in
  let rec aux ((hd :: tl) as acc)  =
    match input_line ic with
    | "" -> aux (CharSet.empty :: acc)
    | s ->
      let nhd =
        CharSet.union hd (charset_of_string s)
      in
        aux (nhd :: tl)
    | exception End_of_file -> acc
  in
  let sets = aux [CharSet.empty] in
  close_in ic;
  sets

let process fn =
  let sets = read_file fn in
  List.fold_left (fun acc s -> acc + (CharSet.cardinal s)) 0 sets


let read_file2 fn =
  let ic = open_in fn in
  let rec aux acc  =
    match (input_line ic, acc) with
    | "", acc -> aux (None :: acc)
    | s, None :: tl ->
      let nhd = charset_of_string s
      in aux ((Some nhd) :: tl)
    | s, (Some hd) :: tl ->
      let nhd = CharSet.inter hd (charset_of_string s) in
      aux ((Some nhd) :: tl)
    | s, _ -> failwith s
    | exception End_of_file -> acc
  in
  let sets = aux [None] in
  close_in ic;
  List.map (fun (Some s) -> s) sets

let process2 fn =
  let sets = read_file2 fn in
  List.fold_left (fun acc s -> acc + (CharSet.cardinal s)) 0 sets
    