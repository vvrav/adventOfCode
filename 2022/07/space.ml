type fs =
| Dir of string list ref
| File of int

module StringListMap = Map.Make(struct type t = string list let compare = compare end)

let string_of_path =
  List.fold_left
    (fun acc e -> e ^ "/" ^ acc)
    ""

let read_line (path, fs) l =
  try
    match String.split_on_char ' ' l with
    | "$" :: "cd" :: "/" :: [] -> ([], fs)
    | "$" :: "cd" :: ".." :: [] ->
      let parent = match path with hd :: tl -> tl in
      parent, fs
    | "$" :: "cd" :: label :: [] ->
      label :: path, fs
    | "$" :: "ls" :: [] ->
      path, fs
    | "dir" :: label :: [] ->
      let nfs = StringListMap.add (label :: path) (Dir (ref [])) fs in
      let siblings = match StringListMap.find path nfs with Dir s -> s in
        siblings := label :: !siblings;
        (path, nfs)
    | size :: label :: [] ->
      let size = int_of_string size in
      let siblings = match StringListMap.find path fs with Dir s -> s in
      siblings := label :: !siblings;
      path, StringListMap.add (label :: path) (File size) fs
    | _ -> failwith ("oopsie: " ^ l)
    with
    | Match_failure _ -> failwith ("Failed to read: " ^ l ^ " (path: " ^ (string_of_path path) ^ ")")

let read_all sl =
  let _, fs = List.fold_left
    read_line
    ([], StringListMap.singleton [] (Dir (ref [])))
    sl
  in
  fs

let get_dir_sizes fs =
  let rec aux sm path =
    match StringListMap.find_opt path sm with
    | Some s -> sm, s
    | None ->
        let childs = match StringListMap.find path fs with Dir cr -> !cr in
        let nsm, size = List.fold_left
          (fun (sm, size) item -> match StringListMap.find (item :: path) fs with
          | File n -> (sm, size + n)
          | Dir _ -> let sm, s = aux sm (item :: path) in (sm, size + s))
          (sm, 0)
          childs
        in
        StringListMap.add path size nsm, size
    in
    let dir_sizes, total_size = aux StringListMap.empty [] in
    dir_sizes

let get_result1 sizez =
  StringListMap.fold
    (fun k v acc -> if v > 100000 then acc else v + acc)
    sizez
    0

let get_result2 sizez =
  let total = 70000000 in
  let used = StringListMap.find [] sizez in
  let unused = total - used in
  let needed = 30000000 - unused in
  StringListMap.fold
    (fun _ v acc ->
      if v < needed || v > acc then acc
      else v)
    sizez
    used
