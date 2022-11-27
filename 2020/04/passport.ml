module StringSet = Set.Make(struct type t = string let compare = compare end)

let required_keys =
  List.fold_right
    StringSet.add
    ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]
    StringSet.empty

let eye_colors =
  List.fold_right
    StringSet.add
    ["amb";"blu";"brn";"gry";"grn";"hzl";"oth"]
    StringSet.empty

let string_of_pair (x,y) = x ^ ":" ^ y

let string_of_id l =
  "{\n" ^
  List.fold_left
    (^)
    ""
    (List.map (fun p -> "  " ^ (string_of_pair p) ^ "\n") l) ^
  "}\n"

let read_file fn =
  let ic = open_in fn in
  let rec aux (hd :: acc) =
    match input_line ic with
    | "" -> aux ([] :: hd :: acc)
    | s ->
      let items = String.split_on_char ' ' s in
      let pairs = List.map 
        (fun s -> Scanf.sscanf s "%s@:%s" (fun a b -> a, b))
        items
      in
      aux ((pairs @ hd) :: acc)
    | exception End_of_file -> hd :: acc
  in
  let ls = aux [[]] in
  close_in ic;
  ls

let keyset_of_id id =
  List.fold_left
    (fun acc (key, _) -> StringSet.add key acc)
    StringSet.empty
    id

let is_valid id =
  let ks = keyset_of_id id in
  StringSet.subset required_keys ks

let process f l =
  List.fold_left
    (fun acc i -> if f i then acc+1 else acc)
    0
    l

let is_hcl c =
  String.length c = 7 &&
  c.[0] = '#' &&
  try
    String.iter
      (fun c -> if not ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) then raise Exit)
      (String.sub c 1 6);
    true
  with Exit -> false

let is_id n =
  String.length n = 9 &&
  try
    String.iter (fun c -> if not (c >= '0' && c <= '9') then raise Exit) n;
    true
  with Exit -> false

let is_valid_pair (key, value) =
  match key with
  | "byr" ->
    let year = int_of_string value in
    year >= 1920 && year <= 2002
  | "iyr" ->
    let year = int_of_string value in
    year >= 2010 && year <= 2020
  | "eyr" ->
    let year = int_of_string value in
    year >= 2020 && year <= 2030
  | "hgt" ->
    Scanf.sscanf
      value
      "%d%s"
      (fun v u -> match u with
        | "cm" -> v >= 150 && v <= 193
        | "in" -> v >= 59 && v <= 76
        | _ -> false)
  | "hcl" -> is_hcl value
  | "ecl" -> StringSet.mem value eye_colors
  | "pid" -> is_id value
  | "cid" -> true
  | _ -> failwith "oops"

let is_valid2 id =
  is_valid id &&
  List.for_all is_valid_pair id
