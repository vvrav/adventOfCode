module CharSet = Set.Make(struct type t = char let compare = compare end)

let compartments_of_line l =
  let s = (String.length l) / 2 in
  String.sub l 0 s, String.sub l s s

let set_of_string c =
  String.fold_left
    (fun set c -> CharSet.add c set)
    CharSet.empty
    c

let get_error (c1, c2) =
  let inter = CharSet.inter
    (set_of_string c1)
    (set_of_string c2)
  in
  if CharSet.cardinal inter <> 1 then failwith "oopsie"
  else CharSet.choose inter

let score_of_char c =
  if c >= 'a' && c <= 'z' then Char.code c - Char.code 'a' + 1
  else if c >= 'A' && c <= 'Z' then Char.code c - Char.code 'A' + 27
  else failwith "oopsie"


let find_badges l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | e1 :: e2 :: e3 :: tl ->
      let bs = CharSet.inter
      (CharSet.inter (set_of_string e1) (set_of_string e2))
      (set_of_string e3) in
      let b = if CharSet.cardinal bs = 1 then CharSet.choose bs else failwith "oopsie" in
      aux (b :: acc) tl
    | _ -> failwith "oopsie"
  in
  aux [] l

