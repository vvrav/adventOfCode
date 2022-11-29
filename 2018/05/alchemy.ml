module CharSet = Set.Make(struct type t = char let compare = compare end)

let read_file fn =
  let ic = open_in fn in
  let s = input_line ic in
  close_in ic;
  s

let process s =
  let stack =
    String.fold_left
      (fun acc c ->
        match acc with
        | [] -> c :: acc
        | p :: tl ->
          if Char.lowercase_ascii c = Char.lowercase_ascii p && c <> p
          then tl
          else c :: acc)
      []
      s
  in
  List.length stack

let all_letters =
  String.fold_left
    (fun acc c -> CharSet.add c acc)
    CharSet.empty

let process2 s =
  let dict = all_letters s in
  let aux x =
    let stack =
      String.fold_left
      (fun acc c ->
        let lowc = Char.lowercase_ascii c in
        if lowc = Char.lowercase_ascii x then acc
        else match acc with
        | [] -> c :: acc
        | p :: tl ->
          if lowc = Char.lowercase_ascii p && c <> p
          then tl
          else c :: acc)
      []
      s
    in List.length stack
  in
  CharSet.fold
    (fun c m -> min (aux c) m)
    dict
    (String.length s)
