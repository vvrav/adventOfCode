module CharSet = Set.Make(struct type t = char let compare = compare end)

let buffer_size = 14

let next_of_string s =
  let i = ref (-1) in
  fun () ->
    incr i;
    if !i >= String.length s then raise End_of_file
    else s.[!i]

let is_valid_buffer a =
  let cs = Array.fold_left (fun acc c -> CharSet.add c acc) CharSet.empty a in
  not (CharSet.mem ' ' cs) && (CharSet.cardinal cs = buffer_size)

let find_start next =
  let a = Array.make buffer_size ' ' in
  let push c =
    Array.blit a 0 a 1 (buffer_size - 1);
    a.(0) <- c
  in
  let rec scan index =
    match next () with
    | c ->
      push c;
      if is_valid_buffer a then index
      else scan (index + 1)
    | exception End_of_file -> failwith "not found"
  in
  scan 1
