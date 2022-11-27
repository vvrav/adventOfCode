module IntSet = Set.Make(struct type t = int let compare = compare end)

exception Found of int*int*int

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux (IntSet.add (int_of_string s) acc)
    | exception End_of_file -> acc
  in
  let ls = aux IntSet.empty in
  close_in ic;
  ls

let rec process s =
  let n = IntSet.choose s in
  print_string "on test le "; print_int n; print_newline ();
  let ns = IntSet.remove n s in
  if IntSet.is_empty ns then
    0
  else 
    if IntSet.exists (fun m -> m + n = 2020) ns then
      n * (2020-n)
    else
      process ns

let rec process2 s =
  try
    let n = IntSet.choose s in
    print_string "on test le "; print_int n; print_newline ();
    let ns = IntSet.remove n s in
    if IntSet.is_empty ns then
      None
    else (
      IntSet.iter
        (fun m ->
          if IntSet.mem (2020 - m - n) (IntSet.remove m ns)
            then raise (Found (n, m, (2020 - m - n))))
        ns;
      process2 ns
    )
with Found (x, y, z) -> Some (x, y, z)
