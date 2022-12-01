module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)

module CharSet = Set.Make(struct type t = char let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = compare end)

let get_input process filename =
  let ic = open_in filename in
  let rec aux acc =
    match input_line ic with
    | s -> aux ((process s) :: acc)
    | exception End_of_file -> List.rev acc
  in
  let s = aux [] in
  close_in ic;
  s

let get_raw_input = get_input (fun x -> x)
