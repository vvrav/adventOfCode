type grid = int array array * int
exception Bingo of grid * int
module GridSet = Set.Make(struct type t = grid let compare (m1, id1) (m2, id2) = compare id1 id2 end)

let get_id =
  let n = ref 0 in
  fun () -> incr n; !n

let grids = ref GridSet.empty
let add_grid m = grids := GridSet.add m !grids
let is_last () = GridSet.cardinal !grids = 1
let rem_grid m = grids := GridSet.remove m !grids
let has_not_won m = GridSet.mem m !grids

let nmap = Array.make 100 []

let push n m = nmap.(n) <- m :: nmap.(n)

let read_nums s =
  let l = String.split_on_char ',' s in
  List.map int_of_string l

let read_line s =
  let sl = String.split_on_char ' ' s in
  List.filter_map (function "" -> None | s -> Some (int_of_string s)) sl

let read_grid l =
  let m = Array.make_matrix 5 5 0 in
  let id = get_id () in
  add_grid (m, id);
  let rec aux l i =
    match l with
    | s :: tail ->
      let nums = read_line s in
      List.iteri (fun j x -> m.(i).(j) <- x; push x ((m, id), i, j)) nums;
      aux tail (i+1)
    | [] -> ()
  in
  aux l 0

let read_all fn =
  let ic = open_in fn in
  let nums = read_nums (input_line ic) in
  let _ = input_line ic in
  let rec group acc1 acc2 =
    match input_line ic with
    | "" -> group [] (acc1 :: acc2)
    | s -> group (s :: acc1) acc2
    | exception End_of_file -> if List.length acc1 > 0 then acc1 :: acc2 else acc2
  in
  let groups = group [] [] in
  nums, groups


let check (m, id) n i j =
  let pouet = [0; 1; 2; 3; 4] in
  if List.for_all (fun x -> m.(x).(j) = -1) pouet || List.for_all (fun x -> m.(i).(x) = -1) pouet
  then raise (Bingo ((m, id), n))

let sum m =
  Array.fold_left
    (fun acc l -> acc + Array.fold_left (fun acc n -> if n > 0 then acc + n else acc) 0 l)
    0
    m

let one_number1 n =
  let ml = nmap.(n) in
  List.iter (fun ((m, id), i, j) -> m.(i).(j) <- -1; check (m, id) n i j) ml

let all_numbers1 nl =
  try
    List.iter one_number1 nl; None
  with
  | Bingo((m, id),n) -> Some (n, m, n * sum m)

let one_number2 n =
  let ml = nmap.(n) in
  List.iter
    (fun ((m, id), i, j) ->
      m.(i).(j) <- -1;
      if has_not_won (m, id) then
      try
          check (m, id) n i j
      with
        | Bingo(_) as b -> 
          if is_last () then raise b
          else rem_grid (m, id) )
    ml 

let all_numbers2 nl =
  try
    List.iter one_number2 nl; None
  with
  | Bingo((m, id),n) -> Some (n, m, n * sum m)
    

let readnprocess fn =
  let nums, groups = read_all fn in
  List.iter read_grid groups;
  nums
