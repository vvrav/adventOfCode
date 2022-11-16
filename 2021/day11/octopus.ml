module IntPairSet = Set.Make(struct type t = int*int let compare = compare end)

let get_neighbors i j =
  let n = [i-1, j-1; i-1, j; i-1, j+1; i, j-1; i, j+1; i+1, j-1; i+1, j; i+1, j+1] in
  List.filter
    (fun (i, j) -> 0 <= i && i < 10 && 0 <= j && j < 10)
    n

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
  let a = Array.make_matrix 10 10 0 in
  List.iteri
    (fun i s ->
      String.iteri
        (fun j c -> a.(i).(j) <- (int_of_char c - int_of_char '0'))
        s
      )
    l;
  a

let new_flash a f =
  let n = ref IntPairSet.empty in
  for i = 0 to 9 do
    for j = 0 to 9 do
      if a.(i).(j) > 9 && not (IntPairSet.mem (i,j) f) then n := IntPairSet.add (i,j) !n
    done
  done;
  !n

let one_step a =
  let flash = ref IntPairSet.empty in
  for i = 0 to 9 do
    for j = 0 to 9 do
      a.(i).(j) <- a.(i).(j) + 1
    done
  done;
  let nf = ref (new_flash a !flash) in
  while not (IntPairSet.is_empty !nf) do
    IntPairSet.iter
      (fun (i, j) ->
        List.iter
          (fun (x,y) -> a.(x).(y) <- a.(x).(y) + 1)
          (get_neighbors i j))
      !nf;
    flash := IntPairSet.union !flash !nf;
    nf := new_flash a !flash
  done;
  IntPairSet.iter (fun (i,j) -> a.(i).(j) <- 0) !flash;
  IntPairSet.cardinal !flash

let run1 nf steps =
  let f = read_file nf in
  let map = map_of_input f in
  let count = ref 0 in
  for i = 0 to steps - 1 do
    count := !count + one_step map
  done;
  !count

let run2 nf =
  let f = read_file nf in
  let map = map_of_input f in
  let count = ref 1 in
  while one_step map < 100 do
    incr count
  done;
  !count
