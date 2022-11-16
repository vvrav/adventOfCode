type fold = | Horizontal of int | Vertical of int

let debug = true

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

let print_map a =
  for y = 0 to Array.length a.(0) -1 do
    for x = 0 to Array.length a -1 do
      print_char (if a.(x).(y) then '#' else '.')
    done;
    print_newline ()
  done;
  print_newline ();
  print_newline ()

let fold_of_string s =
  Scanf.sscanf
    s
    "fold along %c=%d"
    (fun a b -> match (a,b) with
      | 'x', i -> Vertical i
      | 'y', i -> Horizontal i
      | _ -> failwith ("incorrect fold:" ^ s))

let data_of_input l =
  let dots = ref [] in
  let folds = ref [] in
  List.iter
    (fun s -> match String.split_on_char ',' s with
      | [x; y] -> dots := (int_of_string x, int_of_string y) :: !dots
      | [_] when String.length s > 0 -> folds := (fold_of_string s) :: !folds
      | _ -> ())
    l;
  !dots, !folds

let size dots =
  let mx, my = List.fold_left
    (fun (mx, my) (x,y) -> (max mx x, max my y))
    (0,0)
    dots in
  mx + 1, my + 1

let fold a f =
  let width = Array.length a in
  let height = Array.length a.(0) in
  match f with
  | Vertical x ->
    let na = Array.make_matrix x height false in
    for i = 0 to x-1 do
      for j = 0 to height -1 do
        na.(i).(j) <- a.(i).(j)
      done
    done;
    for i = 1 to width - x - 1 do
      for j = 0 to height -1 do
        na.(x-i).(j) <- na.(x-i).(j) || a.(x+i).(j)
      done
    done;
    if debug then print_map na;
    na
  | Horizontal y ->
    let na = Array.make_matrix width y false in
    for i = 0 to width - 1 do
      for j = 0 to y - 1 do
        na.(i).(j) <- a.(i).(j)
      done;
    done;
    for i = 0 to width - 1 do
      for j = 1 to height - y - 1 do
        na.(i).(y-j) <- na.(i).(y-j) || a.(i).(y+j)
      done
    done;
    if debug then print_map na;
    na

let run1 fn =
  let f = read_file fn in
  let dots, folds = data_of_input f in
  let width, height = size dots in
  let a = Array.make_matrix width height false in
  List.iter (fun (x,y) -> a.(x).(y) <- true) dots;
  if debug then print_map a;
  let final = List.fold_left fold a folds in
  (* let final = fold a (List.hd folds) in *)
  let count = ref 0 in
  Array.iter (Array.iter (fun x -> if x then incr count)) final;
  !count

