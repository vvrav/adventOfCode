open Scanf

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> 
      let a = Scanf.sscanf s "%d-%d %c: %s" (fun a b c d -> a, b, c, d) in
      aux (a :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  ls

let test_one (a, b, c, pass) =
  let count = ref 0 in
  String.iter (fun x -> if x = c then incr count) pass;
  !count >= a && !count <= b

let test_one2 (a, b, c, pass) =
  (c = pass.[a-1]) <> (c = pass.[b-1])

let process l test_fun =
  List.fold_left
    (fun acc e -> if test_fun e then acc + 1 else acc)
    0
    l
