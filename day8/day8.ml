
let size_to_digits = [|
  [];
  [];
  [1];
  [7];
  [4];
  [2; 3; 5];
  [0; 6; 9];
  [8]
|]

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

let read_line l =
  match String.split_on_char '|' l with
  | [p; d] ->
      let pl = String.split_on_char ' ' (String.trim p) in
      let dl = String.split_on_char ' ' (String.trim d) in
      pl, dl
  | _ -> failwith "failed to read line"

let run1 fn =
  let f = read_file fn in
  let digits = List.flatten (List.map (fun l -> snd (read_line l)) f) in
  let count = ref 0 in
  List.iter (fun x -> if List.length size_to_digits.(String.length x) = 1 then (print_endline x; incr count)) digits;
  !count
