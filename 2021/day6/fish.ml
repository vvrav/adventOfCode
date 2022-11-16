let read_file fn =
  let ic = open_in fn in
  let l = input_line ic in
  close_in ic;
  List.map int_of_string (String.split_on_char ',' l)

let read_data l =
  let a = Array.make 9 0 in
  List.iter (fun x -> a.(x) <- a.(x) + 1) l;
  a

let one_day a =
  let b = Array.make 9 0 in
  List.iteri (fun i x -> b.(i) <- x) (List.tl (Array.to_list a));
  b.(6) <- b.(6) + a.(0);
  b.(8) <- a.(0);
  b

let process fn n =
  let data = read_file fn in
  let a = ref (read_data data) in
  for i = 1 to n do
    a := one_day !a
  done;
  Array.fold_left (+) 0 !a
