let read_file fn =
  let ic = open_in fn in
  let l = input_line ic in
  close_in ic;
  let l = List.map int_of_string (String.split_on_char ',' l) in
  List.sort compare l

let median l =
  let len = List.length l in
  if len mod 2 = 0
  then float_of_int (List.nth l (len/2))
  else (float_of_int ((List.nth l (len/2)) + (List.nth l (len/2 + 1))) /. 2.)

let moy l =
  let len = List.length l in
  let sum = List.fold_left (+) 0 l in
  (float_of_int sum) /. (float_of_int len)

let cost m l =
  List.fold_left (fun acc x -> acc + (abs (m - x))) 0 l

let run fn =
  let l = read_file fn in
  let m = int_of_float (median l) in
  cost m l

let triang x = x * (x+1) / 2

let cost2 m l =
  List.fold_left (fun acc x -> acc + triang (abs (m-x))) 0 l

let run2 fn =
  let l = read_file fn in
  let m = moy l in
  min (cost2 (int_of_float m) l) (cost2 (int_of_float (ceil m)) l)
  