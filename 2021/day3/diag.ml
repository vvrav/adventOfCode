exception ReadError of char

let one_line s a =
  String.iteri
    (fun i c ->
      let z, u = a.(i) in
      match c with
      | '0' -> a.(i) <- z+1, u
      | '1' -> a.(i) <- z, u+1
      | c -> raise (ReadError c))
    s

let first_line s =
  let l = String.length s in
  let a = Array.make l (0, 0) in
  String.iteri 
    (fun i c ->
      match c with
      | '0' -> a.(i) <- (1, 0)
      | '1' -> a.(i) <- (0,1)
      | c -> raise (ReadError c))
    s;
  a

let rec process_file ic a =
  match input_line ic with
  | s -> one_line s a; process_file ic a
  | exception End_of_file -> close_in ic
  | exception e -> close_in ic; raise e

let get_freq fn =
  let ic = open_in fn in
  try
    let a = first_line (input_line ic) in
    process_file ic a;
    close_in ic;
    a
  with e -> close_in ic; raise e

let get_rates a =
  Array.fold_left
    (fun (gamma, epsilon) (z, u) -> if z < u then (gamma *2 + 1, epsilon * 2) else (gamma * 2, epsilon * 2 + 1))
    (0, 0)
    a

let run fn =
  let a = get_freq fn in
  let (gamma, epsilon) = get_rates a in
  gamma * epsilon