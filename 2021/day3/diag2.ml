exception ReadError of char

let one_line s (z, u) i =
  match s.[i] with
  | '0' -> z + 1, u
  | '1' -> z, u + 1
  | c -> raise (ReadError c)

let process_list l i =
  let rec aux l f i = match l with
  | s :: tail -> let f = one_line s f i in aux tail f i
  | [] -> f
  in
  aux l (0, 0) i

let filter c i =
  List.filter (fun s -> s.[i] = c)

let get_o_rating l =
  let rec aux l i =
    match l with
    | [s] -> s
    | [] -> failwith "oops"
    | _ ->
      let (z, u) = process_list l i in
      if z > u
      then aux (filter '0' i l) (i+1)
      else aux (filter '1' i l) (i+1)
  in
  int_of_string ("0b" ^ aux l 0)

let get_co2_rating l =
  let rec aux l i =
    match l with
    | [s] -> s
    | [] -> failwith "oops"
    | _ ->
      let (z, u) = process_list l i in
      if z > u
      then aux (filter '1' i l) (i+1)
      else aux (filter '0' i l) (i+1)
  in
  int_of_string ("0b" ^ aux l 0)

let read_file fn =
  let ic = open_in fn in
  let rec aux l = match input_line ic with
    | s -> aux (s :: l)
    | exception End_of_file -> l
  in
  let sl = aux [] in
  close_in ic;
  sl