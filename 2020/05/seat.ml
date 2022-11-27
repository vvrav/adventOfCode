



let ups_of_rows s =
  let res = ref [] in
  String.iter (fun c -> res := (c = 'B') :: !res) s;
  List.rev !res

let ups_of_cols s =
  let res = ref [] in
  String.iter (fun c -> res := (c = 'R') :: !res) s;
  List.rev !res

let rec dicho (a,b) up =
  let mid = (a + b + 1) / 2 in
  if up then (mid, b) else (a, mid-1)

let id_of_pass s =
  let rows = ups_of_rows (String.sub s 0 7) in
  let cols = ups_of_cols (String.sub s 7 3) in
  let (row, _) = List.fold_left dicho (0, 127) rows in
  let (col, _) = List.fold_left dicho (0, 7) cols in
  row * 8 + col


let find_hole ids =
  let rec aux acc l =
    match l with
    | h1 :: h2 :: tl ->
      if h2 - h1 > 1
      then aux ((h1, h2) :: acc) (h2 :: tl) 
      else aux acc (h2 :: tl)
    | _ -> acc
  in
  aux [] (List.sort compare ids)

let process () =
  let ic = open_in "input" in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> List.rev acc
  in
  let passes = aux [] in
  close_in ic;
  let ids = List.map id_of_pass passes in
  let max_id = List.fold_left max 0 ids in
  print_endline ("Max id: " ^ (string_of_int max_id));
  find_hole ids
  
(* 675 *)