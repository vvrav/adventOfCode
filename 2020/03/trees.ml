let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = List.rev (aux []) in
  close_in ic;
  ls

let process1 l =
  let rec aux acc map n =
    match map with
    | [] -> acc
    | hd :: tl ->
      let nacc = if hd.[n] = '#' then acc + 1 else acc in
      aux nacc tl ((n + 3) mod (String.length hd))
    in
    aux 0 l 0

let rec pop n l =
  if n > 0 then
    match l with
    | [] -> []
    | hd :: tl -> pop (n-1) tl
  else
    l
  
let process2 stepx stepy l =
  let rec aux acc map n =
    match map with
    | [] -> acc
    | hd :: _ ->
      let nacc = if hd.[n] = '#' then acc + 1 else acc in
      aux nacc (pop stepy map) ((n + stepx) mod (String.length hd))
    in
    aux 0 l 0

