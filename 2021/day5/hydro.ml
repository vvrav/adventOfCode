
let order a b =
  if a < b then a, b else b, a

let one_line s map =
  let x1, y1, x2, y2 =
    Scanf.sscanf s "%d,%d -> %d,%d" (fun a b c d -> a, b, c, d) in
  if x1 = x2
  then
    let y1, y2 = order y1 y2 in
    for y = y1 to y2 do
      map.(x1).(y) <- map.(x1).(y) + 1
    done
  else if y1 = y2
  then
    let x1, x2 = order x1 x2 in
    for x = x1 to x2 do
      map.(x).(y1) <- map.(x).(y1) + 1
    done
  else if (x2 - x1) * (y2 - y1) > 0
  then
    let x1, x2 = order x1 x2 in
    let y1, y2 = order y1 y2 in
    let diff = x2 - x1 in
    for i = 0 to diff do
      map.(x1+i).(y1+i) <- 1 + map.(x1+i).(y1+i)
    done
  else
    let x1, x2, y1, y2 =
      if x1 > x2 then x2, x1, y2, y1 else x1, x2, y1, y2 in
    let diff = x2 - x1 in
    for i = 0 to diff do
      map.(x1+i).(y1-i) <- 1 + map.(x1+i).(y1-i)
    done


let read_file dim fn =
  let map = Array.make_matrix dim dim 0 in
  let ic = open_in fn in
  let rec aux () =
    match input_line ic with
    | s -> one_line s map; aux ()
    | exception End_of_file -> close_in ic
  in
  aux ();
  map

let overlap map =
  let count = ref 0 in
  Array.iter (Array.iter (fun x -> if x > 1 then incr count)) map;
  !count
