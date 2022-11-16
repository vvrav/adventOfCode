exception Touche
exception Rate
module IntPaisSet = Set.Make (struct type t = int*int let compare = compare end)

(* Ugly brute force *)

let touche vx vy xmin xmax ymin ymax =
  let x, y, vx, vy = ref 0, ref 0, ref vx, ref vy in
  try for i = 0 to 50000 do
      x := !x + !vx;
      y := !y + !vy;
      if !vx > 0 then decr vx;
      decr vy;
      if !x <= xmax && !x >= xmin && !y <= ymax && !y >= ymin then
        raise Touche;
      if !x > xmax || (!vx = 0 && !x < xmin) then
        raise Rate;
      if !vy < 0 && !y < ymin then
        raise Rate
      done;
      false
    with
    | Touche -> true
    | Rate -> false

let () =
  let ic = open_in "input" in
  let s = input_line ic in
  let xmin, xmax, ymin, ymax =
  Scanf.sscanf s "target area: x=%d..%d, y=%d..%d" (fun a b c d -> a, b, c, d) in
  close_in ic;
  let res = ref IntPaisSet.empty in
  for x = 0 to 1000 do
    for y = -1000 to 5000 do
        if touche x y xmin xmax ymin ymax then
        res := IntPaisSet.add (x, y) !res 
    done;
  done;
  IntPaisSet.iter (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) !res;
  print_int (IntPaisSet.cardinal !res);
  print_newline ()

