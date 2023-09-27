module IntPairSet = Set.Make(struct type t = int * int let compare = compare end)

let is_too_far (x, y) (a, b) =
  abs (x - a) > 1 || abs (y - b) > 1

let pre_process s = Scanf.sscanf s "%c %d" (fun a b -> a,b)

let one_step dir ((hx, hy) :: tl) =
  let hx, hy = match dir with
  | 'U' -> hx, hy + 1
  | 'D' -> hx, hy - 1
  | 'R' -> hx + 1, hy
  | 'L' -> hx - 1, hy
  | _ -> failwith "unknown dir"
  in
  let rec aux hl tl =
    match hl, tl with
    | _, [] -> List.rev hl
    | [], _ -> failwith "oopsie"
    | (px, py) :: _, (cx, cy) :: tltl ->
      if not (is_too_far (px, py) (cx, cy)) then
        (* the current has no need to move, so neither have the others*)
        List.append (List.rev hl) tl
      else
        let ncx, ncy =
            (if px > cx then cx + 1
            else if px < cx then cx - 1
            else cx),
            (if py > cy then cy + 1
            else if py < cy then cy - 1
            else cy)
        in
        aux ((ncx, ncy) :: hl) tltl    
  in
    aux [hx, hy] tl

let rec one_instr f (rope, acc) (d, l)  =
  if l = 0 then rope, acc
  else
    let nrope = one_step d rope in
    let nacc = f acc nrope in
    one_instr f (nrope, nacc) (d, l - 1)

let rec last l = match l with
    | [] -> raise Not_found
    | x :: [] -> x
    | _ :: tl -> last tl

let process instrs size =
  let f = fun v r -> IntPairSet.add (last r) v in
  let rope = List.init size (fun _ -> 0, 0) in
  let _, s = List.fold_left (one_instr f) (rope, IntPairSet.empty) instrs in
  IntPairSet.cardinal s
