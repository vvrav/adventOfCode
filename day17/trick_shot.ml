
module IntSet = Set.Make (struct type t = int let compare = compare end)

let list_of_set s = IntSet.fold (List.cons) s []

let read_file fn =
  let ic = open_in fn in
  let s = input_line ic in
  let target =
    Scanf.sscanf s "target area: x=%d..%d, y=%d..%d" (fun a b c d -> a, b, c, d) in
  close_in ic;
  target

let triangulaire x =
  x * (x+1) / 2

let get_xs xmin xmax =
  let res = ref [] in
  for initial_vx = 1 to xmax do
    let x = ref 0 in
    let vx = ref initial_vx in
    let i = ref 0 in
    while !x <= xmax && !vx > 0 do
      incr i;
      x := !x + !vx;
      decr vx;
      if !x >= xmin then res := (initial_vx, !i) :: !res
    done
  done;
  !res

let get_steps xs =
  List.fold_left (fun acc (_, s) -> IntSet.add s acc) IntSet.empty xs

let get_bornes n ymin ymax =
  let nf = float_of_int n in
  let aux y = let yf = float_of_int y in(2. *. yf +. nf *. nf -. nf) /. (2. *. nf) in
  int_of_float (ceil (aux ymin)), int_of_float (floor (aux ymax))

let get_ys x n ymin ymax =
  let res = ref IntSet.empty in
  if x <> n then
    let yinf, ysup = get_bornes n ymin ymax in
    for y = yinf to ysup do
      res := IntSet.add y !res
    done;
  else begin (* specific case when vx falls to 0 *)
    for i = n to 10*n do
      let yinf, ysup = get_bornes i ymin ymax in
      for y = yinf to ysup do
        res := IntSet.add y !res
      done
    done
  end;
  !res

let run1 fn =
  let xmin, xmax, ymin, ymax = read_file fn in
  let nsteps = get_xs xmin xmax in
  let ys =
    List.fold_left
      (fun acc (x,n) -> IntSet.union (get_ys x n ymin ymax) acc)
      IntSet.empty
      nsteps
  in
  let hys = IntSet.map triangulaire ys in
  IntSet.max_elt hys
