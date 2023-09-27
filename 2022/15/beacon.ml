module IntPairSet = Set.Make(struct type t = int * int let compare = compare end)

exception Found of int * int

let parse_line s =
  Scanf.sscanf s
    "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun a b c d -> (a,b), (c,d))

let manhattan (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let get_scan_area =
  List.map (fun (s, b) -> s, manhattan s b)

let get_beacons_on_line n l =
  List.fold_left
    (fun acc (_, (bx, by)) -> if by = n then IntPairSet.add (bx, by) acc else acc)
    IntPairSet.empty
    l

let intervals_of_line y =
  List.fold_left
    (fun acc ((sx, sy), r) ->
      let r = r - abs (sy - y) in
      if r > 0 then (sx - r, sx + r) :: acc
      else acc)
  []

let merge_intervals l =
  let l = List.sort compare l in
  let rec aux acc l =
    match l with
    | [i] -> i :: acc
    | ((x1, y1) as p1) :: ((x2, y2) as p2) :: tl ->
        if x2 > y1 + 1 then aux (p1 :: acc) (p2 :: tl)
        else if y1 > y2 then aux acc (p1 :: tl)
        else aux acc ((x1, y2) :: tl)
    | [] -> failwith "nope"
  in
  List.rev (aux [] l)

let covered_positions beacons =
  List.fold_left
    (fun acc (a, b) ->
      let b_in_i = IntPairSet.filter (fun (x, _) -> x >= a && x <= b) beacons in
       acc + b - a + 1 - (IntPairSet.cardinal b_in_i))
    0

let process1 input n =
  let scan_areas = get_scan_area input in
  let beacons = get_beacons_on_line n input in
  let merged_intervals = scan_areas |> intervals_of_line n |> merge_intervals in
  covered_positions beacons merged_intervals

let process2 low high input =
  let scan_areas = get_scan_area input in
  try
    for y = low to high do
      let intervals = scan_areas |> intervals_of_line y |> merge_intervals in
      List.iter (fun (a,b) ->
          if a > low then raise (Found (a-1,y))
          else if b < high then raise (Found (b+1,y)))
      intervals
    done;
    raise Not_found
  with | Found (x, y) -> x, y, x*high+y