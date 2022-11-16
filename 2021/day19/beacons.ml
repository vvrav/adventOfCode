module TSet = Set.Make (struct type t = int * int * int let compare = compare end)
module USet = Set.Make (struct type t = int let compare = compare end)

exception Merged of (int * int * int) list * TSet.t

let sample_scanner =
  List.fold_left
    (fun acc p -> TSet.add p acc)
    TSet.empty
    [ 404,-588,-901;
      528,-643,409;
      -838,591,734;
      390,-675,-793;
      -537,-823,-458;
      -485,-357,347;
      -345,-311,381;
      -661,-816,-575;
      -876,649,763;
      -618,-824,-621;
      553,345,-567;
      474,580,667;
      -447,-329,318;
      -584,868,-557;
      544,-627,-890;
      564,392,-477;
      455,729,728;
      -892,524,684;
      -689,845,-530;
      423,-701,434;
      7,-33,-71;
      630,319,-379;
      443,580,662;
      -789,900,-551;
      459,-707,401; ]

(* all 24 possible permutations (incl. identity) *)
let rotations = [
  (fun (x, y, z) -> ( x,  y,  z));
  (fun (x, y, z) -> ( x, -z,  y));
  (fun (x, y, z) -> ( x, -y, -z));
  (fun (x, y, z) -> ( x,  z, -y));
  (fun (x, y, z) -> (-x,  y, -z));
  (fun (x, y, z) -> (-x,  z,  y));
  (fun (x, y, z) -> (-x, -y,  z));
  (fun (x, y, z) -> (-x, -z, -y));
  (fun (x, y, z) -> ( y,  z,  x));
  (fun (x, y, z) -> ( y, -x,  z));
  (fun (x, y, z) -> ( y, -z, -x));
  (fun (x, y, z) -> ( y,  x, -z));
  (fun (x, y, z) -> (-y, -z,  x));
  (fun (x, y, z) -> (-y, -x, -z));
  (fun (x, y, z) -> (-y,  z, -x));
  (fun (x, y, z) -> (-y,  x,  z));
  (fun (x, y, z) -> ( z,  x,  y));
  (fun (x, y, z) -> ( z, -y,  x));
  (fun (x, y, z) -> ( z, -x, -y));
  (fun (x, y, z) -> ( z,  y, -x));
  (fun (x, y, z) -> (-z, -y, -x));
  (fun (x, y, z) -> (-z,  x, -y));
  (fun (x, y, z) -> (-z,  y,  x));
  (fun (x, y, z) -> (-z, -x,  y));
]

(* THE CORE FUNCTIONS *)

(* keep it squared (and avoid dealing with floats) *)
let distance (x1, y1, z1) (x2, y2, z2) =
  let x = x1 - x2 and y = y1 - y2 and z = z1 - z2 in
  x*x + y*y + z*z

let get_distances points =
  let res = ref USet.empty in
  TSet.iter
    (fun a -> TSet.iter
      (fun b -> if a <> b then res := USet.add (distance a b) !res)
      points)
    points;
  !res

let read_file fn =
  let ic = open_in fn in
  let scans = ref [] in
  let current = ref TSet.empty in
  try while true do
    let l = input_line ic in
    if String.length l = 0 then begin
      scans := ([0,0,0], !current) :: !scans;
      current := TSet.empty end
    else try
      let point = Scanf.sscanf l "%d,%d,%d" (fun x y z -> x, y, z) in
      current := TSet.add point !current
    with Scanf.Scan_failure _ -> ()
    done;
    failwith "not even possible"
  with End_of_file ->
    if not (TSet.is_empty !current) then
      scans := ([0,0,0], !current) :: !scans;
  !scans

let translate (x, y, z) (a, b, c) = (a-x, b-y, c-z)

let merge (s1, scan1) (s2, scan2) =
  List.iter
    (fun r -> (* for each possible rotation ... *)
      let scan2 = TSet.map r scan2 in
      TSet.iter (*  for each point of s1 test it as origin *)
        (fun t1 -> 
          let scan1 = TSet.map (translate t1) scan1 in
          TSet.iter (* for each point of s2 test it as origin *)
            (fun t2 ->
              let scan2 = TSet.map (translate t2) scan2 in
              if TSet.cardinal (TSet.inter scan1 scan2) >= 12 then
                raise (Merged (
                  (List.map (translate t1) s1) @ (List.map (translate t2) (List.map r s2)),
                  TSet.union scan1 scan2)))
            scan2)
        scan1)
    rotations

let manhattan (x1, y1, z1) (x2, y2, z2) =
  abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

(* SOME FUNCTIONS FOR DEBUG PURPOSES *)

let list_of_scan s =
  TSet.fold (List.cons) s []

(* SOME TESTS ! *)

(* test that rotations preserve distance! *)
let () =
  let dist = get_distances sample_scanner in
  List.iter
    (fun rot -> assert (USet.equal dist (get_distances (TSet.map rot sample_scanner))))
    rotations

let dist_match_with_ref r =
  let dr = get_distances r in
  fun a ->let da = get_distances a in USet.cardinal (USet.inter dr da)

let run1 fn =
  let scans = ref (read_file fn) in
  while List.length !scans > 1 do
    match !scans with
     hd :: tl ->
      let ns, nm = List.fold_left
        (fun (acc, m) s ->
          try
            merge m s;
            (s :: acc, m)
          with Merged (s, m) -> (acc, (s, m)))
        ([], hd)
        tl
        in
      scans := nm :: ns
    | _ -> failwith "nope"
  done;
  let scanners, beacons = List.hd !scans in
  TSet.cardinal beacons, scanners

let () =
  let n, scanners = (run1 "input") in
  Printf.printf "Beacons: %d\n" n;
  print_endline "Scanners:";
  List.iter
    (fun (x,y,z) -> Printf.printf "  %d, %d, %d\n" x y z)
    scanners;
  let dists = ref [] in
    List.iter
      (fun x -> 
        List.iter
          (fun y -> if x <> y then dists := (manhattan x y) :: !dists)
          scanners)
      scanners;
  Printf.printf "Largest d = %d\n" (List.fold_left max 0 !dists);
  print_newline ()
