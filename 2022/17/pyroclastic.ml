module IntPairSet = Set.Make(struct type t = int * int let compare (x1,y1) (x2,y2) = compare (y1,x1) (y2,x2) end)
module IntMap = Map.Make(struct type t = int let compare = compare end)

exception Blocked

let f1 = [ 0,0 ; 1,0 ; 2,0 ; 3,0 ]

let f2 = [ 
  1,0 ;
  0,1 ; 1,1 ; 2,1 ;
  1,2
]

let f3 = [
  0,0 ; 1,0 ; 2,0 ;
  2,1 ;
  2,2 ;
]

let f4 = [ 0,0 ; 0,1 ; 0,2 ; 0,3 ]

let f5 = [
  0,0 ; 1,0 ;
  0,1 ; 1,1 ;
]


let print_map ?shape map =
  let map = match shape with
  | Some (sh, (sx, sy)) -> List.fold_left (fun map (x,y)-> IntPairSet.add (x+sx, y+sy) map) map sh
  | None -> map
  in
  let _, my = IntPairSet.max_elt map in
  for y = my downto 0 do
    for x = 0 to 6 do
      if IntPairSet.mem (x, y) map then print_char '#'
      else print_char '.'
    done;
    print_newline ();
  done;
  print_newline ()

let movex c shape (x, y) map =
  let dx = if c = '>' then 1 else -1 in
  try
    List.iter
      (fun (bx,by) ->
        let nbx = x + bx + dx in
        let nby = y + by in
        if nbx < 0 || nbx >= 7 || IntPairSet.mem (nbx,nby) map then raise Blocked)
      shape;
    (x + dx, y)
  with Blocked -> x, y

let movey shape (x, y) map =
  List.iter
    (fun (bx, by) ->
      let nbx = x + bx in
      let nby = y + by - 1 in
      if nby < 0 || IntPairSet.mem (nbx, nby) map then raise Blocked)
    shape;
  x, y - 1

let one_shape ?(debug=false) shape (jets_, ji, jl) map =
  let maxy = match IntPairSet.max_elt_opt map with | Some (_, y) -> y | None -> -1 in
  let bxy = ref (2, maxy + 4) in
  if debug then
    print_map ~shape:(shape, !bxy) map;
  try while true do
    let jd = jets_.[!ji] in
    if debug then (
      print_char jd;
      print_newline ()
    );
    ji := (!ji + 1) mod jl;
    (* move sideway *)
    bxy := movex jd shape !bxy map;
    if debug then
      print_map ~shape:(shape, !bxy) map;
    (* move down *)
    bxy := movey shape !bxy map;
    if debug then
      print_map ~shape:(shape, !bxy) map;
  done;
  failwith "oopsie"
  with Blocked ->
    let bx, by = !bxy in
    List.fold_left
      (fun map (cx, cy) -> IntPairSet.add (bx + cx, by + cy) map)
      map
      shape

let process_n ?(debug=false) input n =
  let shapes = Array.of_list [f1; f2; f3; f4; f5] in
  let shapes_length = Array.length shapes in
  let jets_index = ref 0 in
  let jets_length = String.length input in
  let map = ref IntPairSet.empty in
  let states = Array.make shapes_length IntMap.empty in
  let bonus = ref 0 in
  let step = ref 0 in
  while !step < n do
    print_endline (string_of_int !step);
    let shapei = !step mod shapes_length in
    let _, altitude = Option.value ~default:(0,-1) (IntPairSet.max_elt_opt !map) in
    let st = states.(shapei) in
    (if IntMap.mem !jets_index st then
      let blockn = IntMap.find !jets_index st in
      (match blockn with
      | (i1, a1) :: (i2, a2) :: (i3, a3) :: _ when
        i1 - i2 = i2 - i3 && a1 - a2 = a2 - a3 ->
          let di = i1 - i2 in
          let da = a1 - a2 in
          let q = (n - !step) / di in
          step := !step + di * q;
          bonus := !bonus + da * q;
          Printf.printf "di: %d ; da: %d ; q: %d ; step: %d ; bonus: %d\n" di da q !step !bonus
      | _ -> ());
      let s = String.concat ", " (List.map (fun (n, a) -> Printf.sprintf "(%d, %d)" n a) blockn) in
      Printf.printf "Cycle found: %d -> %d: %s\n" shapei !jets_index s);
    states.(shapei) <- IntMap.update !jets_index (function None -> Some [!step, altitude] | Some l -> Some ((!step, altitude) :: l)) st;
    if !step < n then (
    let shape = shapes.(shapei) in
    map := one_shape ~debug shape (input, jets_index, jets_length) !map;
    incr step)
  done;
  let _, my = IntPairSet.max_elt !map in
  my + 1 + !bonus

let () =
  let ic = open_in "input" in
  let s = input_line ic in
  close_in ic;
  let r = 
    process_n s 1000000000000
  in
  print_endline (string_of_int r)
