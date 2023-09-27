module IntPairSet = Set.Make(struct type t = int * int let compare = compare end)

let matrix_of_input sl =
  let width = String.length (List.hd sl) in
  let height = List.length sl in
  let map = Array.make_matrix width height 0 in
  List.iteri
    (fun y s ->
      String.iteri
        (fun x c -> map.(x).(y) <- int_of_char c - int_of_char '0')
        s)
    sl;
  map

let print_matrix m =
  for y = 0 to Array.length m - 1 do
    for x = 0 to Array.length m.(0) - 1 do
      print_int m.(x).(y);
    done;
    print_newline ()
  done

let get_visible map =
  let height = Array.length map.(0) in
  let width = Array.length map in
  Printf.printf "height: %d ; width: %d\n" height width;
  let vset = ref IntPairSet.empty in
  let add tree =
    vset := IntPairSet.add tree !vset
  in
  for x = 0 to width - 1 do
    let max = ref (-1) in
    let y = ref 0 in
    Printf.printf "Col %d: top to bottom\n" x;
    while !y < height -1 do
      if map.(x).(!y) > !max then begin
        Printf.printf "  add (%d,%d) of size %d (previous max size was %d)\n" x !y map.(x).(!y) !max;
        add (x, !y);
        max := map.(x).(!y)
      end;
      incr y
    done;
    let max = ref (-1) in
    let y = ref (height -1) in
    Printf.printf "Col %d: bottom to top\n" x;
    while !y > 0 do
      if map.(x).(!y) > !max then begin
        add (x, !y);
        max := map.(x).(!y)
      end;
      decr y
    done;
  done;
  for y = 0 to height -1 do
    let max = ref (-1) in
    let x = ref 0 in
    while !x < width -1 do
      if map.(!x).(y) > !max then begin
        add (!x, y);
        max := map.(!x).(y)
      end;
      incr x
    done;
    let max = ref (-1) in
    let x = ref (width -1 ) in
    while !x > 0 do
      if map.(!x).(y) > !max then begin
        add (!x, y);
        max := map.(!x).(y)
      end;
      decr x
    done;
  done;
  !vset

let get_scenic_score map x y =
  let height = Array.length map.(0) in
  let width = Array.length map in
  let size = map.(x).(y) in
  let total = ref 1 in
  (* top bottom *)
  let score = ref 0 in
  (try
    for i = y + 1 to height - 1 do
      Printf.printf "(%d,%d)\n" x i;
      incr score;
      if map.(x).(i) >= size then raise Exit
      done
  with Exit -> ());
  total := !total * !score;
  (* bottom to top *)
  score := 0;
  (try
    for i = y - 1 downto 0 do
      Printf.printf "(%d,%d)\n" x i;
      incr score;
      if map.(x).(i) >= size then raise Exit
      done
  with Exit -> ());
  total := !total * !score;
  (* left to right *)
  score := 0;
  (try
    for i = x + 1 to width - 1 do
      Printf.printf "(%d,%d)\n" x i;
      incr score;
      if map.(i).(y) >= size then raise Exit
      done
  with Exit -> ());
  total := !total * !score;
  (* right to left *)
  score := 0;
  (try
    for i = x - 1 downto 0 do
      Printf.printf "(%d,%d)\n" x i;
      incr score;
      if map.(i).(y) >= size then raise Exit
      done
  with Exit -> ());
  total := !total * !score;
  !total

let get_max_scenic_score map =
    let max = ref 0 in
    let height = Array.length map.(0) in
    let width = Array.length map in
    for i = 0 to width - 1 do
      for j = 0 to height - 1 do
        let s = get_scenic_score map i j in
        if s > !max then max := s
      done
    done;
    !max

(* totally not satified with these solutions xD *)
