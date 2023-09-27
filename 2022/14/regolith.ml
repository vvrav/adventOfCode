

module CoordMap = Map.Make(struct type t = int * int let compare = compare end)

exception Rest

let parse_line s =
  s |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "->")
    |> List.map (fun s -> Scanf.sscanf s "%d,%d" (fun a b -> a, b))

let get_bounds input =
  let minx = ref max_int in
  let maxx = ref min_int in
  let miny = ref max_int in
  let maxy = ref min_int in
  List.iter
    (List.iter (fun (x,y) ->
      if x < !minx then minx := x;
      if x > !maxx then maxx := x;
      if y < !miny then miny := y;
      if y > !maxy then maxy := y))
    input;
  !minx, !maxx, !miny, !maxy

let build_map input =
  let map = ref CoordMap.empty in
  let add_point c = map:= CoordMap.add c '#' !map in
  let draw_line (ox, oy) (dx, dy) =
    if ox < dx then
      for x = ox to dx do add_point (x, oy) done
    else if ox > dx then
      for x = ox downto dx do add_point (x, oy) done
    else if oy < dy then
      for y = oy to dy do add_point (ox, y) done
    else
      for y = oy downto dy do add_point (ox, y) done
  in
  let rec draw_path l = match l with
    | c1 :: c2 :: tl -> draw_line c1 c2; draw_path (c2 :: tl)
    | _ -> ()
  in
  List.iter draw_path input;
  !map

let print_map map minx maxx miny maxy =
  for y = miny to maxy do
    for x = minx to maxx do
      match CoordMap.find_opt (x,y) map with
      | Some c -> print_char c
      | None -> print_char '.'
    done;
    print_newline ()
  done

let process input =
  let minx, maxx, miny, maxy = get_bounds input in
  let map = ref (build_map input) in
  print_map !map minx maxx miny maxy;
  let count = ref 0 in
  try
    while true do
      let x = ref 500 in
      (try for y = 0 to maxy do
        if not (CoordMap.mem (!x, y+1) !map) then (* ok it just drop!*)
           ()
        else if !x <= minx then (* escaped by the leftmost side *)
          raise Exit
        else if not (CoordMap.mem (!x-1, y+1) !map) then (* drop to the left *)
          decr x
        else if !x >= maxx then (* escaped by the rightmost side *)
          raise Exit
        else if not (CoordMap.mem (!x+1, y+1) !map) then (* drop to the right *)
          incr x
        else (* rest ! *) (
          map := CoordMap.add (!x, y) 'o' !map;
          raise Rest
        )
        done;
        (* to the abyss! *)
        raise Exit
      with Rest -> incr count)
    done;
    failwith "how is it event possible?"
  with Exit -> (
    print_newline ();
    print_map !map minx maxx 0 maxy;
    !count)

let process2 input =
  let minx, maxx, miny, maxy = get_bounds input in
  let map = ref (build_map input) in
  print_map !map minx maxx miny maxy;
  let count = ref 0 in
  while not (CoordMap.mem (500,0) !map) do
      let x = ref 500 in
      (try for y = 0 to maxy + 1 do
        if not (CoordMap.mem (!x, y+1) !map) then (* ok it just drop!*)
            ()
        else if not (CoordMap.mem (!x-1, y+1) !map) then (* drop to the left *)
          decr x
        else if not (CoordMap.mem (!x+1, y+1) !map) then (* drop to the right *)
          incr x
        else (* rest ! *) (
          map := CoordMap.add (!x, y) 'o' !map;
          raise Rest
        )
        done;
        map := CoordMap.add (!x, maxy+1) 'o' !map;
        incr count
      with Rest -> incr count)
    done;
    print_endline "finish!";
    print_newline ();
    print_map !map (minx-5) (maxx+5) 0 (maxy+2);
    !count
