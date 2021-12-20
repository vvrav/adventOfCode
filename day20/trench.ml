let read_file fn =
  let ic = open_in fn in
  let algo = input_line ic in
  assert (String.length algo = 512);
  let _ = input_line ic in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let f = List.rev (aux []) in
  close_in ic;
  let dimy = List.length f in
  let dimx = String.length (List.hd f) in
  let m = Array.make_matrix dimx dimy '.' in
  List.iteri
    (fun y s ->
      String.iteri
        (fun x c -> m.(x).(y) <- c)
        s)
    f;
  algo, m

let extend infc m =
  let dimx = Array.length m in
  let dimy = Array.length m.(0) in
  let minx = ref max_int in
  let maxx = ref min_int in
  let miny = ref max_int in
  let maxy = ref min_int in
  for x = 0 to dimx - 1 do
    for y = 0 to dimy - 1 do
      if m.(x).(y) <> infc then begin
        if x > !maxx then
          maxx := x;
        if x < !minx then
          minx := x;
        if y > !maxy then
          maxy := y;
        if y < !miny then
          miny := y
      end
    done
  done;
  let ndimx = !maxx - !minx + 5 in
  let ndimy = !maxy - !miny + 5 in
  let nm = Array.make_matrix ndimx ndimy infc in
  for x = 0 to dimx - 1 do
    for y = 0 to dimy - 1 do
      nm.(x + 2 - !minx).(y + 2 - !miny) <- m.(x).(y)
    done
  done;
  nm

let print_m m =
  let dimx = Array.length m in
  let dimy = Array.length m.(0) in
  for x = 0 to dimx - 1 do
    for y = 0 to dimy - 1 do
        print_char m.(x).(y)
    done;
    print_newline ()
  done

let count m =
  Array.fold_left
    (Array.fold_left (fun acc c -> if c = '#' then acc + 1 else acc))
    0
    m

let neighbors (x, y) = [
  x-1, y-1; x, y-1; x+1, y-1;
  x-1, y; x, y; x+1, y;
  x-1, y+1; x, y+1; x+1, y+1;
]
    
let algopos (x,y) m =
  List.fold_left
    (fun acc (x,y) ->
      if m.(x).(y) = '#' then
        2 * acc + 1
      else
        2 * acc)
    0
    (neighbors (x,y))

let next_char algo m (x,y) =
  let pos = algopos (x, y) m in
  algo.[pos]

let one_step infc algo m =
  let m = extend infc m in
  let dimx = Array.length m in
  let dimy = Array.length m.(0) in
  let ninfc = if infc = '.' then algo.[0] else algo.[511] in
  let nm = Array.make_matrix dimx dimy ninfc in
  for x = 1 to dimx - 2 do
    for y = 1 to dimy - 2 do
      nm.(x).(y) <- next_char algo m (x, y)
    done
  done;
  nm, ninfc

let run fn steps =
  let algo, m = read_file fn in
  let m = ref m in
  let infc = ref '.' in
  for i = 0 to steps - 1 do
    let nm, ninfc = one_step !infc algo !m in
    m := nm; infc := ninfc
  done;
  count !m
