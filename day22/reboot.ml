module ITSet = Set.Make (struct type t = int * int * int let compare = compare end)

let turn s (on, (x1, x2), (y1, y2), (z1, z2)) =
  let ns = ref ITSet.empty in
  for x = x1 to x2 do
    for y = y1 to y2 do
      for z = z1 to z2 do
        ns := ITSet.add (x, y, z) !ns
      done
    done
  done;
  if on then
    ITSet.union s !ns
  else
    ITSet.diff s !ns



let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s ->
      let op, (x1, x2), (y1, y2), (z1, z2) as instr =
        Scanf.sscanf s "%s x=%d..%d,y=%d..%d,z=%d..%d"
          (fun op x1 x2 y1 y2 z1 z2 ->
            if op <> "on" && op <> "off" then
              failwith "input error"
            else
              op = "on", (x1, x2), (y1, y2), (z1, z2))
      in
      if x1 < -50 || x2 > 50 || y1 < -50 || y2 > 50 || z1 < -50 || z2 > 50 then
        aux acc
      else
        aux (instr :: acc)
    | exception End_of_file -> acc
  in
  let instrs = List.rev (aux []) in
  close_in ic;
  instrs

let run1 fn =
  let instrs = read_file fn in
  let res = List.fold_left turn ITSet.empty instrs in
  ITSet.cardinal res

let () =
  let r = run1 "test2" in
  Printf.printf "%d\n" r
