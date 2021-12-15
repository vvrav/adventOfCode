exception Found of int

(* Set for points to be processed. first value is the cost, so that smallest cost is always treadted first *)
module IntTrupleSet = Set.Make (struct type t = int * int * int let compare = compare end)

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  List.rev ls

let get_neighbors x y len =
  List.filter
    (fun (a, b) -> a >= 0 && a < len && b >= 0 && b < len)
    [x-1, y; x, y-1; x+1, y; x, y+1]

let data_of_input l =
  let len = String.length (List.hd l) in
  let map = Array.make_matrix len len 0 in
  List.iteri
    (fun y s -> 
      String.iteri
        (fun x c -> map.(y).(x) <- int_of_char c - int_of_char '0')
        s)
    l;
  map

let process_cost map =
  let len = Array.length map in
  let todo = ref (IntTrupleSet.singleton (0, 0, 0)) in
  let costs = Array.make_matrix len len 1000000 in
  costs.(0).(0) <- 0;
  try
    while IntTrupleSet.cardinal !todo > 0 do
      (* print_debug (Printf.sprintf "Todo: %s" (string_of_todo !todo)); *)
      let c, x, y = IntTrupleSet.min_elt !todo in
      todo := IntTrupleSet.remove (c, x, y) !todo;
      if c <= costs.(x).(y) then begin (* discard obsolete queue items *)
        let cc = costs.(x).(y) in
        let neighbors = get_neighbors x y len in
        List.iter
          (fun (i, j) -> 
            let nc = cc + map.(i).(j) in
            if i = len-1 && j = len-1 then raise (Found nc);
            if nc < costs.(i).(j) && nc < costs.(len-1).(len-1) then begin
              costs.(i).(j) <- nc;
              todo := IntTrupleSet.add (nc, i,j) !todo
            end)
          neighbors;
      end
    done;
    failwith "error: not found!"
  with Found c -> c

let run1 fn =
  let f = read_file fn in
  let map = data_of_input f in
  process_cost map

let full_map smap =
  let len = Array.length smap in
  let fmap = Array.make_matrix (len*5) (len*5) 0 in
  for i = 0 to 4 do
    for j = 0 to 4 do
      for x = 0 to len -1 do
        for y = 0 to len -1 do
          fmap.(x+i*len).(y+j*len) <- ((smap.(x).(y) + i + j -1) mod 9)+1
        done
      done
    done
  done;
  fmap

let run2 fn = 
  let f = read_file fn in
  let map = data_of_input f in
  let map = full_map map in
  process_cost map

let () =
  print_int (run2 "input");
  print_newline ()
