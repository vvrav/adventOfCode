
exception Invalid of int

exception Cont of int Queue.t
let preamble_size = 25
let last = preamble_size - 1

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux ((int_of_string s) :: acc)
    | exception End_of_file -> List.rev acc
  in
  let s = aux [] in
  close_in ic;
  s

let find_invalid s =
  let a = Array.make preamble_size (-1) in
  let push n =
    Array.blit a 0 a 1 last;
    a.(0) <- n
  in
    try
      List.iter
        (fun n ->
          (* init phase: just push it *)
          if a.(last) = -1 then
            push n
          else if Array.exists (fun x -> Array.exists (fun y -> x <> y && x + y = n) a) a
          then push n
          else raise (Invalid n))
        s;
        0
    with 
    | Invalid n -> Printf.printf "Found invalid number: %d" n; n
    | End_of_file -> Printf.printf "no invalid num?"; 0


let find_contiguous s invalid =
  let queue = Queue.create () in
  try
    List.iter
      (fun i ->
        Queue.push i queue;
        while Queue.fold (+) 0 queue > invalid do
          ignore(Queue.pop queue)
        done;
        if Queue.length queue > 1 && 
          Queue.fold (+) 0 queue = invalid
        then
          raise (Cont queue) 
        )
      s;
    0
  with Cont q ->
    let m1 = Queue.fold min max_int q in
    let m2 = Queue.fold max 0 q in
    m1 + m2
