module IntSet = Set.Make(struct type t = int let compare = compare end)

exception Loop of int * IntSet.t

type instruction =
| ACC of int
| JMP of int
| NOP of int

let inst_of_string s =
  Scanf.sscanf
    s
    "%s %d"
    (fun i v -> match i with
    | "acc" -> ACC v
    | "nop" -> NOP v
    | "jmp" -> JMP v
    | _ -> failwith i)

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux ((inst_of_string s) :: acc)
    | exception End_of_file -> List.rev acc
  in
  let s = Array.of_list (aux []) in
  close_in ic;
  s

let rec exec acc n code seen =
  if n >= Array.length code then acc else
  if IntSet.mem n seen then raise (Loop (acc, seen)) else
  let seen = IntSet.add n seen in
  match code.(n) with
  | ACC v -> exec (acc+v) (n+1) code seen
  | JMP v -> exec acc (n+v) code seen
  | NOP _ -> exec acc (n+1) code seen

let swap prog n =
  match prog.(n) with
  | NOP v -> prog.(n) <- JMP v
  | JMP v -> prog.(n) <- NOP v
  | _ -> failwith "IMPOSSSSSIBLE SWAP"

let process prog =
  let seen =
    try
      ignore (exec 0 0 prog IntSet.empty);
      failwith "oops";
    with
      Loop (n, seen) ->
        Printf.printf "Looped! acc = %d\n" n;
        seen
  in
  let swappable =
    IntSet.filter
      (fun n -> match prog.(n) with NOP n | JMP n -> true | _ -> false)
      seen
  in
  IntSet.iter
    (fun n ->
      try
        swap prog n;
        let res = exec 0 0 prog IntSet.empty in
        Printf.printf "Terminated! acc = %d\n" res;
        swap prog n
      with
        Loop _ -> swap prog n
      )
      swappable;
