

let process_one_stack_line stacks l =
  for i = 0 to Array.length stacks - 1 do
    match l.[i*4+1] with
    | ' ' -> ()
    | c -> stacks.(i) <- c :: stacks.(i)
  done

let process_input f =
  let rec input_cutter acc l =
    match l with
    | "" :: tl -> acc, tl
    | hd :: tl -> input_cutter (hd :: acc) tl
    | _ -> failwith "oopsie"
  in
  let rstacks, instrl = input_cutter [] f in
  let n, rstacks = match rstacks with
  | hd :: tl ->
      List.fold_left
        (fun acc s -> if s = "" then acc else int_of_string s)
        0
        (String.split_on_char ' ' hd),
      tl
    | _ -> failwith ("oopsie, failed to process: ")
  in
  let stacks = Array.make n [] in
  List.iter (process_one_stack_line stacks) rstacks;
  stacks,
  List.map
    (fun s -> Scanf.sscanf s "move %d from %d to %d" (fun a b c -> a, (b-1), (c-1)))
    instrl

(* remove top n crates from stack `col` and return them (keep order) *)
let pop stacks col n =
  let rec aux acc n =
    if n = 0 then acc else
    match stacks.(col) with
    | hd :: tl ->
      let nacc = hd :: acc in
      stacks.(col) <- tl;
      aux nacc (n-1)
    | _ -> failwith "pop error"
  in
  List.rev (aux [] n)

(* remove top n crates from stack `col` and return them (reverse order) *)
let pop2 stacks col n =
  let rec aux acc n =
    if n = 0 then acc else
    match stacks.(col) with
    | hd :: tl ->
      let nacc = hd :: acc in
      stacks.(col) <- tl;
      aux nacc (n-1)
    | _ -> failwith "pop error"
  in
  aux [] n

let print_tops stacks =
  Array.iter
    (fun l -> let top = match l with [] -> ' ' | c :: _ -> c in print_char top)
    stacks

let push stacks col crates =
  List.iter
    (fun c -> stacks.(col) <- c :: stacks.(col))
    crates

let exec_one stacks (n, orig, dest) =
  push stacks dest (pop stacks orig n);
  print_tops stacks;
  print_newline ()

let exec_all stacks instrl =
  List.iter (exec_one stacks) instrl;
  Array.iter
    (fun l -> let top = match l with [] -> ' ' | c :: _ -> c in print_char top)
    stacks;
  print_newline()

let exec_one2 stacks (n, orig, dest) =
  push stacks dest (pop2 stacks orig n);
  print_tops stacks;
  print_newline ()

let exec_all2 stacks instrl =
  List.iter (exec_one2 stacks) instrl;
  Array.iter
    (fun l -> let top = match l with [] -> ' ' | c :: _ -> c in print_char top)
    stacks;
  print_newline()
