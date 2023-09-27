type monkey = {
  id: int;
  objects: int Queue.t;
  operation : int -> int;
  test : int;
  target_true : int;
  target_false : int;
}


let parse_monkey ic =
  (* monkey id *)
  let id = Scanf.sscanf (input_line ic) "Monkey %d:" (fun i -> i) in
  (* starting objects *)
  let objl =
    Scanf.sscanf 
      (input_line ic)
      " Starting items: %s@\n"
      (fun s ->
        s |> String.split_on_char ','
          |> List.map String.trim
          |> List.map int_of_string )
  in
  let objects = Queue.create () in
  List.iter (fun o -> Queue.add o objects) objl;
  (* operation *)
  let operation =
    Scanf.sscanf
      (input_line ic)
      " Operation: new = old %c %s"
      (fun c n ->
        let op = match c with | '+' -> (+) | '*' -> ( * ) | _ -> failwith "oopsie" in
        if n = "old" then fun o -> op o o
        else op (int_of_string n))
  in
  (* test *)
  let test = Scanf.sscanf
    (input_line ic)
    " Test: divisible by %d"
    (fun n -> n)
  in
  (* targets *)
  let target_true = Scanf.sscanf (input_line ic) " If true: throw to monkey %d" (fun i -> i) in
  let target_false = Scanf.sscanf (input_line ic) " If false: throw to monkey %d" (fun i -> i) in
  { id; objects; operation; test; target_true; target_false; }

let get_input fn =
  let ic = open_in fn in
  let monkey_list = ref [] in
  (try while true do
    let monkey = parse_monkey ic in
    monkey_list := monkey :: !monkey_list;
    ignore (input_line ic)
  done;
  with End_of_file -> ());
  let sml = List.sort (fun m1 m2 -> compare m1.id m2.id) !monkey_list in
  let monkey_array = Array.of_list sml in
  monkey_array

let run1 monkeys =
  let n = Array.length monkeys in
  let count = Array.make n 0 in
  let addc i = count.(i) <- count.(i) + 1 in
  for round = 1 to 20 do
    Printf.printf "Round %d\n" round;
    for m = 0 to n - 1 do
      let current = monkeys.(m) in
      Printf.printf "Monkey %d\n" m;
      while not (Queue.is_empty current.objects) do
        let o = Queue.take current.objects in
        Printf.printf "  inspects an item with a worry level of %d\n" o;
        let no = (current.operation o) / 3 in
        Printf.printf "    new worry level: %d\n" no;
        let target =
          if no mod current.test = 0
          then (
            Printf.printf "    condition satisfied!";
            current.target_true)
          else (
            Printf.printf "    condition not satisfied!";
            current.target_false)
        in
        Printf.printf " send to monkey %d\n" target;
        Queue.push no monkeys.(target).objects;
        addc m
      done
    done;
    Printf.printf "\nEnd of the round\n";
    Array.iter
    (fun m -> let objs =
      Queue.fold
        (fun acc o -> acc ^ ", " ^ (string_of_int o))
        (Printf.sprintf "Monkey %d" m.id)
        m.objects in 
      Printf.printf "%s\n" objs)
      monkeys;
    print_newline () 
  done;
  Array.sort compare count;
  count.(n - 1) * count.(n - 2)

let run2 monkeys =
  let n = Array.length monkeys in
  let count = Array.make n 0 in
  let addc i = count.(i) <- count.(i) + 1 in
  let ppcm = Array.fold_left (fun acc m -> acc * m.test) 1 monkeys in
  for round = 1 to 10000 do
    for m = 0 to n - 1 do
      let current = monkeys.(m) in
      while not (Queue.is_empty current.objects) do
        let o = Queue.take current.objects in
        let no = (current.operation o) mod ppcm in
        let target =
          if no mod current.test = 0
          then current.target_true
          else current.target_false
        in
        Queue.push no monkeys.(target).objects;
        addc m
      done
    done;
    if round mod 1000 = 0 then (
    Printf.printf "Round %d\n" round;
    let count_s = Array.fold_left (fun acc c -> acc ^ ", " ^ (string_of_int c)) "" count in
    print_string count_s;
    print_newline ()
    )
  done;
  Array.sort compare count;
  count.(n - 1) * count.(n - 2)
  