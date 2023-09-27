module StringMap = Map.Make(struct type t = string let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

let get_input process filename =
  let ic = open_in filename in
  let rec aux acc =
    match input_line ic with
    | s -> aux ((process s) :: acc)
    | exception End_of_file -> List.rev acc
  in
  let s = aux [] in
  close_in ic;
  s

let parse l =
    Scanf.sscanf
      l
      "Valve %[A-Z] has flow rate=%d; tunnel%_c lead%_c to valve%_c %s@\n"
      (fun name rate rest ->
        name,
        rate,
        rest |> String.split_on_char ',' |> (List.map String.trim)
      )

let get_raw_tunnels_map l =
  List.fold_left
    (fun acc (n, _, l) -> StringMap.add n l acc)
    StringMap.empty
    l

let get_valves l =
  List.filter_map (fun (n, r, _) -> if r > 0 then Some (n, r) else None) l

let get_costs_from_room room tunnels =
  let costs = ref StringMap.empty in
  let queue = Queue.create () in
  Queue.add (room, 0) queue;
  while not (Queue.is_empty queue) do
    let r, c = Queue.take queue in
    if not (StringMap.mem r !costs) then (
      costs := StringMap.add r c !costs;
      List.iter (fun d -> Queue.add (d, c+1) queue) (StringMap.find r tunnels)
    )
  done;
  !costs

let get_tc_valves tunnels valves =
  let n = List.length valves in
  let names, rates = List.split valves in
  let name_of_id = Array.of_list names in
  let rate_of_id = Array.of_list rates in
  let id_of_name = Seq.fold_left (fun acc (i, n) -> StringMap.add n i acc) StringMap.empty (Array.to_seqi name_of_id) in
  let res = Array.make_matrix n n (-1) in
  Array.iteri
    (fun id r ->
      let costs = get_costs_from_room r tunnels in
      Array.iteri (fun did dname -> res.(id).(did) <- StringMap.find dname costs) name_of_id)
    name_of_id;
  res, id_of_name, rate_of_id

let process input =
  let valves = get_valves input in
  let tunnels = get_raw_tunnels_map input in
  let travels, id_of_name, rates = get_tc_valves tunnels (("AA", 0) :: valves) in (* "AA" will be pos 0 *)
  let valve_ids = List.map (fun (n, _) -> StringMap.find n id_of_name) valves |> IntSet.of_list in
  let scores = ref [] in
  let stack = Stack.create () in
  Stack.push (0, 30, valve_ids, 0) stack;
  while not (Stack.is_empty stack) do
    let pos, time, closed, score = Stack.pop stack in
    let possibilities = IntSet.fold
      (fun d acc -> let rem = time - travels.(pos).(d) - 1 in if rem > 0 then (d, rem) :: acc else acc)
      closed
      []
    in
    if possibilities = [] then scores := score :: !scores
    else List.iter
      (fun (d, rem) -> Stack.push (d, rem, IntSet.remove d closed, score + rem * rates.(d)) stack)
      possibilities
  done;
  List.fold_left max 0 !scores
  
  let process2 input =
    (* init *)
    let valves = get_valves input in
    let tunnels = get_raw_tunnels_map input in
    let travels, id_of_name, rates = get_tc_valves tunnels (("AA", 0) :: valves) in (* "AA" will be pos 0 *)
    let valve_ids = List.map (fun (n, _) -> StringMap.find n id_of_name) valves |> IntSet.of_list in
    let scores = ref [] in
    (* use a stack? *)
    let stack = Stack.create () in
    Stack.push ((0, 26), (0, 26), valve_ids, 0) stack;
    while not (Stack.is_empty stack) do
      let (pos1, time1), (pos2, time2), closed, score = Stack.pop stack in
      if time1 <= 0 && time2 <= 0 then scores := score :: !scores
      else (
      (* lets consider the one with the most time first *)
      let cpos, ctime, opos, otime = if time1 >= time2 then pos1, time1, pos2, time2 else pos2, time2, pos1, time1 in
      let possibilities = IntSet.fold
        (fun d acc -> let rem = ctime - travels.(cpos).(d) - 1 in if rem > 0 then (d, rem) :: acc else acc)
        closed
        []
      in
      (* special case: maybe the second worker has still smth to do! *)
      if possibilities = [] then Stack.push ((cpos, 0), (opos, otime), closed, score) stack
      else
        List.iter
          (fun (d, rem) -> Stack.push ((d, rem), (opos, otime), IntSet.remove d closed, score + rem * rates.(d)) stack)
          possibilities)
    done;
    List.fold_left max 0 !scores

let () =
    let input = get_input parse "input" in
    let res = process2 input in
    print_int res;
    print_newline ()
