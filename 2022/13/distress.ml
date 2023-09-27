type packet =
| Int of int
| List of packet list

let parse s = 
  let stack = Stack.create () in
  let i = ref 0 in
  let l = String.length s in
  while !i < l do
    if s.[!i] = ',' then incr i
    else if s.[!i] = '[' then begin
      Stack.push (List []) stack;
      incr i
    end
    else if s.[!i] = ']' then begin
      let closed = match Stack.pop stack with | List l -> List.rev l | _ -> failwith "not a list" in
      (match Stack.pop_opt stack with
      | Some (List l) -> Stack.push (List (List closed :: l)) stack
      | Some _ -> failwith "not a list (2)"
      | None -> if  !i = l -1 then Stack.push (List closed) stack else failwith "reached the end?");
      incr i
    end
    else begin
      let parent = match Stack.pop stack with | List l -> l | _ -> failwith "parent not a list" in
      Scanf.sscanf (String.sub s !i (l - !i)) "%[0-9]" (fun num ->
        i := !i + (String.length num);
        Stack.push (List (Int (int_of_string num) :: parent)) stack)
    end
  done;
  Stack.pop stack

let rec string_of_packet p =
  match p with
  | Int i -> string_of_int i
  | List l ->
    "[" ^ (String.concat "," (List.map string_of_packet l)) ^ "]"

let rec compare_packets p1 p2 =
  if p1 = p2 then 0 else
  match p1, p2 with
  | Int i1, Int i2 -> compare i1 i2
  | List [], _ -> (-1)
  | _, List [] -> 1
  | List (h1 :: t1), List (h2 :: t2) ->
    let c = compare_packets h1 h2 in
    if c = 0 then compare_packets (List t1) (List t2)
    else c
  | Int _, _ -> compare_packets (List [p1]) p2
  | _, Int _ -> compare_packets p1 (List [p2])

let get_input fn =
  let ic = open_in fn in
  let pairs = ref [] in
  (try
    while true do
      let p1 = parse (input_line ic) in
      let p2 = parse (input_line ic) in
      pairs := (p1, p2) :: !pairs;
      ignore(input_line ic)
    done
  with End_of_file -> ());
  List.rev !pairs

let process1 pairs =
  let sum = ref 0 in
  List.iteri
    (fun i (x,y) -> if compare_packets x y < 0 then sum := !sum + i + 1)
    pairs;
  !sum

let process2 pairs =
  (* didn't want to change the parsing process, so extract full list from list of pairs *)
  let packets = List.fold_left (fun acc (x,y) -> x :: y :: acc) [] pairs in
  (* add divider packets *)
  let p2 = List [List [Int 2]] in
  let p6 = List [List [Int 6]] in
  let packets =  p2 :: p6 :: packets in
  let spackets = List.sort compare_packets packets in
  let res = ref 1 in
  List.iteri
    (fun i p -> if p = p2 || p = p6 then res := !res * (i+1))
    spackets;
  !res
