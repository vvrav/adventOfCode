let exec_one (x :: tl as acc) s =
  if String.starts_with ~prefix:"noop" s then x :: acc
  else Scanf.sscanf s "addx %d" (fun n -> 
    (x + n) :: x :: acc)

let exec input =
  List.rev
    (List.fold_left
      exec_one
      [1]
      input)

let process1 instrs =
  let states = exec instrs in
  let sum = ref 0 in
  List.iteri
    (fun i v ->
      let i = i + 1 in
      if i mod 40 = 20 then sum := !sum + i * v )
    states;
  !sum

let process2 instrs =
  let states = exec instrs in
  List.iteri
    (fun i v ->
      let x = i mod 40 in
      if i > 0 && x = 0 then print_newline ();
      if abs (x - v) < 2 then print_char '#' else print_char ' ')
  states
