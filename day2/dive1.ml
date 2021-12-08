exception ReadError

type instr =
 | Up of int
 | Forward of int
 | Down of int

let filename = "input"

let one_line l =
  match String.split_on_char ' ' l with
  | ["up"; n] -> Up(int_of_string n)
  | ["down"; n] -> Down(int_of_string n)
  | ["forward"; n] -> Forward(int_of_string n)
  | _ -> raise ReadError

let rec process ic h d =
  try
    let line = input_line ic in
    let i = one_line line in
    match i with
    | Up(x) -> process ic h (d-x)
    | Down(x) -> process ic h (d+x)
    | Forward(x) -> process ic (h+x) d
  with
  | End_of_file ->
    close_in ic;
    (h, d)

let rec process2 ic a h d =
    match one_line(input_line ic) with
    | Up(x) -> process2 ic (a-x) h d
    | Down(x) -> process2 ic (a+x) h d
    | Forward(x) -> process2 ic a (h+x) (d+a*x)
    | exception End_of_file -> close_in ic; (h, d)
    | exception e -> close_in ic; raise e
