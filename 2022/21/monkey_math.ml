module StringMap = Map.Make(struct type t = string let compare = compare end)
type job = Num of int | Op of char * string * string | Human

type jobtree = TNum of int | TOp of (char * jobtree * jobtree) | TX

let parse_line acc l =
  let id, res = Scanf.sscanf l "%s@: %s@\n" (fun id res -> id, res) in
  let job = match int_of_string_opt res with
  | Some n -> Num n
  | None -> Scanf.sscanf res "%s %c %s" (fun a op b -> Op (op, a, b))
  in
  StringMap.add id job acc


let get_input filename =
  let ic = open_in filename in
  let rec aux acc =
    match input_line ic with
    | s -> aux (parse_line acc s)
    | exception End_of_file -> acc
  in
  let s = aux StringMap.empty in
  close_in ic;
  s

let op_of_char = function
| '+' -> (+)
| '-' -> (-)
| '/' -> (/)
| '*' -> ( * )
| _ -> failwith "incorrect op"

let process input =
  let map = ref input in
  let stack = Stack.create () in
  Stack.push "root" stack;
  while not (Stack.is_empty stack) do
    let id = Stack.pop stack in
    match StringMap.find id !map with
    | Op (op, a, b) -> begin
      match StringMap.find a !map, StringMap.find b !map with
      | Num na, Num nb -> map := StringMap.add id (Num ((op_of_char op) na nb)) !map
      | Op _, Num _ -> Stack.push id stack; Stack.push a stack
      | Num _, Op _ -> Stack.push id stack; Stack.push b stack
      | _ -> Stack.push id stack; Stack.push a stack; Stack.push b stack
      end
    | Num _ -> ()
    | Human -> failwith "nope"
  done;
  StringMap.find "root" !map

let rec solve tree n =
  match tree with
  | TX -> n
  | TOp ('+', TNum x, st)
  | TOp ('+', st, TNum x) -> solve st (n-x)
  | TOp ('-', TNum x, st) -> solve st (x-n)
  | TOp ('-', st, TNum x) -> solve st (x+n)
  | TOp ('*', TNum x, st)
  | TOp ('*', st, TNum x) -> solve st (n/x)
  | TOp ('/', TNum x, st) -> solve st (x/n)
  | TOp ('/', st, TNum x) -> solve st (x*n)
  | _ -> failwith "???"

let process2 input =
  let input = StringMap.update "root" (function Some (Op (_, a, b)) -> Some (Op ('=', a, b)) | _ -> failwith "!") input in
  let rec aux id =
    if id = "humn" then TX
    else match StringMap.find id input with
    | Op ('=', a, b) ->
      let ta = aux a in
      let tb = aux b in
      TOp ('=', ta, tb)
    | Op (c, a, b) ->
      let ta = aux a in
      let tb = aux b in
      (match ta, tb with
      | TNum na, TNum nb -> TNum ((op_of_char c) na nb)
      | _ -> TOp (c, ta, tb))
    | Num n -> TNum n
    | _ -> failwith "?"
  in
  match aux "root" with
  | TOp (_, TNum x, st) | TOp (_, st, TNum x) -> solve st x
