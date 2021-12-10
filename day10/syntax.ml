exception SyntaxError of char

let openings = ['{';'(';'[';'<']
let closings = ['}';')';']';'>']

let matching_parr p =
  match p with
  | '>' -> '<'
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | c -> raise (SyntaxError c)

let get_score p =
  match p with
  | '>' -> 25137
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | c -> raise (SyntaxError c)

let get_compl_score = function
  | '(' -> 1
  | '[' -> 2
  | '{' -> 3
  | '<' -> 4
  | c -> raise (SyntaxError c)

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> acc
  in
  let ls = aux [] in
  close_in ic;
  ls

let read_line l =
  let pile = ref [] in
  String.iter
    (fun c ->
      if List.mem c openings then
        pile := c :: !pile
      else if List.mem c closings && List.hd !pile = matching_parr c then
        pile := List.tl !pile
      else
        raise (SyntaxError c))
    l;
  !pile

let run1 fn =
  let f = read_file fn in
  let score = ref 0 in
  List.iter
    (fun l ->
       try
         ignore (read_line l)
       with SyntaxError c -> score := !score + get_score c)
    f;
  !score

let run2 fn =
  let f = read_file fn in
  let scores = ref [] in
  List.iter
    (fun l ->
      try
        let pl = read_line l in
        List.iter print_char pl; print_newline ();
        let sc = List.fold_left (fun acc p -> acc * 5 + get_compl_score p) 0 pl in
        scores := sc :: !scores
      with SyntaxError c -> ())
    f;
  List.nth (List.sort compare !scores) ((List.length !scores)/2)
