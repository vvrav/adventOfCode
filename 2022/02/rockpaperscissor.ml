type choice =
| Rock
| Paper
| Scissors

type outcome =
| Win
| Draw
| Lose

let choice_of_char = function
| 'A' | 'X' -> Rock
| 'B' | 'Y' -> Paper
| 'C' | 'Z' -> Scissors
| c -> failwith ("unrecognized: " ^ (String.make 1 c))

let outcome_of_char = function
| 'X' -> Lose
| 'Y' -> Draw
| 'Z' -> Win
| c -> failwith ("unrecognized: " ^ (String.make 1 c))

let outcome_of_choices = function
| a, b when a = b -> Draw
| Rock, Scissors | Scissors, Paper | Paper, Rock -> Lose
| _ -> Win

let choice_of_outcome = function
| c, Draw -> c
| Scissors, Lose | Rock, Win -> Paper
| Rock, Lose | Paper, Win -> Scissors
| Paper, Lose | Scissors, Win -> Rock

let process_one_line s =
  Scanf.sscanf s "%c %c" (fun a b -> choice_of_char a, choice_of_char b)

let process_one_line2 s =
  Scanf.sscanf s "%c %c" (fun a b -> choice_of_char a, outcome_of_char b)

let score_of_round (a, b) =
  let outcome = outcome_of_choices (a, b) in
  let s1 = match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0
 in
 let s2 = match b with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  in
  s1 + s2

let score_of_round2 (a, outcome) =
  let b = choice_of_outcome (a, outcome) in
  let s1 = match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0
 in
 let s2 = match b with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  in
  s1 + s2

let total_score score_function =
  List.fold_left (fun acc x -> acc + (score_function x) ) 0