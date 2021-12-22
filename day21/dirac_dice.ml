
module IPMap = Map.Make (
  struct
     type t = (int * int) * (int * int) 
     let compare ((_, as1), (_, as2) as a) ((_, bs1), (_, bs2) as b) =
       compare (min as1 as2, a) (min bs1 bs2, b)
  end)

let push m n a b =
  m := IPMap.update
    (a, b)
    (function
      | Some x -> Some (n+x)
      | None -> Some n)
    !m

let pop m =
  let key, v = IPMap.min_binding !m in
  m := IPMap.remove key !m;
  key, v

let ddice, reset_dd, total_dd =
  let n = ref 0 in
  (fun () ->
    incr n;
    (!n-1) mod 100 + 1),
  (fun () -> n := 0),
  (fun () -> !n)


let dirac = [
  3, 1;
  4, 3;
  5, 6;
  6, 7;
  7, 6;
  8, 3;
  9, 1;
]

let one_turn pos score =
  let moves = ddice () + ddice () + ddice () in
  let npos = ((pos + moves - 1) mod 10) + 1 in
  npos, score + npos


let play pos1 pos2 =
  reset_dd ();
  let rec aux pos1 score1 pos2 score2 =
    let npos1, nscore1 = one_turn pos1 score1 in
    if nscore1 >= 1000 then
      score2 * total_dd()
    else
      aux pos2 score2 npos1 nscore1
  in
  aux pos1 0 pos2 0
    
let play2 pos1 pos2 =
  let m = ref IPMap.empty in
  let win1 = ref 0 in
  let win2 = ref 0 in
  push m 1 (pos1, 0) (pos2, 0);
  while not (IPMap.is_empty !m) do
    let ((p1, s1), (p2, s2)), n = pop m in
    List.iter
      (fun (d1, n1) ->
        let np1 = ((p1 + d1 - 1) mod 10) + 1 in
        let ns1 = s1 + np1 in
        if ns1 >= 21 then
          win1 := !win1 + n * n1
        else
          List.iter
            (fun (d2, n2) ->
              let np2 = ((p2 + d2 - 1) mod 10) + 1 in
              let ns2 = s2 + np2 in
              if ns2 >= 21 then
                win2 := !win2 + n * n1 * n2
              else
                push m (n*n1*n2) (np1, ns1) (np2, ns2))
            dirac)
      dirac
  done;
  !win1, !win2

let () =
  let s1, s2 = play2 7 4 in
  Printf.printf "%d ; %d\n" s1 s2;
  if s1 > s2 then
    print_endline "Player 1 win!"
  else
    print_endline "Player 2 win!"