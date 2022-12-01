let parse_and_group sl =
  List.fold_left
    (fun acc s ->
      match s, acc with
      | "", _ ->  []::acc
      | s, hd::tl -> ((int_of_string s)::hd)::tl
      | _ -> failwith "oopsie")
    [[]]
    sl

let get_max_total cl =
  List.fold_left
    (fun acc x -> max acc (List.fold_left (+) 0 x))
    0
    cl

let get_top_three_sum cl =
  let summedl = List.map (List.fold_left (+) 0) cl in
  let ssummedl = List.sort (fun x y -> compare y x) summedl in
  match ssummedl with
  | a :: b :: c :: _ -> a + b +c
  | _ -> failwith "oopsie"
