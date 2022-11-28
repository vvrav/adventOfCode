module StringMap = Map.Make(struct type t = string let compare = compare end)

let read_one_line s =
  Scanf.sscanf s "%s %s bags contain %s@." (
    fun k1 k2 rest -> 
      let key = k1 ^ " " ^ k2 in
      (* key, String.split_on_char ',' rest *)
      if rest = "no other bags" then
        key, []
      else 
        let r =
          List.map
            (fun s -> Scanf.sscanf s " %d %s %s" (fun n k1 k2 -> (k1 ^ " " ^ k2, n)))
            (String.split_on_char ',' rest)
        in
        key, r
      )

let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line ic with
    | s -> 
        let key, value = read_one_line s in
        aux (StringMap.add key value acc)
    | exception End_of_file -> acc
  in
  let sm = aux StringMap.empty in
  close_in ic;
  sm

let string_of_map sm =
  StringMap.fold
    (fun key value acc ->
      let vals = List.fold_left (fun acc (i, n) -> acc ^ (string_of_int n) ^ " " ^ i ^ "; ") "" value in
      acc ^ key ^ " -> " ^ vals ^ "\n")
    sm
    ""

let rec is_shiny_container sm value =
  let containees = List.map (fun (a, _) -> a) value in
  List.mem "shiny gold" containees ||
  List.exists (fun k -> is_shiny_container sm (StringMap.find k sm)) containees

let count_sc sm =
  StringMap.fold
    (fun _ v acc -> if is_shiny_container sm v then acc + 1 else acc)
    sm
    0

let rec total_bags sm color =
  List.fold_left
    (fun acc (c, n) -> acc + n * (total_bags sm c))
    1
    (StringMap.find color sm)

(* dont forget to substract 1! (the shiny gold bag itself) *)

(* these solutions could probably be improved using memoization *)
