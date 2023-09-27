exception Found of int

let map_of_sl sl =
  let width = String.length (List.hd sl) in
  let height = List.length sl in
  let start = ref (0, 0) in
  let goal = ref (0, 0) in
  let map = Array.make_matrix width height 0 in
  List.iteri
    (fun y s ->
      String.iteri
        (fun x c -> 
          if c = 'S' then (start := (x,y); map.(x).(y) <- 0)
          else if c = 'E' then (goal := (x,y); map.(x).(y) <- 25)
          else map.(x).(y) <- int_of_char c - int_of_char 'a')
        s)
    sl;
  map, !start, !goal

let process map ?start goal =
  let width = Array.length map in
  let height = Array.length map.(0) in
  let cost = Array.make_matrix width height None in
  let queue = Queue.create () in
  (match start with
  | None -> Array.iteri (fun x a -> Array.iteri (fun y v -> if v = 0 then Queue.add ((x,y), 0) queue) a) map
  | Some s -> Queue.push (s, 0) queue);
  let res = try
    while not (Queue.is_empty queue) do
      let (x, y), c = Queue.pop queue in
      if cost.(x).(y) = None then begin
        if (x,y) = goal then raise (Found c);
        cost.(x).(y) <- Some c;
        let alt = map.(x).(y) in
        if x > 0 && map.(x-1).(y) - alt <= 1 then Queue.add ((x-1,y), c+1) queue;
        if x < width - 1 && map.(x+1).(y) - alt <= 1 then Queue.add ((x+1,y), c+1) queue;
        if y > 0 && map.(x).(y-1) - alt <= 1 then Queue.add ((x,y-1), c+1) queue;
        if y < height - 1  && map.(x).(y+1) - alt <= 1 then Queue.add ((x,y+1), c+1) queue
      end
    done;
    -1
  with Found n ->  n
  in
  cost, res
