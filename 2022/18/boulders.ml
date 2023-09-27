module FaceMap = Map.Make(struct type t = int * int * int * char let compare = compare end)
module BlockSet = Set.Make(struct type t = int * int * int let compare = compare end)

let parse_line s =
  Scanf.sscanf s "%d,%d,%d" (fun x y z -> x,y,z)

let faces_of_cube x y z = [
  x, y, z, 'X';
  x, y, z, 'Z';
  x, y, z, 'Y';
  x+1, y, z, 'X';
  x, y+1, z, 'Y';
  x, y, z+1, 'Z'; 
]

let add_face map face =
  FaceMap.update
    face
    (function None -> Some 1 | Some n -> Some (n+1))
    map

let add_cube map (x,y,z) =
  List.fold_left
    add_face
    map
    (faces_of_cube x y z)

let count_ext_faces map =
  FaceMap.fold (fun _ n sum -> if n = 1 then sum + 1 else sum) map 0



(* let neighbours (x, y, z, d) =
  match d with
  | 'X' -> [
      x, y-1, z, 'X'; x, y, z-1, 'X'; x, y+1, z, 'X'; x, y, z+1, 'X';
      x-1, y, z, 'Y'; x, y, z, 'Y'; x-1, y+1, z, 'Y'; x, y+1, z, 'Y';
      x-1, y, z, 'Z'; x, y, z, 'Z'; x-1, y, z+1, 'Z'; x, y, z+1, 'Z';
  ]
  | 'Y' -> [
      x-1, y, z, 'Y'; x+1, y, z, 'Y'; x, y, z-1, 'Y'; x, y, z+1, 'Y';
      x, y-1, z, 'X'; x, y, z, 'X'; x+1, y-1, z, 'X'; x+1, y, z, 'X';
      x, y-1, z, 'Z'; x, y, z, 'Z'; x, y-1, z+1, 'Z'; x, y, z+1, 'Z';
  ]
  | 'Z' -> [
      x-1, y, z, 'Z'; x+1, y, z, 'Z'; x, y-1, z, 'Z'; x, y+1, z, 'Z';
      x, y, z, 'X'; x, y, z-1, 'X'; x+1, y, z, 'X'; x+1, y, z-1, 'X';
      x, y, z, 'Y'; x, y, z-1, 'Y'; x, y+1, z, 'Y'; x, y+1, z-1, 'Y';
  ]
  | _ -> failwith "oopsie"

let count_ext_faces2 map =
  let stack = Stack.create () in
  let start, _ = FaceMap.min_binding map in
  let ext = ref FaceSet.empty in
  Stack.push start stack;
  while not (Stack.is_empty stack) do
    let c = Stack.pop stack in
    if not (FaceSet.mem c !ext) && (FaceMap.find_opt c map = Some 1) then (
      ext := FaceSet.add c !ext;
      List.iter (fun x -> Stack.push x stack) (neighbours c)
    )
  done;
  !ext *)

let get_extrems input =
  let x0, x1 = ref max_int, ref min_int in
  let y0, y1 = ref max_int, ref min_int in
  let z0, z1 = ref max_int, ref min_int in
  List.iter
    (fun (x, y, z) ->
        if x < !x0 then x0 := x;
        if x > !x1 then x1 := x;
        if y < !y0 then y0 := y;
        if y > !y1 then y1 := y;
        if z < !z0 then z0 := z;
        if z > !z1 then z1 := z;)
    input;
  !x0-1, !x1+1, !y0-1, !y1+1, !z0-1, !z1+1

let get_neighbours (x,y,z) = [
  x-1, y, z;
  x+1, y, z;
  x, y-1, z;
  x, y+1, z;
  x, y, z-1;
  x, y, z+1;
]


let process2 input =
  let x0, x1, y0, y1, z0, z1 = get_extrems input in
  let is_valid (x,y,z) =
    x >= x0 && x<= x1 && y >= y0 && y <= y1 && z >= z0 && z <= z1
  in
  let blocks = List.fold_left (fun s x -> BlockSet.add x s) BlockSet.empty input in
  let res = ref BlockSet.empty in
  let queue = Queue.create () in
  Queue.add (x0, y0, z0) queue;
  while not (Queue.is_empty queue) do
    let b = Queue.take queue in
    if not (BlockSet.mem b blocks) && not (BlockSet.mem b !res) && is_valid b then (
      res := BlockSet.add b !res;
      List.iter (fun x -> Queue.add x queue) (get_neighbours b)
    )
  done ;
  let faces =
    BlockSet.fold
      (fun x acc -> add_cube acc x)
      !res
      FaceMap.empty
  in
  let n = count_ext_faces faces in
  let dx, dy, dz = x1-x0+1, y1-y0+1, z1-z0+1 in
  n - 2*dx*dy - 2*dx*dz - 2*dy*dz

