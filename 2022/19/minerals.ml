type resource = {
  ore: int;
  clay: int;
  obsidian: int;
  geode: int;
}

type state = {
  time: int;
  resources: resource;
  robots: resource;
  max: resource;
  (* prev: state option; *)
}

type recipe = {
  r_ore: resource;
  r_clay: resource;
  r_obsidian: resource;
  r_geode: resource;
}

exception Finish of int

let mk_resource ?(ore = 0) ?(clay = 0) ?(obsidian = 0) ?(geode = 0) () =
  { ore; clay; obsidian; geode; }

let parse_line l =
  Scanf.sscanf
    l
    "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian."
    (fun id ore1 clay1 obs1 obs2 geo1 geo2 ->
      id,
      {
        r_ore = mk_resource ~ore:ore1 ();
        r_clay = mk_resource ~ore:clay1 ();
        r_obsidian = mk_resource ~ore:obs1 ~clay:obs2 ();
        r_geode = mk_resource ~ore:geo1 ~obsidian:geo2 ();
      })

let compute_dt diff prod =
  diff / prod + (if diff mod prod = 0 then 0 else 1)

let next r state =
  let res = ref [] in
  (* ore *)
  let dore = r.r_ore.ore - state.resources.ore in
  let dtore =
    if dore > 0
    then compute_dt dore state.robots.ore
    else 0
  in
  let dt = dtore + 1 in
  (if state.max.ore > state.robots.ore && dt <= state.time then
    let nstate = {
      time = state.time - dt;
      resources = {
        ore = state.resources.ore + state.robots.ore * dt - r.r_ore.ore;
        clay = state.resources.clay + state.robots.clay * dt;
        obsidian = state.resources.obsidian + state.robots.obsidian * dt;
        geode = state.resources.geode + state.robots.geode * dt;
      };
      robots = { state.robots with ore = state.robots.ore + 1};
      max = state.max;
      (* prev = Some state; *)
     } in
    res := nstate :: !res);
  (* clay *)
  let dore = r.r_clay.ore - state.resources.ore in
  let dtore =
    if dore > 0
    then compute_dt dore state.robots.ore
    else 0
  in
  let dt = dtore + 1 in
  (if state.max.clay > state.robots.clay && dt <= state.time then
    let nstate = {
      time = state.time - dt;
      resources = {
        ore = state.resources.ore + state.robots.ore * dt - r.r_clay.ore;
        clay = state.resources.clay + state.robots.clay * dt;
        obsidian = state.resources.obsidian + state.robots.obsidian * dt;
        geode = state.resources.geode + state.robots.geode * dt;
      };
      robots = { state.robots with clay = state.robots.clay + 1};
      max = state.max;
      (* prev = Some state; *)
     } in
    res := nstate :: !res);
  (* obsidian *)
  let dore = r.r_obsidian.ore - state.resources.ore in
  let dtore =
    if dore > 0
    then compute_dt dore state.robots.ore
    else 0
  in
  let dclay = r.r_obsidian.clay - state.resources.clay in
  let dtclay =
    if state.robots.clay = 0
    then 100
    else if dclay > 0
    then compute_dt dclay state.robots.clay
    else 0
  in
  let dt = (max dtore dtclay) + 1 in
  (if state.max.obsidian > state.robots.obsidian && dt <= state.time then
    let nstate = {
      time = state.time - dt;
      resources = {
        ore = state.resources.ore + state.robots.ore * dt - r.r_obsidian.ore;
        clay = state.resources.clay + state.robots.clay * dt - r.r_obsidian.clay;
        obsidian = state.resources.obsidian + state.robots.obsidian * dt;
        geode = state.resources.geode + state.robots.geode * dt;
      };
      robots = { state.robots with obsidian = state.robots.obsidian + 1};
      max = state.max;
      (* prev = Some state; *)
     } in
    res := nstate :: !res);
  (* geode *)
  let dore = r.r_geode.ore - state.resources.ore in
  let dtore =
    if dore > 0
    then compute_dt dore state.robots.ore
    else 0
  in
  let dobs = r.r_geode.obsidian - state.resources.obsidian in
  let dtobs =
    if state.robots.obsidian = 0
    then 100
    else if dobs > 0
    then compute_dt dobs state.robots.obsidian
    else 0
  in
  let dt = (max dtobs dtore) + 1 in
  (if dt <= state.time then
    let nstate = {
      time = state.time - dt;
      resources = {
        ore = state.resources.ore + state.robots.ore * dt - r.r_geode.ore;
        clay = state.resources.clay + state.robots.clay * dt;
        obsidian = state.resources.obsidian + state.robots.obsidian * dt - r.r_geode.obsidian;
        geode = state.resources.geode + state.robots.geode * dt;
      };
      robots = { state.robots with geode = state.robots.geode + 1};
      max = state.max;
      (* prev = Some state; *)
     } in
    res := nstate :: !res);
  !res

let process_one_recipe time r =
  let max_resources = {
    ore = List.fold_left max 0 [r.r_ore.ore; r.r_clay.ore; r.r_obsidian.ore; r.r_geode.ore];
    clay = r.r_obsidian.clay;
    obsidian = r.r_geode.obsidian;
    geode = 0
  } in
  let start_state = {
    time;
    resources = mk_resource ();
    robots = mk_resource ~ore:1 ();
    max = max_resources;
    (* prev = None; *)
  } in
  let stack = Stack.create () in
  Stack.push start_state stack;
  let res = ref [] in
  while not (Stack.is_empty stack) do
    let st = Stack.pop stack in
    match next r st with
    | [] -> 
      let ng = st.resources.geode + st.time * st.robots.geode in
      (* res := (ng, st) :: !res *)
      if ng > 0 then
      res := ng :: !res
    | l -> List.iter (fun s -> Stack.push s stack) l
  done;
  (* let score, st =
    List.fold_left
      (fun ((ascore, astate) as acc) ((score, state) as x) -> if ascore > score then acc else x)
      (0, start_state)
      !res
  in
  score *)
  List.fold_left max 0 !res

(* let rec unfold_history state =
  match state.prev with
  | Some p -> {state with prev = None} :: (unfold_history p)
  | None -> [{state with prev = None}] *)

let process1 l =
  List.fold_left
    (fun acc (id, r) -> acc + id * (process_one_recipe 24 r))
    0
    l


let () =
  let ic = open_in "example" in
  let rec aux acc =
    match input_line ic with
    | s -> aux (parse_line s :: acc)
    | exception End_of_file -> List.rev acc
  in
  let input = aux [] in
  close_in ic;
  let score = process_one_recipe 32 (snd (List.nth input 0)) in
  print_int score;
  print_newline ()
