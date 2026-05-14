(*open Types*)

type container = {
  id: int;
  name: string ;
  occupied_voume: float;
  required_volume: float; (*in mL*)
  temperature: float;    (*in degrees c*)
  agitation: bool;     (*agitation state*)
}

type step =
  Init | Dispense of container * container  | Agitate (*toggle*) of container| ChangeTemp of container


module ContainerKey =
    struct
      type t = string
      let compare = compare
end

module ContainerMap = Map.Make(ContainerKey)

type state = {
  containers: container ContainerMap.t
}

type frame = {
  step: step;
  state: state;
  execution_time: float (*in seconds*)
}

type ir_program = {
  frames: frame list
}

let frame_empty : frame = {
  step = Init;
  state = {containers = ContainerMap.empty};
  execution_time = 0.0
}

let create_frame step state execution_time : frame =
  {
    step = step;
    state = state;
    execution_time = execution_time
  }

let generate_container_id map =
    if ContainerMap.is_empty map then 0
    else
        let _, last_container = ContainerMap.max_binding map in
        last_container.id + 1

let add_container_to_map occupied_voume required_volume map name =
  let container = {
    id = generate_container_id map;
    name = name;
    occupied_voume = occupied_voume;
    required_volume = required_volume;
    temperature = 25.0;
    agitation = false
  } in
    ContainerMap.add name container map

(* All three helpers now key by container.name, not string_of_int container.id *)
let agitate_container container map =
  let updated_container = {container with agitation = not container.agitation} in
  ContainerMap.add container.name updated_container map

let change_container_temp container new_temp map =
  let updated_container = {container with temperature = new_temp} in
  ContainerMap.add container.name updated_container map

let update_container_volume container (new_occupied_volume: float) map =
  let updated_container = {container with occupied_voume = new_occupied_volume} in
  ContainerMap.add container.name updated_container map

let env_to_frame ?(step = Init) ?(execution_time = 0.0) (env: Types.env) : frame =
  let option_or default = function
    | Some value -> value
    | None -> default
  in
  let add_solution_as_container map ((name, solution): string * Types.solution) =
    let occupied_volume = option_or 0.0 solution.Types.volume in
    let required_volume = occupied_volume in
    let container = {
      id = generate_container_id map;
      name = name;
      occupied_voume = occupied_volume;
      required_volume = required_volume;
      temperature = option_or 25.0 solution.Types.temperature;
      agitation = solution.Types.agitate
    } in
    ContainerMap.add name container map
  in
  let add_rv_as_container map ((name, rv): string * Types.rv) =
    let occupied_volume =
      match rv.Types.solution with
      | Some solution -> option_or 0.0 solution.Types.volume
      | None -> 0.0
    in
    let required_volume = option_or occupied_volume rv.Types.max_volume in
    let temperature =
      match rv.Types.temperature, rv.Types.solution with
      | Some t, _ -> t
      | None, Some solution -> option_or 25.0 solution.Types.temperature
      | None, None -> 25.0
    in
    let container = {
      id = generate_container_id map;
      name = name;
      occupied_voume = occupied_volume;
      required_volume = required_volume;
      temperature = temperature;
      agitation = rv.Types.agitate
    } in
    ContainerMap.add name container map
  in
  let containers_from_solutions =
    Types.SolutionMap.bindings env.Types.solutions
    |> List.fold_left add_solution_as_container ContainerMap.empty
  in
  let containers =
    Types.RVMap.bindings env.Types.rvs
    |> List.fold_left add_rv_as_container containers_from_solutions
  in
  create_frame step {containers = containers} execution_time


let generate_json_from_ir_program (program: ir_program) : string =
  let container_to_json (container: container) : Yojson.Basic.t =
    `Assoc [
      ("id", `Int container.id);
      ("name", `String container.name);
      ("occupied_volume", `Float container.occupied_voume);
      ("required_volume", `Float container.required_volume);
      ("temperature", `Float container.temperature);
      ("agitation", `Bool container.agitation)
    ]
  in
  let step_to_json (step: step) : Yojson.Basic.t =
    match step with
    | Init -> `String "Init"
    | Dispense (c1, c2) ->
        `Assoc [
          ("Dispense", `List [container_to_json c1; container_to_json c2])
        ]
    | Agitate c ->
        `Assoc [
          ("Agitate", container_to_json c)
        ]
    | ChangeTemp c ->
        `Assoc [
          ("ChangeTemp", container_to_json c)
        ]
  in
  let state_to_json (state: state) : Yojson.Basic.t =
    let containers_json =
      ContainerMap.bindings state.containers
      |> List.map (fun (name, container) ->
          (name, container_to_json container))
    in
    `Assoc containers_json
  in
  let frame_to_json (frame: frame) : Yojson.Basic.t =
    `Assoc [
      ("step", step_to_json frame.step);
      ("state", state_to_json frame.state);
      ("execution_time", `Float frame.execution_time)
    ]
  in
  let frames_json =
    List.map frame_to_json program.frames
  in
  let program_json = `Assoc [
    ("frames", `List frames_json)
  ] in
  Yojson.Basic.pretty_to_string program_json


let create_init_frame_if_empty (ir_prog : ir_program) : ir_program =
    if ir_prog.frames = [] then
        let init_frame = create_frame Init {containers = ContainerMap.empty} 0.0 in
        {frames = [init_frame]}
    else
        ir_prog

let current_map (ir_prog: ir_program) : container ContainerMap.t =
  List.fold_left
    (fun acc frame ->
      ContainerMap.union (fun _key _a b -> Some b) acc frame.state.containers)
    ContainerMap.empty
    ir_prog.frames
