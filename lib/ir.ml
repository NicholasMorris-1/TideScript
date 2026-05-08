(*open Types*)

type container = {
  id: int;
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

(*let ir_program : ir_program = {
  frames = []
  }*)

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

let add_container_to_map occupied_voume required_volume map =
  let container = {
    id = generate_container_id map;
    occupied_voume = occupied_voume;
    required_volume = required_volume;
    temperature = 25.0;
    agitation = false
  } in
    ContainerMap.add (string_of_int container.id) container map

let agitate_container container map =
  let updated_container = {container with agitation = not container.agitation} in
  ContainerMap.add (string_of_int container.id) updated_container map

let change_container_temp container new_temp map =
  let updated_container = {container with temperature = new_temp} in
  ContainerMap.add (string_of_int container.id) updated_container map

let update_container_volume container new_occupied_volume map =
  let updated_container = {container with occupied_voume = new_occupied_volume} in
  ContainerMap.add (string_of_int container.id) updated_container map


let generate_json_from_ir_program (program: ir_program) : string =
  let container_to_json (container: container) : Yojson.Basic.t =
    `Assoc [
      ("id", `Int container.id);
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







(*here we eval the expressions to update the intermediate representation,
  in the ir we do not care about molecules, concentrations, etc *)
