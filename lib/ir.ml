open Types

type container = {
  id: int;
  name: string option;
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
    name = None;
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

let update_container_volume container (new_occupied_volume: float) map =
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


let rec eval_expr_for_ir (e: expression) (ir_prog: ir_program) : ir_program =
  match e with
  | Sequence (e1, e2) ->
      let ir_prog' = eval_expr_for_ir e1 ir_prog in
        eval_expr_for_ir e2 ir_prog'
  | Solution (s,_,_) ->
     let container = {
        id = generate_container_id (ContainerMap.empty);
        name = Some s;
        occupied_voume = 0.0;
        required_volume = 10.0; (*default volume for a solution*)
        temperature = 25.0;
        agitation = false
      } in
      let new_map = add_container_to_map container.occupied_voume container.required_volume ContainerMap.empty in
      let new_frame = create_frame (Dispense (container, container)) {containers = new_map} 1.0 in
      {frames = ir_prog.frames @ [new_frame]}
  | AASolution (s,_,_,_) ->
     let container = {
        id = generate_container_id (ContainerMap.empty);
        name = Some s;
        occupied_voume = 0.0;
        required_volume = 10.0; (*default volume for a solution*)
        temperature = 25.0;
        agitation = false
      } in
      let new_map = add_container_to_map container.occupied_voume container.required_volume ContainerMap.empty in
      let new_frame = create_frame (Dispense (container, container)) {containers = new_map} 1.0 in
      {frames = ir_prog.frames @ [new_frame]}
  | Agitate s ->
      let container = ContainerMap.find s (ContainerMap.empty) in
      let new_map = agitate_container container ContainerMap.empty in
      let new_frame = create_frame (Agitate container) {containers = new_map} 0.5 in
      {frames = ir_prog.frames @ [new_frame]}
  | ChangeTemp (s, temp) ->
        let container = ContainerMap.find s (ContainerMap.empty) in
        let new_map = change_container_temp container temp ContainerMap.empty in
        let new_frame = create_frame (ChangeTemp container) {containers = new_map} 1.0 in
        {frames = ir_prog.frames @ [new_frame]}
  | Mix (s1, s2, s3, _, _, v, _) ->
        let container1 = ContainerMap.find s1 (ContainerMap.empty) in
        let container2 = ContainerMap.find s2 (ContainerMap.empty) in
        let volume = match v with
          | Volume x -> x
          | VolumeParam _ -> 10.0  (* This should not happen after substitution *)
          | NoVolume -> 10.0 in
        let container3 = {
            id = generate_container_id (ContainerMap.empty);
            name = Some s3;
            occupied_voume = volume;
            required_volume = volume;
            temperature = 25.0;
            agitation = false
          } in
        let new_map = add_container_to_map container3.occupied_voume container3.required_volume ContainerMap.empty in
        let new_map' = update_container_volume container1 (container1.occupied_voume +. volume) new_map in
        let new_map'' = update_container_volume container2 (container2.occupied_voume +. volume) new_map' in
        let new_frame = create_frame (Dispense (container1, container3)) {containers = new_map''} 2.0 in
        {frames = ir_prog.frames @ [
            new_frame;
            create_frame (Dispense (container2, container3)) {containers = new_map''} 2.0
            ]}
  |  _ -> ir_prog


let print ir_prog =
  let json = generate_json_from_ir_program ir_prog in
  print_endline json
