[@@@ocaml.warning "-33"]
open Graph
open Types

type containter_kind =
  | Resevoir | Reactor | Mixer | Waste


type container = {
    id: int;
    description: string;
    kind: containter_kind;
    agitator: bool;
    heater: bool;
    volume: float option;
    pump: bool;
    occupied_volume: float option;
    required_volume: float option;
  }

module Container = struct
  type t = container

  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end


module ContainerGraph = Graph.Imperative.Digraph.Concrete(Container)

let make_container ~id ~description ~kind ~agitator ~heater ~volume ~pump
    ~occupied_volume ~required_volume : container =
  {
    id;
    description;
    kind;
    agitator;
    heater;
    volume;
    pump;
    occupied_volume;
    required_volume;
  }

let env_to_hardware_graph (env : env) : ContainerGraph.t =
  let graph = ContainerGraph.create () in
  let next_id = ref 0 in
  let fresh_id () =
    incr next_id;
    !next_id
  in
  let add_vertex container =
    ContainerGraph.add_vertex graph container;
    container
  in
  let add_reservoir ~description ~agitate ~volume ~required_volume ~temperature =
    make_container
      ~id:(fresh_id ())
      ~description
      ~kind:Resevoir
      ~agitator:agitate
      ~heater:(Option.is_some temperature)
      ~volume
      ~pump:true
      ~occupied_volume:volume
      ~required_volume
    |> add_vertex
  in
  let waste =
    make_container
      ~id:(fresh_id ())
      ~description:"Waste"
      ~kind:Waste
      ~agitator:false
      ~heater:false
      ~volume:None
      ~pump:false
      ~occupied_volume:None
      ~required_volume:None
    |> add_vertex
  in
  let reactors =
    RVMap.fold
      (fun name rv acc ->
        let occupied_volume =
          match rv.solution with
          | Some solution -> solution.volume
          | None -> None
        in
        let reactor =
          make_container
            ~id:(fresh_id ())
            ~description:("RV: " ^ name)
            ~kind:Reactor
            ~agitator:rv.agitate
            ~heater:(Option.is_some rv.temperature)
            ~volume:rv.max_volume
            ~pump:false
            ~occupied_volume
            ~required_volume:rv.max_volume
          |> add_vertex
        in
        reactor :: acc)
      env.rvs
      []
  in
  let add_required_volume name required_volume required_map =
    let previous_required_volume =
      match SolutionMap.find_opt name required_map with
      | Some existing_volume -> existing_volume
      | None -> 0.0
    in
    SolutionMap.add name (previous_required_volume +. required_volume) required_map
  in
  let solution_required_volumes =
    SolutionLineageMap.fold
      (fun _ lineage required_map ->
        let required_map_with_parent_1 =
          match lineage.parent_1_required_volume with
          | Some required_volume ->
            add_required_volume lineage.parent_1 required_volume required_map
          | None -> required_map
        in
        match lineage.parent_2_required_volume with
        | Some required_volume ->
          add_required_volume lineage.parent_2 required_volume required_map_with_parent_1
        | None -> required_map_with_parent_1)
      env.solution_lineage
      SolutionMap.empty
  in
  let solution_reservoirs =
    SolutionMap.fold
      (fun name (solution : solution) acc ->
        let required_volume =
          match SolutionMap.find_opt name solution_required_volumes with
          | Some required_from_children ->
            (match solution.volume with
            | Some current_volume -> Some (max current_volume required_from_children)
            | None -> Some required_from_children)
          | None -> solution.volume
        in
        let reservoir =
          add_reservoir
            ~description:("Solution: " ^ name)
            ~agitate:solution.agitate
            ~volume:solution.volume
            ~required_volume
            ~temperature:solution.temperature
        in
        SolutionMap.add name reservoir acc)
      env.solutions
      SolutionMap.empty
  in
  let reservoirs_from_solutions =
    SolutionMap.fold (fun _ reservoir acc -> reservoir :: acc) solution_reservoirs []
  in
  let reservoirs_from_aa_solutions =
    AASolutionMap.fold
      (fun name (solution : aa_solution) acc ->
        add_reservoir
          ~description:("AA Solution: " ^ name)
          ~agitate:solution.agitate
          ~volume:solution.volume
          ~required_volume:solution.volume
          ~temperature:solution.temperature
        :: acc)
      env.aa_solutions
      reservoirs_from_solutions
  in
  let reservoirs =
    SolventMap.fold
      (fun name _ acc ->
        add_reservoir
          ~description:("Solvent: " ^ name)
          ~agitate:false
          ~volume:None
          ~required_volume:None
          ~temperature:None
        :: acc)
      env.solvents
      reservoirs_from_aa_solutions
  in
  let reservoirs =
    ResinMap.fold
      (fun name _ acc ->
        add_reservoir
          ~description:("Resin: " ^ name)
          ~agitate:false
          ~volume:None
          ~required_volume:None
          ~temperature:None
        :: acc)
      env.resins
      reservoirs
  in
  List.iter
    (fun source ->
      List.iter
        (fun reactor -> ContainerGraph.add_edge graph source reactor)
        reactors)
    reservoirs;
  SolutionLineageMap.iter
    (fun mixed_solution_name lineage ->
      let mixed_solution =
        try Some (SolutionMap.find mixed_solution_name solution_reservoirs)
        with Not_found -> None
      in
      let parent_1_container =
        try Some (SolutionMap.find lineage.parent_1 solution_reservoirs)
        with Not_found -> None
      in
      let parent_2_container =
        try Some (SolutionMap.find lineage.parent_2 solution_reservoirs)
        with Not_found -> None
      in
      match mixed_solution with
      | None -> ()
      | Some mixed_container ->
        (match parent_1_container with
         | Some parent_container -> ContainerGraph.add_edge graph parent_container mixed_container
         | None -> ());
        (match parent_2_container with
         | Some parent_container -> ContainerGraph.add_edge graph parent_container mixed_container
         | None -> ()))
    env.solution_lineage;
  List.iter (fun reactor -> ContainerGraph.add_edge graph reactor waste) reactors;
  graph



module HardwareGraphDot = Graph.Graphviz.Dot (struct
  include ContainerGraph

  let graph_attributes (_ : t) : Graph.Graphviz.DotAttributes.graph list = []

  let default_vertex_attributes (_ : t) : Graph.Graphviz.DotAttributes.vertex list =
    []

  let default_edge_attributes (_ : t) : Graph.Graphviz.DotAttributes.edge list = []

  let vertex_name v = Printf.sprintf "node_%d" v.id

  let get_subgraph (_ : vertex) : Graph.Graphviz.DotAttributes.subgraph option =
    None

  let volume_to_label volume =
    match volume with
    | Some v -> Printf.sprintf "%.2f mL" v
    | None -> "None"

  let vertex_attributes (v : vertex) : Graph.Graphviz.DotAttributes.vertex list =
    let color =
      match v.kind with
      | Resevoir -> 0xADD8E6
      | Reactor -> 0xF08080
      | Mixer -> 0x90EE90
      | Waste -> 0xD3D3D3
    in
    let label =
      match v.kind with
      | Resevoir | Reactor ->
        Printf.sprintf
          "%s\noccupied: %s\nrequired: %s"
          v.description
          (volume_to_label v.occupied_volume)
          (volume_to_label v.required_volume)
      | Mixer | Waste -> v.description
    in
    [`Shape `Box; `Style `Filled; `Fillcolor color; `Label label]

  let edge_attributes (_ : edge) : Graph.Graphviz.DotAttributes.edge list = []
end)

let output_hardware_graph_to_dot graph = HardwareGraphDot.output_graph stdout graph

let output_dot_to_file graph filename =
  let oc = open_out filename in
  HardwareGraphDot.output_graph oc graph;
  close_out oc


let generate_png_from_dot dot_filename output_png_filename =
    let command =
      Printf.sprintf
        "dot -Tpng %s -o %s"
        (Filename.quote dot_filename)
        (Filename.quote output_png_filename)
    in
    match Sys.command command with
    | 0 -> ()
    | code ->
        failwith
          (Printf.sprintf
             "Failed to generate PNG with Graphviz (exit code %d). Ensure `dot` is installed and available on PATH."
             code)


let hardware_graph_to_png graph dot_filename png_filename =
  output_dot_to_file graph dot_filename;
  generate_png_from_dot dot_filename png_filename
