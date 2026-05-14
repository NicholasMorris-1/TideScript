[@@@ocaml.warning "-33"]
open Graph

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
