[@@@ocaml.warning "-33"]
open Hardwaregraph

type step =
       Init | Dispense | Move | Combine | ChangeTemp
       | Agitate | Deagitate


type frame = {
    id: int;
    step: step;
    graph: ContainerGraph.t;
  }
