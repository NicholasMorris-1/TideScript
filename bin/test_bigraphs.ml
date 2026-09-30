open Tidescript

let () =
  print_endline "Running bigraph test...";
  print_endline(Tide_bigraph.Big.show Tide_bigraph.b);
  print_endline(Bool.to_string Tide_bigraph.result);
  print_endline(Bool.to_string(Tide_bigraph.validity_result));
  print_endline("Reachable states: " ^ string_of_int (List.length Tide_bigraph.stepped))
