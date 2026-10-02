open Tidescript
open Tide_bigraph

let () =
  print_endline "Running bigraph test...";
  print_endline(Big.show Tide_bigraph.b);
  print_endline(Bool.to_string result);
  print_endline(Bool.to_string(validity_result));
  print_endline("Reachable states: " ^ string_of_int (List.length stepped));
  save_dot_to_dir s0 "output" "initial_state.dot";
  save_tikz_to_dir s0 "output" "initial_state.tex";
