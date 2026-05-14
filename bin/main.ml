open Tidescript


let () =
  let initial_env = Syntax.init_env
   in
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.toplevel Lexer.token lexbuf in
  let updated_env_with_solution, _  =  Syntax.eval_expr e initial_env in
  let dot_filename = "hardware_graph.dot" in
  let png_filename = "hardware_graph.png" in
  let cwd = Sys.getcwd () in
  let hardware_graph = Hardwaregraph.env_to_hardware_graph updated_env_with_solution in
  Hardwaregraph.hardware_graph_to_png hardware_graph dot_filename png_filename;
  Printf.printf
    "Hardware graph written to:\n- %s\n- %s\n"
    (Filename.concat cwd dot_filename)
    (Filename.concat cwd png_filename);
  Functions.print_env updated_env_with_solution None
