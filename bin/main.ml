open Tidescript


let () =
  let initial_env = {
    Types.solutes = Types.SoluteMap.empty;
    Types.solvents = Types.SolventMap.empty;
    Types.solutions = Types.SolutionMap.empty;
    Types.protocols = Types.ProtocolMap.empty;
  } in

  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.toplevel Lexer.token lexbuf in
  let updated_env_with_solution, _  =  Syntax.eval_expr e initial_env in
  Functions.print_env updated_env_with_solution None
