open Tidescript


let () =
  let initial_env = Syntax.init_env
  in
  let initial_ir_prog : Ir.ir_program = {frames = []} in
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.toplevel Lexer.token lexbuf in
  let _updated_env_with_solution, _, updated_ir_prog = Syntax.eval_expr e initial_env initial_ir_prog in
  let ir_prog_json = Ir.generate_json_from_ir_program updated_ir_prog in
  print_endline ir_prog_json
