open Tidescript


let () =
  let initial_env = Syntax.init_env
   in
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.toplevel Lexer.token lexbuf in
  let updated_env_with_solution, _  =  Syntax.eval_expr e initial_env in
  Functions.print_env updated_env_with_solution None;
    let ir_prog = Ir.create_init_frame_if_empty (Ir.eval_expr_for_ir e {frames = []}) in
    let ir_json = Ir.generate_json_from_ir_program ir_prog in
    print_endline ir_json
