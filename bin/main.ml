open Tidescript


let () =
  let initial_env = Syntax.init_env
   in
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.toplevel Lexer.token lexbuf in
  let args = Array.to_list Sys.argv in
  let brs_file =
    let rec find = function
      | [] -> None
      | "--brs" :: file :: _ -> Some file
      | "--brs" :: [] ->
          invalid_arg "--brs requires an output filename"
      | _ :: rest -> find rest
    in
    find (List.tl args)
  in
  (match brs_file with
   | Some file -> Bigraphs.write_brs e file
   | None -> ());
  let updated_env_with_solution, _ = Syntax.eval_expr e initial_env in
  Functions.print_env updated_env_with_solution None
