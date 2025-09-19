open Types
open Functions


let rec create_solute_list sollist solute_map =
  match sollist with
  | EmptySollist -> []
  | Sollist (s, t, rest) ->
    let solute = find_solute_by_name s solute_map in
    (solute, t) :: (create_solute_list rest solute_map)

let rec create_solvent_list solvnlist map =
  match solvnlist with
  | EmptySolvnlist -> []
  | Solvnlist (s, rest) -> (find_solvent_by_name s map )::(create_solvent_list rest map)


let init_env = {
  solutes = SoluteMap.empty;
  solvents = SolventMap.empty;
  solutions = SolutionMap.empty;
  protocols = ProtocolMap.empty;
}


let shallow_copy_env (env : env) : env =
  {
    solutes = env.solutes;
    solvents = env.solvents;
    solutions = env.solutions;
    protocols = env.protocols;
  }

let rec eval_expr (e : expression) (env : env) : (env * solution option) =
      match e with
      | Sequence (e1, e2) ->
                  let env', _ = eval_expr e1 env in
                  eval_expr e2 env'
      | Addpeptide (s, t) ->
                  ({env with solutes = add_peptide s t env.solutes}, None)
      | Addmolecule (s, t) ->
                  ({env with solutes = add_molecule s t env.solutes}, None)
      | Solvent (s) ->
                  ({env with solvents = add_solvent s env.solvents}, None)
      | Solution (var, args1, args2) ->
                  let solute_list = create_solute_list args1 env.solutes in
                  let solvent_list = create_solvent_list args2 env.solvents in
                  ({env with solutions = init_solution var solute_list solvent_list env.solutions}, None)
      | Combine (s1, s2, s3) ->
                  ({env with solutions = combine_solutions s1 s2 s3 env.solutions}, None)
      | Mix (s1, s2, s3, eq1, eq2, v) ->
                  let vol_float = match v with
                        | Volume x -> x
                        | VolumeParam _ -> 10.0  (* This should not happen after substitution *)
                        | NoVolume -> 10.0 in
                  let solution = mix_solutions_return_solution s2 s3 eq1 eq2 vol_float env.solutions in
                  ({env with solutions = SolutionMap.add s1 solution env.solutions}, None)
      | Agitate (s) ->
                  ({env with solutions = agitate_solution s env.solutions}, None)
      | Return (s) ->
                  let solution = find_solution_by_name s env.solutions in
                  env, Some solution
      | ChangeTemp (s, t) ->
                  ({env with solutions = change_temp_solution s t env.solutions}, None)
      | Deagitate (s) ->
                  ({env with solutions = de_agitate_solution s env.solutions}, None)
      | Wait(x) ->
                  let () = Unix.sleep 2 in
                  let () = Printf.printf "Waiting for %d hours\n" x in
                  (env, None)
      | Protocol (s, r, a, e) ->
                  ({env with protocols = add_protocol (create_protocol s r a e) env.protocols}, None)
      | Call (s, args) ->
                  let env' = bind_args_2 env s args in
                  let p = retrieve_protocol s env.protocols in
                  (match p.returntype with
                  | VoidType ->
                              let _, _ = eval_expr p.expressions env' in
                              (env, None)
                  | SolutionType ->
                              raise (Failure ("Protocol " ^ s ^ " has return type Solution, but return value not handled")))
      | Call_2 (s, args) ->
                  let p = retrieve_protocol s env.protocols in
                  let bound_p = bind_params_with_args_in_protocol p args in
                  let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
                  eval_expr alpha_converted_expr env
      | Call_void (s, args) ->
                  let p_env = init_env in
                  let p = retrieve_protocol s env.protocols in
                  (if p.returntype <> VoidType then
                        raise (Failure ("Protocol " ^ s ^ " does not have return type Void"))
                  else ());
                  let bound_p = bind_params_with_args_in_protocol p args in
                  let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
                  (match alpha_converted_expr with
                  | Mix (s1, s2, s3, eq1, eq2, v) ->
                              let _protocol_result = mix_solutions_protocol s1 s2 s3 eq1 eq2 v env.solutions p_env.solutions in
                              (env, None)
                  | _ ->
                              let _, _ = eval_expr alpha_converted_expr p_env in
                              (env, None))
      | Call_solution (s_1, s_2, args) ->
                  let p_env = shallow_copy_env env in
                  let p = retrieve_protocol s_2 env.protocols in
                  (if p.returntype <> SolutionType then
                              raise (Failure ("Protocol " ^ s_2 ^ " does not have return Solution"))
                  else ());
                  let bound_p = bind_params_with_args_in_protocol p args in
                  let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
                  let _p_env', sol_opt =
                    match alpha_converted_expr with
                    | Mix (_s1, s2, s3, eq1, eq2, v) ->
                              let new_solution = mix_solutions_protocol_return_solution s2 s3 eq1 eq2 v p_env.solutions in
                              let new_p = {p with solutions = SolutionMap.add s2 new_solution p.solutions} in
                              ({p_env with protocols = ProtocolMap.add s2 new_p p_env.protocols}, Some new_solution)
                    | Return var_name ->
                              let solution = find_solution_by_name var_name p_env.solutions in
                              let _p_2 = {p with returnSolution = Some solution} in
                              (p_env, Some solution)
                    | _ ->
                              eval_expr alpha_converted_expr p_env
                  in
                  (match sol_opt with
                   | Some solution ->
                          ({env with solutions = SolutionMap.add s_1 solution env.solutions}, Some solution)
                   | None -> (env, None))
      | Call_solution_2 (s_1, s_2, args) ->
            let env' = shallow_copy_env env in
            let p = retrieve_protocol s_2 env'.protocols in
            if p.returntype <> SolutionType then
              raise (Failure ("Protocol " ^ s_2 ^ " does not have return Solution"))
            else
              let bound_p = bind_params_with_args_in_protocol p args in
              let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
              let _env'', sol_opt = eval_expr alpha_converted_expr env' in
              (match sol_opt with
              | Some solution ->
                        ({env with solutions = SolutionMap.add s_1 solution env.solutions}, Some solution)
              | None -> (env, None))

      | Print ->
                  let () = Unix.sleep 2 in
                  let () = print_env env None in
                  (env, None)
      | _ -> env, None



(*let eval_protocol_expr (e:expression) (env:env) (p_env) : env =
  match e with
  | Mix (s1, s2, s3, eq1, eq2, v) ->
     let vol_float =  v in
     {env with solutions = mix_solutions_protocol s1 s2 s3 eq1 eq2 vol_float env.solutions p_env.solutions}
  | _ -> eval_expr e env*)
