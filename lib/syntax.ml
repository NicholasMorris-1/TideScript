open Types
open Functions


let rec create_solute_list sollist solute_map =
  match sollist with
  | EmptySollist -> []
  | Sollist (s, t, cu, rest) ->
    let solute = find_solute_by_name s solute_map in
    let conc = match cu with
      | Millimolar -> t
      | MgPerMl -> convert_mgML_to_mm t solute
      | ConcUnitParam _ -> t in  (* This should not happen after substitution *)
    (solute, conc) :: (create_solute_list rest solute_map)

let rec create_solvent_list solvnlist map =
  match solvnlist with
  | EmptySolvnlist -> []
  | Solvnlist (s, rest) -> (find_solvent_by_name s map )::(create_solvent_list rest map)


let init_env = {
  solutes = SoluteMap.empty;
  solvents = SolventMap.empty;
  solutions = SolutionMap.empty;
  aa_solutions = AASolutionMap.empty;
  protocols = ProtocolMap.empty;
  resins = ResinMap.empty;
  rvs = RVMap.empty;
}



let shallow_copy_env (env : env) : env =
  {
    solutes = env.solutes;
    solvents = env.solvents;
    solutions = env.solutions;
    aa_solutions = env.aa_solutions;
    protocols = env.protocols;
    resins = env.resins;
    rvs = env.rvs;

  }

let rec eval_expr (e : expression) (env : env) (ir_prog : Ir.ir_program)
  : (env * solution option * Ir.ir_program) =
  let env', sol_opt, ir_prog' =
    match e with
    | Sequence (e1, e2) ->
        let env_after_e1, _, ir_prog_after_e1 = eval_expr e1 env ir_prog in
        eval_expr e2 env_after_e1 ir_prog_after_e1
    | Addpeptide (s, t) ->
        let env' = {env with solutes = add_peptide s t env.solutes} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Addmolecule (s, t) ->
        let env' = {env with solutes = add_molecule s t env.solutes} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Solvent s ->
        let env' = {env with solvents = add_solvent s env.solvents} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | AddResin (s, c, r) ->
        let env' = {env with resins = add_resin s c r env.solutes env.resins} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | AddRV (s, c, r) ->
        let env' = {env with rvs = add_rv s None c (Some r) env.resins env.rvs} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Solution (var, args1, args2) ->
        let solute_list = create_solute_list args1 env.solutes in
        let solvent_list = create_solvent_list args2 env.solvents in
        let env' = {env with solutions = init_solution var solute_list solvent_list env.solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | AASolution (var, aa, args1, args2) ->
        let solute_list = create_solute_list args1 env.solutes in
        let solvent_list = create_solvent_list args2 env.solvents in
        let env' = {env with aa_solutions = add_aa_solution var aa solute_list solvent_list env.aa_solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Combine (s1, s2, s3) ->
        let env' = {env with solutions = combine_solutions s1 s2 s3 env.solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Mix (s1, s2, s3, eq1, eq2, v, u) ->
        let vol_float =
          match v with
          | Volume x -> x
          | VolumeParam _ -> 10.0  (* This should not happen after substitution *)
          | NoVolume -> 10.0
        in
        let solution = mix_solutions_return_solution_with_unit s2 s3 eq1 eq2 vol_float env.solutions u in
        let env' = {env with solutions = SolutionMap.add s1 solution env.solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Agitate s ->
        let env' = agitate_toggle_rv_or_solution s env in
        let () = print_env env' None in
        (env', None, ir_prog)
    | AddTo (s1, v, u, s2) ->
        let vol_float =
          match v with
          | Volume x -> x
          | VolumeParam _ -> 10.0  (* This should not happen after substitution *)
          | NoVolume -> 10.0
        in
        let env' = add_solution_to_rv_with_unit s2 s1 vol_float u env in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Drain s ->
        let env' = {env with rvs = drain_rv s env.rvs} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Neat (s1, s2) ->
        let solution = create_solution_from_neat_molecule s2 env in
        let env' = {env with solutions = SolutionMap.add s1 solution env.solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Return s ->
        let solution = find_solution_by_name s env.solutions in
        let () = print_env env (Some solution) in
        (env, Some solution, ir_prog)
    | ChangeTemp (s, t) ->
        let env' = {env with solutions = change_temp_solution s t env.solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Deagitate s ->
        let env' = {env with solutions = de_agitate_solution s env.solutions} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Wait x ->
        let () = Unix.sleep 2 in
        let () = Printf.printf "Waiting for %d hours\n" x in
        let () = print_env env None in
        (env, None, ir_prog)
    | Protocol (s, r, a, e') ->
        let env' = {env with protocols = add_protocol (create_protocol s r a e') env.protocols} in
        let () = print_env env' None in
        (env', None, ir_prog)
    | Call (s, args) ->
        let env' = bind_args_2 env s args in
        let p = retrieve_protocol s env.protocols in
        (match p.returntype with
        | VoidType ->
            let _, _, ir_prog_after_call = eval_expr p.expressions env' ir_prog in
            let () = print_env env None in
            (env, None, ir_prog_after_call)
        | SolutionType ->
            raise (Failure ("Protocol " ^ s ^ " has return type Solution, but return value not handled")))
    | Call_2 (s, args) ->
        let p = retrieve_protocol s env.protocols in
        let bound_p = bind_params_with_args_in_protocol p args in
        let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
        eval_expr alpha_converted_expr env ir_prog
    | Call_void (s, args) ->
        let p_env = init_env in
        let p = retrieve_protocol s env.protocols in
        (if p.returntype <> VoidType then
           raise (Failure ("Protocol " ^ s ^ " does not have return type Void"))
         else ());
        let bound_p = bind_params_with_args_in_protocol p args in
        let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
        (match alpha_converted_expr with
        | Mix (s1, s2, s3, eq1, eq2, v, u) ->
            let vol_float =
              match v with
              | Volume x -> x
              | VolumeParam _ -> 10.0  (* This should not happen after substitution *)
              | NoVolume -> 10.0
            in
            let solution_result = mix_solutions_return_solution_with_unit s2 s3 eq1 eq2 vol_float env.solutions u in
            let env' = {env with solutions = SolutionMap.add s1 solution_result env.solutions} in
            let () = print_env env' None in
            (env', None, ir_prog)
        | _ ->
            let _, _, ir_prog_after_call = eval_expr alpha_converted_expr p_env ir_prog in
            let () = print_env env None in
            (env, None, ir_prog_after_call))
    | Call_solution_2 (s_1, s_2, args) ->
        let env' = shallow_copy_env env in
        let p = retrieve_protocol s_2 env'.protocols in
        if p.returntype <> SolutionType then
          raise (Failure ("Protocol " ^ s_2 ^ " does not have return Solution"))
        else
          let bound_p = bind_params_with_args_in_protocol p args in
          let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
          let _, sol_opt, ir_prog_after_call = eval_expr alpha_converted_expr env' ir_prog in
          (match sol_opt with
          | Some solution ->
              let env'' = {env with solutions = SolutionMap.add s_1 solution env.solutions} in
              let () = print_env env'' (Some solution) in
              (env'', Some solution, ir_prog_after_call)
          | None ->
              let () = print_env env None in
              (env, None, ir_prog_after_call))
    | Print ->
        let () = Unix.sleep 2 in
        let () = print_env env None in
        (env, None, ir_prog)
    | _ ->
        let () = print_env env None in
        (env, None, ir_prog)
  in
  let current_frame = Ir.env_to_frame env' in
  let ir_prog_with_frame = {Ir.frames = ir_prog'.frames @ [current_frame]} in
  (env', sol_opt, ir_prog_with_frame)



(*let eval_protocol_expr (e:expression) (env:env) (p_env) : env =
  match e with
  | Mix (s1, s2, s3, eq1, eq2, v, u) ->
     let vol_float =  v in
     {env with solutions = mix_solutions_protocol s1 s2 s3 eq1 eq2 vol_float env.solutions p_env.solutions}
  | _ -> eval_expr e env*)
