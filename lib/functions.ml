open Types
open Aminoacids
open Solvents
open Periodictable

(*Neccesary to fetch an amino acid from a one letter code input from a sequence*)


let find_amino_acid_by_one_letter_code c = List.find (fun aa -> aa.one_letter_code = c)

(*Same as above but for three letter code, this functionality isn't utilised yet*)


let find_amino_acid_by_three_letter_code c amino_acid_list =
  let aa =find_amino_acid_by_one_letter_code c amino_acid_list in
    aa.three_letter_code


(*Find the mass of a paricular amino acid by its one letter code, useful for calculating the mass of a peptide sequence*)

let find_average_mass_by_one_letter_code c amino_acid_list =
  let aa = find_amino_acid_by_one_letter_code c amino_acid_list in
  aa.average_mass



(*Same as above but for the monoisotopic mass*)


let find_monoisotopic_mass_by_one_letter_code  c amino_acid_list =
  let aa = find_amino_acid_by_one_letter_code c amino_acid_list in
    aa.monoisotopic_mass

(*Fetch the smiles string of an amino acid from one letter code*)


let find_smiles_by_one_letter_code c amino_acid_list =
  let aa = find_amino_acid_by_one_letter_code c amino_acid_list in
    aa.smiles


let calculate_peptide_mass sequence =
  let total_mass = List.fold_left(fun acc aa ->
      acc  +. find_average_mass_by_one_letter_code aa.one_letter_code natural_amino_acids ) 0.0  sequence in
      total_mass +. 18.01528

(*same as above for monoisotopic*)

let calculate_peptide_monoisotopic_mass sequence =
  let total_mass = List.fold_left(fun acc aa ->
      acc  +. find_monoisotopic_mass_by_one_letter_code aa.one_letter_code natural_amino_acids ) 0.0  sequence in
      total_mass +. 18.0105


(*This function is neccesary for combing smiles strings of amino acids to form the smiles string of a peptide
  This is because there is a loss of water molecule in the condensations reaction of amino acids
  i.e the "O" is taken off the smiles string, hydrogens are ommitted in smiles representation*)
  let remove_last_char str =
    let len = String.length str in
    let new_str = String.sub str 0 (len - 1) in
    new_str


let generate_smiles sequence =
  let smiles = List.fold_left(fun acc aa -> acc  ^ remove_last_char(find_smiles_by_one_letter_code aa.one_letter_code natural_amino_acids)) ""  sequence in
  smiles ^ "O"


(*This function constructs a sequence, which is a list of amino_acids, from an string input of just the one letter code*)

let construct_sequence aa_sequence =
  String.fold_right (fun c acc ->
    let found_amino_acid = find_amino_acid_by_one_letter_code c natural_amino_acids in
    found_amino_acid :: acc) aa_sequence []



 (*Constructs a whole peptide type from a sequence, other properties are calculated from previously defined functions*)
  let construct_peptide aa_sequence =
    let sequence = construct_sequence aa_sequence in
    let average_mass = calculate_peptide_mass sequence in
    let monoisotopic_mass = calculate_peptide_monoisotopic_mass sequence in
    let smiles = generate_smiles sequence in
    Peptide
    {
      sequence;
      average_mass;
      monoisotopic_mass;
      smiles;
    }

(*let create_solute (input : [> `PeptideInput of peptide | `MoleculeInput of molecule]) =
  match input with
  | `PeptideInput peptide -> Peptide (peptide)
  | `MoleculeInput molecule -> Molecule (molecule)*)



(*This function adds user declared peptides to map*)
let add_peptide name sequence map  =
  let peptide = construct_peptide sequence in
  let key = name in
  SoluteMap.add key peptide  map


(*let find_solute_by_name name map =
  match SoluteMap.find_opt name map with
  | Some solute -> solute
   | None -> raise Not_found*)






let find_solvent_from_list name  =
  try
  List.find (fun x -> x.solname = name) solvent_list
  with
  | Not_found -> raise Not_found

let add_solvent name map =
  let key = name in
  let solvent = find_solvent_from_list name in
  SolventMap.add key solvent map

let find_solvent_by_name name map =
  try
    SolventMap.find name map
  with
  | Not_found -> raise Not_found

let find_peptide_by_name name map =
  try
    SoluteMap.find name map
  with
  | Not_found -> raise Not_found

let init_solution name solute_list solvent_list map =
  let key = name in
  let solution : solution = {
    solutes = solute_list;
    solvents = solvent_list;
    agitate = false;
    volume = None;
    temperature = None;
  } in
  SolutionMap.add key solution map

let add_aa_solution name aa_code solute_list solvent_list map =
  let key = name in
  let aa = find_amino_acid_by_one_letter_code aa_code natural_amino_acids in
  let aa_solution : aa_solution = {
    aa;
    solutes = solute_list;
    solvents = solvent_list;
    agitate = false;
    volume = None;
    temperature = None;
  } in
  AASolutionMap.add key aa_solution map


let find_solution_by_name name map =
  try
    SolutionMap.find name map
  with
    | Not_found -> raise Not_found

let combine_solutions name sol1 sol2 (map: solution SolutionMap.t) =
  let solution_1 : solution = find_solution_by_name sol1 map in
  let solution_2 : solution = find_solution_by_name sol2 map in
  let solutes = solution_1.solutes @ solution_2.solutes in
  let solvents = solution_1.solvents @ solution_2.solvents in
  let solution : solution  = {
    solutes;
    solvents;
    agitate = false;
    volume = None;
    temperature = None;
  } in
  SolutionMap.add name solution map


let mix_solutions name sol1 sol2 eq1 eq2 (final_volume: float) (map: solution SolutionMap.t) =
  let solution_1 : solution = find_solution_by_name sol1 map in
  let solution_2 : solution = find_solution_by_name sol2 map in
  let stoichiometric_ratio = eq1 /. eq2 in
  let moles_1 = solution_1.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc) 0.0 in
  let moles_2 = solution_2.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc) 0.0 in
  let volume_1 = (final_volume *. stoichiometric_ratio) /. (stoichiometric_ratio +. 1.0) in
  let volume_2 = final_volume -. volume_1 in
  let new_conc_1 = moles_1 /. volume_1 in
  let new_conc_2 = moles_2 /. volume_2 in
  let solutes_1 = List.map (fun (solute, _) -> (solute, new_conc_1)) solution_1.solutes in
  let solutes_2 = List.map (fun (solute, _) -> (solute, new_conc_2)) solution_2.solutes in
  let solutes = solutes_1 @ solutes_2 in
  let solvents = solution_1.solvents @ solution_2.solvents in
  let volume = Some final_volume in
    let solution : solution  = {
        solutes;
        solvents;
        agitate = false;
        volume;
        temperature = None;
      } in
    SolutionMap.add name solution map

let mix_solutions_protocol name sol1 sol2 eq1 eq2 (final_volume: volume_type) (map: solution SolutionMap.t) (p_map : solution SolutionMap.t) =
  let final_vol =
  match final_volume with
  | NoVolume -> 0.0
  | Volume final_volume -> final_volume 
  | VolumeParam _ -> 0.0  (* This should not happen after substitution *) in
  let solution_1 : solution = find_solution_by_name sol1 map in
  let solution_2 : solution = find_solution_by_name sol2 map in
  let stoichiometric_ratio = eq1 /. eq2 in
  let moles_1 = solution_1.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc) 0.0 in
  let moles_2 = solution_2.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc) 0.0 in
  let volume_1 = (final_vol *. stoichiometric_ratio) /. (stoichiometric_ratio +. 1.0) in
  let volume_2 = final_vol -. volume_1 in
  let new_conc_1 = moles_1 /. volume_1 in
  let new_conc_2 = moles_2 /. volume_2 in
  let solutes_1 = List.map (fun (solute, _) -> (solute, new_conc_1)) solution_1.solutes in
  let solutes_2 = List.map (fun (solute, _) -> (solute, new_conc_2)) solution_2.solutes in
  let solutes = solutes_1 @ solutes_2 in
  let solvents = solution_1.solvents @ solution_2.solvents in
  let volume = Some final_vol in
    let solution : solution  = {
        solutes;
        solvents;
        agitate = false;
        volume;
        temperature = None;
      } in
    SolutionMap.add name solution p_map


let mix_solutions_protocol_return_solution sol1 sol2 eq1 eq2 (final_volume: volume_type) (map: solution SolutionMap.t) =
  let final_vol =
  match final_volume with
  | NoVolume -> 0.0
  | Volume final_volume -> final_volume 
  | VolumeParam _ -> 0.0  (* This should not happen after substitution *) in
  let solution_1 : solution = find_solution_by_name sol1 map in
  let solution_2 : solution = find_solution_by_name sol2 map in
  let stoichiometric_ratio = eq1 /. eq2 in
  let moles_1 = solution_1.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc) 0.0 in
  let moles_2 = solution_2.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc) 0.0 in
  let volume_1 = (final_vol *. stoichiometric_ratio) /. (stoichiometric_ratio +. 1.0) in
  let volume_2 = final_vol -. volume_1 in
  let new_conc_1 = moles_1 /. volume_1 in
  let new_conc_2 = moles_2 /. volume_2 in
  let solutes_1 = List.map (fun (solute, _) -> (solute, new_conc_1)) solution_1.solutes in
  let solutes_2 = List.map (fun (solute, _) -> (solute, new_conc_2)) solution_2.solutes in
  let solutes = solutes_1 @ solutes_2 in
  let solvents = solution_1.solvents @ solution_2.solvents in
  let volume = Some final_vol in
    let solution : solution  = {
        solutes;
        solvents;
        agitate = false;
        volume;
        temperature = None;
      } in
    solution



let mix_solutions_return_solution sol1 sol2 eq1 eq2 (final_volume: float) (map: solution SolutionMap.t) =
  let solution_1 : solution = find_solution_by_name sol1 map in
  let solution_2 : solution = find_solution_by_name sol2 map in
  let stoichiometric_ratio = eq1 /. eq2 in
  let volume_1 = (final_volume *. stoichiometric_ratio) /. (stoichiometric_ratio +. 1.0) in
  let volume_2 = final_volume -. volume_1 in
  let moles_1 = solution_1.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc *. volume_1) 0.0 in
  let moles_2 = solution_2.solutes |> List.fold_left (fun acc (_, conc) -> acc +. conc *. volume_2) 0.0 in
  let new_conc_1 = if volume_1 > 0. then moles_1 /. final_volume else 0.0 in
  let new_conc_2 = if volume_2 > 0. then moles_2 /. final_volume else 0.0 in
  let solutes_1 = List.map (fun (solute, _) -> (solute, new_conc_1)) solution_1.solutes in
  let solutes_2 = List.map (fun (solute, _) -> (solute, new_conc_2)) solution_2.solutes in
  let solutes = solutes_1 @ solutes_2 in
  let solvents = solution_1.solvents @ solution_2.solvents in
  let volume = Some final_volume in
  let solution : solution  = {
      solutes;
      solvents;
      agitate = false;
      volume;
      temperature = None;
    } in
  solution


let mix_solutions_return_solution_with_unit sol1 sol2 eq1 eq2 (final_volume: float) (map: solution SolutionMap.t) (volunit: volume_unit) =
  let final_volume_in_ml =
    match volunit with
    | Liters -> final_volume *. 1000.0
    | Milliliters -> final_volume
    | _ -> final_volume  (* This should not happen after substitution *)
     in
  mix_solutions_return_solution sol1 sol2 eq1 eq2 final_volume_in_ml map






let mix_solutions_simple name sol1 sol2 vol1 vol2 (map: solution SolutionMap.t) =
  let solution_1 : solution = find_solution_by_name sol1 map in
  let solution_2 : solution = find_solution_by_name sol2 map in
  let dilution_factor_1 = vol1 /. (vol1 +. vol2) in
  let dilution_factor_2 = vol2 /. (vol1 +. vol2) in
  let solutes_1 = List.map (fun (solute, conc) -> (solute, conc *. dilution_factor_1)) solution_1.solutes in
  let solutes_2 = List.map (fun (solute, conc) -> (solute, conc *. dilution_factor_2)) solution_2.solutes in
  let solutes = solutes_1 @ solutes_2 in
  let solvents = solution_1.solvents @ solution_2.solvents in
  let volume = Some (vol1 +. vol2) in
  let solution : solution  = {
      solutes;
      solvents;
      agitate = false;
      volume;
      temperature = None;
    } in
  SolutionMap.add name solution map




let agitate_solution (solname : string) (map : solution SolutionMap.t)  =
  let solution = find_solution_by_name solname map in
  let new_solution : solution = {
    solutes = solution.solutes;
    solvents = solution.solvents;
    agitate = true;
    volume = solution.volume;
    temperature = solution.temperature;
  } in
  SolutionMap.add solname new_solution map



let de_agitate_solution (solname : string) (map : solution SolutionMap.t)  =
  let solution = find_solution_by_name solname map in
  let new_solution : solution = {
    solutes = solution.solutes;
    solvents = solution.solvents;
    agitate = false;
    volume = solution.volume;
    temperature = solution.temperature;
  } in
  SolutionMap.add solname new_solution map

let change_temp_solution (solname : string) (temp : float) (map : solution SolutionMap.t)  =
  let solution = find_solution_by_name solname map in
  let new_solution : solution = {
    solutes = solution.solutes;
    solvents = solution.solvents;
    agitate = solution.agitate;
    volume = solution.volume;
    temperature = Some temp;
  } in
  SolutionMap.add solname new_solution map

let is_a_return expr =
  match expr with
  | Return _ -> true
  | _ -> false

let is_there_a_return exprs =
  List.exists is_a_return exprs

let does_protocol_have_return protocol =
  is_there_a_return [protocol.expressions]


(* Function to find atomic mass by symbol *)
let find_atomic_mass periodic_table symbol =
  let rec search = function
    | [] -> failwith ("Element not found: " ^ symbol)
    | atom :: rest ->
        if atom.symbol = symbol then atom.atomic_mass
        else search rest
  in
  search periodic_table

(* Parse a number from string, default to 1 if empty *)
let parse_count str =
  if str = "" then 1
  else int_of_string str

(* Regular expression-like parsing for element + count *)
let parse_element_count s =
  let len = String.length s in
  let rec find_digit_start i =
    if i >= len then i
    else if s.[i] >= '0' && s.[i] <= '9' then i
    else find_digit_start (i + 1)
  in
  let digit_start = find_digit_start 0 in
  let element = String.sub s 0 digit_start in
  let count_str = if digit_start = len then ""
                  else String.sub s digit_start (len - digit_start) in
  let count = parse_count count_str in
  (element, count)

(* Split formula into element-count pairs *)
let tokenize_formula formula =
  let len = String.length formula in
  let rec parse_tokens acc i =
    if i >= len then List.rev acc
    else
      let rec find_next_element j =
        if j >= len then j
        else if formula.[j] >= 'A' && formula.[j] <= 'Z' then j
        else find_next_element (j + 1)
      in
      let next_start = find_next_element (i + 1) in
      let token = String.sub formula i (next_start - i) in
      let (element, count) = parse_element_count token in
      parse_tokens ((element, count) :: acc) next_start
  in
  if len = 0 then []
  else parse_tokens [] 0

(* Main function to calculate molecular weight *)
let calculate_molecular_weight periodic_table formula =
  let tokens = tokenize_formula formula in
  let total_weight = List.fold_left (fun acc (element, count) ->
    let atomic_mass = find_atomic_mass periodic_table element in
    acc +. (atomic_mass *. float_of_int count)
  ) 0.0 tokens in
  total_weight




let add_molecule name formula map =
  let key = name in
  let molecule =
    Molecule
    {
      name;
      molecular_weight = calculate_molecular_weight periodic_table formula ;
      formula;
      smiles = "";
    }
    in
    SoluteMap.add key molecule map



let find_molecule_by_name name map =
  try
    SoluteMap.find name map
  with
   | Not_found -> raise Not_found

let find_solute_by_name name map =
  try
    SoluteMap.find name map
  with
  | Not_found -> raise Not_found


let return_solution name global_map protocol_map =
  let solution = find_solution_by_name name protocol_map in
  let key = name in
  SolutionMap.add key solution global_map

let find_resin_by_name name map =
  try
    ResinMap.find name map
  with
  | Not_found -> raise Not_found

let dispense_soltution name (dispense_vol: float) (map: solution SolutionMap.t) : solution =
  let solution = find_solution_by_name name map in
  match solution.volume with
  | Some vol when (dispense_vol <= vol) ->
      let dilution_factor = dispense_vol /. vol in
      let new_solutes = List.map (fun (solute, conc) -> (solute, conc *. dilution_factor)) solution.solutes in
      let new_solution : solution = {
        solutes = new_solutes;
        solvents = solution.solvents;
        agitate = solution.agitate;
        volume = Some dispense_vol;
        temperature = solution.temperature;
      } in
      new_solution
  | None ->
    let new_solution : solution = {
      solutes = solution.solutes;
      solvents = solution.solvents;
      agitate = solution.agitate;
      volume = Some dispense_vol;
      temperature = solution.temperature;
    } in new_solution
  | _ -> raise (Failure "Not enough volume in solution to dispense")


let add_resin name loading_value peptide_id (p_map: solute SoluteMap.t) resin_map =
  let key = name in
  let solute_opt =
    match find_peptide_by_name peptide_id p_map with
    | peptide -> Some peptide
    | exception Not_found -> None in
  let peptide_opt =
    match solute_opt with
    | Some (Peptide p) -> Some p
    | _ -> None in
  let resin : resin = {
    resname = name;
    loading = loading_value;
    resin_bound_peptide = peptide_opt;
  } in
  ResinMap.add key resin resin_map

let add_rv name max_volume_opt resin_id resin_amount_opt resin_map rv_map =
  let key = name in
  let resin_opt =
    match find_resin_by_name resin_id resin_map with
    | resin -> Some resin
    | exception Not_found -> None in
  let rv : rv = {
    max_volume = max_volume_opt;
    resin = resin_opt;
    resin_amount = resin_amount_opt;
    solution = None;
    agitate = false;
    temperature = None;
  } in
  RVMap.add key rv rv_map


let agitate_rv (rvname : string) (map : rv RVMap.t)  =
  let rv = try RVMap.find rvname map with Not_found -> raise Not_found in
  let new_rv : rv = {
    max_volume = rv.max_volume;
    resin = rv.resin;
    resin_amount = rv.resin_amount;
    solution = rv.solution;
    agitate = true;
    temperature = rv.temperature;
  } in
  RVMap.add rvname new_rv map

let de_agitate_rv (rvname : string) (map : rv RVMap.t)  =
  let rv = try RVMap.find rvname map with Not_found -> raise Not_found in
  let new_rv : rv = {
    max_volume = rv.max_volume;
    resin = rv.resin;
    resin_amount = rv.resin_amount;
    solution = rv.solution;
    agitate = false;
    temperature = rv.temperature;
  } in
  RVMap.add rvname new_rv map

let agitate_toggle_rv (rvname :string) (map : rv RVMap.t) =
  let rv = try RVMap.find rvname map with Not_found -> raise Not_found in
  match rv.agitate with
  | true -> let new_rv : rv = {
              max_volume = rv.max_volume;
              resin = rv.resin;
              resin_amount = rv.resin_amount;
              solution = rv.solution;
              agitate = false;
              temperature = rv.temperature;
            } in
            RVMap.add rvname new_rv map
  | false -> let new_rv : rv = {
               max_volume = rv.max_volume;
               resin = rv.resin;
               resin_amount = rv.resin_amount;
               solution = rv.solution;
               agitate = true;
               temperature = rv.temperature;
             } in
             RVMap.add rvname new_rv map

let agitate_toggle_solution (solname :string) (map : solution SolutionMap.t) =
  let solution = try SolutionMap.find solname map with Not_found -> raise Not_found in
  match solution.agitate with
  | true -> let new_solution : solution = {
              solutes = solution.solutes;
              solvents = solution.solvents;
              agitate = false;
              volume = solution.volume;
              temperature = solution.temperature;
            } in
            SolutionMap.add solname new_solution map
  | false -> let new_solution : solution = {
               solutes = solution.solutes;
               solvents = solution.solvents;
               agitate = true;
               volume = solution.volume;
               temperature = solution.temperature;
             } in
             SolutionMap.add solname new_solution map

let agitate_toggle_rv_or_solution (name :string) (env: env) : env =
  match find_solution_by_name name env.solutions with
  | _solution -> let new_solutions = agitate_toggle_solution name env.solutions in
                {env with solutions = new_solutions}
  | exception Not_found ->
      (match RVMap.find_opt name env.rvs with
       | Some _ -> let new_rvs = agitate_toggle_rv name env.rvs in
                   {env with rvs = new_rvs}
       | None -> raise Not_found)

let is_there_sufficent_volume (rv: rv) (vol: float) =
  match rv.max_volume with
  | Some max_vol -> if vol <= max_vol then true else false
  | None -> true


let find_rv_by_name name map =
  try
    RVMap.find name map
  with
  | Not_found -> raise Not_found

let add_solution_to_rv_2 (rvname : string) (solution_name : string) (vol: float) (env: env) =
  let rv = find_rv_by_name rvname env.rvs in
    match is_there_sufficent_volume rv vol with
    | true -> let dispensed_solution = dispense_soltution solution_name vol env.solutions in
              let new_rv : rv = {
                max_volume = rv.max_volume;
                resin = rv.resin;
                resin_amount = rv.resin_amount;
                solution = Some dispensed_solution;
                agitate = rv.agitate;
                temperature = rv.temperature;
              } in
              let new_rvs = RVMap.add rvname new_rv env.rvs in
              {env with rvs = new_rvs}
  | false -> raise (Failure "Not enough volume in reaction vessel")

let add_solution_to_rv_with_unit (rvname : string) (solution_name : string) (vol: float) (volunit: volume_unit) (env: env) :env =
  let volume_in_ml =
    match volunit with
    | Liters -> vol *. 1000.0
    | Milliliters -> vol
    | _ -> vol  (* This should not happen after substitution *) in
  add_solution_to_rv_2 rvname solution_name volume_in_ml env






let add_solution_to_rv (rvname : string) (solname : string)  (smap : solution SolutionMap.t) (rmap : rv RVMap.t) =
  let rv = try RVMap.find rvname rmap with Not_found -> raise Not_found in
  let solution = try SolutionMap.find solname smap with Not_found -> raise Not_found in
    let new_rv : rv = {
    max_volume = rv.max_volume;
    resin = rv.resin;
    resin_amount = rv.resin_amount;
    solution = Some solution;
    agitate = rv.agitate;
    temperature = rv.temperature;
  } in
  RVMap.add rvname new_rv rmap

let drain_rv (rvname : string) (rmap : rv RVMap.t) =
  let rv = try RVMap.find rvname rmap with Not_found -> raise Not_found in
  let new_rv : rv = {
    max_volume = rv.max_volume;
    resin = rv.resin;
    resin_amount = rv.resin_amount;
    solution = None;
    agitate = rv.agitate;
    temperature = rv.temperature;
  } in
  RVMap.add rvname new_rv rmap


let create_solution_from_neat_molecule (molecule_name : string) (env: env) : solution =
  let molecule = find_molecule_by_name molecule_name env.solutes in
  let solute_list = [(molecule, 1.0)] in
  let solvent_list = [] in
  {
    solutes = solute_list;
    solvents = solvent_list;
    agitate = false;
    volume = None;
    temperature = None;
  }



let create_protocol name returntype (arglist: arg list) expressions  =
  let name = name in
  let arglist = arglist in
  let expressions = expressions in
  let solutions = SolutionMap.empty in
  let returntype = returntype in
  let returnSolution = None in
  {
    name;
    arglist;
    expressions;
    solutions;
    returntype;
    returnSolution;
  }



let add_protocol protocol map =
  let key = protocol.name in
  ProtocolMap.add key protocol map

let retrieve_protocol name map =
  try
    ProtocolMap.find name map
  with
  | Not_found -> raise Not_found

let bind_arg_2 env (argname :arg) (callname : arg) =
  match argname with
  | StringArg s1 ->
    (match callname with
    | StringArg s2 ->
      let solution = find_solution_by_name s2 env.solutions in
      let smap = SolutionMap.add s1 solution env.solutions in
      {env with solutions = smap}
    | FloatArg _ -> raise (Failure "Type mismatch: expected StringArg"))
  | FloatArg f1 ->
     (match callname with
        | FloatArg f2 ->
        let solution = find_solution_by_name (string_of_float f2) env.solutions in
        let smap = SolutionMap.add (string_of_float f1) solution env.solutions in
        {env with solutions = smap}
        | StringArg _ -> raise (Failure "Type mismatch: expected FloatArg"))

let bind_args_2 env pid (pargs: arg list) =
  let p = retrieve_protocol pid env.protocols in
  let bargs = List.fold_left2 (fun  acc a i -> bind_arg_2 acc a i) env ( p.arglist) pargs in
  bargs

let var_counter = ref 0

let tmp_var () : string =
  incr var_counter;
  "tmp" ^ string_of_int !var_counter

let rec free_vars (expr : expression) : string list =
  match expr with
  | Sequence (e1, e2) -> (free_vars e1) @ (free_vars e2)
  | Addpeptide (s, _) -> [s]
  | Addmolecule (s, _) -> [s]
  | Solvent s -> [s]
  | Solution (var, _, _) -> [var]
  | Combine (s1, s2, s3) -> [s1; s2; s3]
  | Mix (s1, s2, s3, _, _, _,_) -> [s1; s2; s3]
  | Agitate s -> [s]
  | Deagitate s -> [s]
  | ChangeTemp (s, _) -> [s]
  | Wait _ -> []
  | Protocol (_, _, _, e) -> free_vars e
  | Call (s, args) ->
      let arg_vars = List.fold_left (fun acc arg ->
        match arg with
        | StringArg str -> str :: acc
        | FloatArg f -> (string_of_float f) :: acc
      ) [] args in
      s :: arg_vars
  | Return s -> [s]
  | Print -> []
  (*| Dispense v -> [v]
    | FindLocation v -> [v]*)
  (*| Agitate (v, _) -> [v]*)
  | _ -> []


let rec alpha_convert (avoid_vars : string list) (expr : expression) : expression =
  match expr with
  | Sequence (e1, e2) ->
      let new_e1 = alpha_convert avoid_vars e1 in
      let new_e2 = alpha_convert avoid_vars e2 in
      Sequence (new_e1, new_e2)
  | Solution (var, args1, args2) ->
      if List.mem var avoid_vars then
        let new_var = tmp_var () in
        let new_avoid_vars = new_var :: avoid_vars in
        let new_e = Solution (new_var, args1, args2) in
        alpha_convert new_avoid_vars new_e
      else
        Solution (var, args1, args2)
  | Protocol (name, returntype, arglist, e) ->
      let arg_names = List.map (function StringArg s -> s | FloatArg f -> string_of_float f) arglist in
      let new_avoid_vars = arg_names @ avoid_vars in
      let new_e = alpha_convert new_avoid_vars e in
      Protocol (name, returntype, arglist, new_e)
  | Call (s, args) ->
      let arg_names = List.map (function StringArg s -> s | FloatArg f -> string_of_float f) args in
      let new_avoid_vars = arg_names @ avoid_vars in
      let new_args = List.map (function
        | StringArg s when List.mem s new_avoid_vars -> StringArg (tmp_var ())
        | FloatArg f when List.mem (string_of_float f) new_avoid_vars -> FloatArg (float_of_string (tmp_var ()))
        | arg -> arg
      ) args in
      Call (s, new_args)
  | _ -> expr


let string_of_vol_unit volunit =
  match volunit with
  | Liters -> "L"
  | Milliliters -> "mL"
  | VolumeUnitParam s -> s
  (*| Microliters -> "uL"*)

let convert_mgML_to_mm (conc: float) (solute: solute) : float =
  let molecular_weight =
    match solute with
    | Peptide p -> p.average_mass
    | Molecule m -> m.molecular_weight in
  conc /. molecular_weight




let rec substitute_var (old_var : string) (new_var : string) (expr : expression) : expression =
  match expr with
  | Sequence (e1, e2) ->
      let new_e1 = substitute_var old_var new_var e1 in
      let new_e2 = substitute_var old_var new_var e2 in
      Sequence (new_e1, new_e2)
  | Addpeptide (s, t) ->
      let new_s = if s = old_var then new_var else s in
      Addpeptide (new_s, t)
  | Addmolecule (s, t) ->
      let new_s = if s = old_var then new_var else s in
      Addmolecule (new_s, t)
  | Solvent s ->
      let new_s = if s = old_var then new_var else s in
      Solvent new_s
  | Solution (var, args1, args2) ->
      let new_var = if var = old_var then new_var else var in
      Solution (new_var, args1, args2)
  | Combine (s1, s2, s3) ->
      let new_s1 = if s1 = old_var then new_var else s1 in
      let new_s2 = if s2 = old_var then new_var else s2 in
      let new_s3 = if s3 = old_var then new_var else s3 in
      Combine (new_s1, new_s2, new_s3)
  | Mix (s1, s2, s3, eq1, eq2, vol, vol_unit) ->
      let new_s1 = if s1 = old_var then new_var else s1 in
      let new_s2 = if s2 = old_var then new_var else s2 in
      let new_s3 = if s3 = old_var then new_var else s3 in
      let new_eq1 = if string_of_float eq1 = old_var then float_of_string new_var else eq1 in
      let new_eq2 = if string_of_float eq2 = old_var then float_of_string new_var  else  eq2 in
      let new_vol = 
        match vol with
        | NoVolume -> NoVolume
        | Volume v ->
            if string_of_float v = old_var then 
              Volume (float_of_string new_var) 
            else 
              Volume v
        | VolumeParam param_name ->
            if param_name = old_var then
              (try Volume (float_of_string new_var) with _ -> VolumeParam new_var)
            else
              VolumeParam param_name
      in
      let new_volunit =
        match vol_unit with
        | VolumeUnitParam param_name ->
            if param_name = old_var then
              (* Convert string to actual unit type *)
              (match new_var with
              | "L" -> Liters
              | "mL" | "ML" -> Milliliters
              | _ -> VolumeUnitParam new_var)
            else VolumeUnitParam param_name
        | Liters | Milliliters as unit -> unit
      in
      Mix (new_s1, new_s2, new_s3, new_eq1, new_eq2, new_vol, new_volunit)
  | Agitate s ->
      let new_s = if s = old_var then new_var else s in
      Agitate new_s
  | Deagitate s ->
      let new_s = if s = old_var then new_var else s in
      Deagitate new_s
  | ChangeTemp (s, t) ->
     let new_s = if s = old_var then new_var else s in
     ChangeTemp (new_s, t)
  | Wait x -> Wait x
  | Protocol (name, returntype, arglist, e) ->
     let new_e = substitute_var old_var new_var e in
     Protocol (name, returntype, arglist, new_e)
  | Call_void (s, args) ->
     let new_s = if s = old_var then new_var else s in
       let new_args = List.map (function
          | StringArg s when s = old_var -> StringArg new_var
          | FloatArg f when string_of_float f = old_var -> FloatArg (float_of_string new_var)
          | arg -> arg
                        ) args in
       Call_void (new_s, new_args)
  | Return s ->
     let new_s = if s = old_var then new_var else s in
     Return new_s
  | Print -> Print
  | _ -> expr

let substitute_multiple_vars (subst_list : (string * string) list) (expr : expression) : expression =
  List.fold_left (fun acc (old_var, new_var) -> substitute_var old_var new_var acc) expr subst_list

let bind_params_with_args_in_protocol (protocol : protocol) (args : arg list) : protocol =
  let param_names = List.map (function StringArg s -> s | FloatArg f -> string_of_float f) protocol.arglist in
  let arg_names = List.map (function StringArg s -> s | FloatArg f -> string_of_float f) args in
  if List.length param_names <> List.length arg_names then
    raise (Failure "Argument count does not match parameter count");
  let subst_list = List.combine param_names arg_names in
  let new_expressions = substitute_multiple_vars subst_list protocol.expressions in
  { protocol with expressions = new_expressions; arglist = args }

let bind_params (formal_params : arg list) (actual_args : arg list) (expr : expression) : expression =
  let param_names = List.map (function StringArg s -> s | FloatArg f -> string_of_float f) formal_params in
  let arg_names = List.map (function StringArg s -> s | FloatArg f -> string_of_float f) actual_args in
  if List.length param_names <> List.length arg_names then
    raise (Failure "Argument count does not match parameter count");
  let subst_list = List.combine param_names arg_names in
  substitute_multiple_vars subst_list expr

let create_alpha_converted_expr s args env =
    let p = retrieve_protocol s env.protocols in
    let bound_p = bind_params_with_args_in_protocol p args in
    let alpha_converted_expr = alpha_convert (free_vars bound_p.expressions) bound_p.expressions in
    alpha_converted_expr


let is_expr_in_protocol expr protocol =
  let rec aux e =
    match e with
    | Sequence (e1, e2) -> aux e1 || aux e2
    | Protocol (_, _, _, e_inner) -> aux e_inner
    | _ -> e = expr
  in
  aux protocol.expressions

let is_expr_in_any_protocol expr protocols =
  ProtocolMap.exists (fun _ protocol -> is_expr_in_protocol expr protocol) protocols






(*let apply_protocol (protocol : protocol) (args : arg list) env : env =
  let bound_protocol = bind_params_with_args_in_protocol protocol args in
  let alpha_converted_expr = alpha_convert (free_vars bound_protocol.expressions) bound_protocol.expressions in
  eval_expr alpha_converted_expr env*)




(*let swap_arg_in_expression (param : arg) (arg : arg) (expr : expression) : expression =
  match expr with
  | Mix (s1, s2, s3, eq1, eq2, v) ->
      let new_s1 = if s1 = param then arg else s1 in
      let new_s2 = if s2 = param then arg else s2 in
      let new_s3 = if s3 = param then arg else s3 in
      Mix (new_s1, new_s2, new_s3, eq1, eq2, v)
  | _ -> expr*)

(*let bind_args_3 env pid pargs =
  let p = retrieve_protocol pid env.protocols in
  let new_protocol = swap_params_with_args_in_procotol p pargs in
    let _ = eval_expr new_protocol.expressions env in
    env*)


(* Make an environment mapping param names to call args *)
(*let make_subst (params : arg list) (args : arg list) : (string * arg) list =
  List.map2 (fun param actual -> (param.name, actual)) params args*)

(*let bind_arg env argname callname =
  let solution = find_solution_by_name callname env.solutions in
  let smap = SolutionMap.add argname solution env.solutions in
  {env with solutions = smap}

 let bind_args env pid pargs =
  let p = retrieve_protocol pid env.protocols in
  let bargs = List.fold_left2 (fun  acc a i -> bind_arg acc a i) env (arglist_to_lst  p.arglist) pargs in
  bargs*)


let print_peptide peptide =
  List.iter (fun a -> print_char a.one_letter_code) peptide.sequence;
  print_newline()

let print_molecule (molecule : molecule) =
  Printf.printf " %s: %s %.2f\n " molecule.name molecule.formula molecule.molecular_weight

let print_solute = function
  | Peptide p -> print_peptide p
  | Molecule m -> print_molecule m


let print_solvent (solvent : solvent) =
  print_endline solvent.solname

let print_solution (solution : solution) =
  List.iter (fun (solute, conc) ->
    print_solute solute;
    Printf.printf "concentration: %.2f, " conc
  ) solution.solutes;
  List.iter print_solvent solution.solvents;
  Printf.printf "agitating: %b\n" solution.agitate;
  Printf.printf "volume: %s\n" (match solution.volume with Some v -> string_of_float v | None -> "None")

let print_solutes map =
  SoluteMap.iter (fun k v ->
    Printf.printf "%s: " k;
    print_solute v;
    print_newline ()
  ) map

let print_solutions map =
  SolutionMap.iter (fun k v ->
    Printf.printf "\n%s: " k;
    print_solution v
  ) map


let print_protocol protocol =
  Printf.printf "Protocol Name: %s\n" protocol.name;
  print_endline "Arguments:";
  List.iter (fun arg ->
      match arg with
      | StringArg s -> Printf.printf "StringArg: %s\n" s
      | FloatArg f -> Printf.printf "FloatArg: %f\n" f
    ) protocol.arglist;
  print_endline "Return Solution:";
  (match protocol.returnSolution with
   | Some sol -> print_solution sol
   | None -> print_endline "None")

let print_protocols map =
  ProtocolMap.iter (fun _k v -> print_protocol v) map

let print_solvents map =
  SolventMap.iter (fun k _v -> print_endline k) map

let print_resins map =
  ResinMap.iter (fun k v ->
    Printf.printf "Resin Name: %s, Loading: %.2f, " k v.loading;
    match v.resin_bound_peptide with
    | Some peptide -> print_peptide peptide
    | None -> print_endline "No bound peptide"
  ) map

let print_rvs map =
  RVMap.iter (fun k v ->
    Printf.printf "RV Name: %s, Max Volume: %s, Resin Amount: %s, " k
      (match v.max_volume with Some mv -> string_of_float mv | None -> "None")
      (match v.resin_amount with Some ra -> string_of_float ra | None -> "None");
    match v.resin with
    | Some resin -> Printf.printf "Resin: %s\n" resin.resname
    | None -> print_endline "No resin"
  ) map

let print_env env (solution: solution option) =
  print_endline "SOLUTES:";
  (match solution with
  | None -> print_endline "None"
  | Some _s -> print_endline "Some");
  print_solutes env.solutes;
  print_newline ();
  print_endline "SOLUTIONS:";
  print_solutions env.solutions;
  print_newline ();
  print_endline "PROTOCOLS:";
  print_protocols env.protocols;
  print_newline ();
  print_endline "SOLVENTS:";
  print_solvents env.solvents;
  print_endline "RESINS:";
  print_resins env.resins;
  print_endline "RVS:";
  print_rvs env.rvs;
  print_newline ()
