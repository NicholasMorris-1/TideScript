open Types
open Aminoacids
open Solvents

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




let add_solution name solute_list solvent_list map =
  let key = name in
  let solution : solution = {
    solutes = solute_list;
    solvents = solvent_list;
    agitate = false;
  } in
  SolutionMap.add key solution map

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
  } in
  SolutionMap.add name solution map

let agitate_solution (solname : string) (map : solution SolutionMap.t)  =
  let solution = find_solution_by_name solname map in
  let new_solution : solution = {
    solutes = solution.solutes;
    solvents = solution.solvents;
    agitate = true;
  } in
  SolutionMap.add solname new_solution map

let de_agitate_solution (solname : string) (map : solution SolutionMap.t)  =
  let solution = find_solution_by_name solname map in
  let new_solution : solution = {
    solutes = solution.solutes;
    solvents = solution.solvents;
    agitate = false;
  } in
  SolutionMap.add solname new_solution map

let add_molecule name formula map =
  let key = name in
  let molecule =
    Molecule
    {
      name;
      average_mass = 0.0;
      monoisotopic_mass = 0.0;
      formula ;
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


let create_protocol name returntype arglist expressions  =
  let name = name in
  let arglist = arglist in
  let expressions = expressions in
  let returntype = returntype in
  {
    name;
    arglist;
    expressions;
    returntype;
  }

let add_protocol protocol map =
  let key = protocol.name in
  ProtocolMap.add key protocol map

let retrieve_protocol name map =
  try
    ProtocolMap.find name map
  with
  | Not_found -> raise Not_found




let bind_arg env argname callname =
  let solution = find_solution_by_name callname env.solutions in
  let smap = SolutionMap.add argname solution env.solutions in
  {env with solutions = smap}

 let bind_args env pid pargs =
  let p = retrieve_protocol pid env.protocols in
  let bargs = List.fold_left2 (fun  acc a i -> bind_arg acc a i) env (arglist_to_lst  p.arglist) pargs in
  bargs


let print_peptide peptide =
  List.iter (fun a -> print_char a.one_letter_code) peptide.sequence;
  print_newline()

let print_molecule (molecule : molecule) =
  Printf.printf "%s: %s\n" molecule.name molecule.formula

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
  Printf.printf "agitating: %b\n" solution.agitate

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

let print_protocols map =
  ProtocolMap.iter (fun k _v -> print_endline k) map

let print_solvents map =
  SolventMap.iter (fun k _v -> print_endline k) map

let print_env env =
  print_endline "SOLUTES:";
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
   print_newline ()
