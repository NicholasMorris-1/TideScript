(*This module defines some types that are useful for chemistry. Properties of different species is encoded in, with scope to expand this*)

type amino_acid =
  {
    number: int;
    name: string;
    three_letter_code: string;
    one_letter_code: char;
    monoisotopic_mass : float;
    average_mass : float;
    formula: string;
    smiles: string;

  }

  type peptide =
  {
    sequence: amino_acid list;
    average_mass: float;
    monoisotopic_mass: float;
    smiles: string;
  }

type molecule = {

   name: string;
   molecular_weight : float;
   formula: string;
   smiles: string
  }


type solvent =
  {
    solname: string;
    formula: string;
    protic: bool;
    polar: bool;
 }

type solute =
  | Peptide of peptide
  | Molecule of molecule


(*type 'a input =
  | PeptideInput of peptide
   | MoleculeInput of molecule*)

(*type solute_input = solute input*)

type solution = {
  solutes  : (solute * float) list;
  solvents : solvent list;
  agitate : bool;
  volume : float option;
  temperature : float option;
}

type resin = {
  resname: string;
  loading: float; (*mmol/g*)
  resin_bound_peptide: peptide option;
}

type rv = {
  max_volume: float option;
  resin: resin option;
  resin_amount: float option; (*grams*)
  solution : solution option;
  agitate: bool;
  temperature: float option;

}

type aa_solution = {
  aa: amino_acid;
  solutes  : (solute * float) list;
  solvents : solvent list;
  agitate : bool;
  volume : float option;
  temperature : float option;
}

type arg =
  | StringArg of string
  | FloatArg of float

type pararm =
  | StringParam of string
  | FloatParam of float

type arglist =
 | EmptyArglist
 | Arglist of arg * arglist

type conc_unit =
  | Millimolar
  | MgPerMl
  | ConcUnitParam of string

type sollist =
  | EmptySollist
  | Sollist of string * float * conc_unit * sollist

type solvnlist =
  | EmptySolvnlist
  | Solvnlist of string * solvnlist

let rec arglist_to_lst (args: arglist) =
  match args with
  | EmptyArglist -> []
  | Arglist(s,l) -> s :: (arglist_to_lst l)

type volume_type =
  | Volume of float
  | VolumeParam of string
  | NoVolume

type volume_unit =
  | Liters
  | Milliliters
  | VolumeUnitParam of string
  (*| Microliters*)




type expression =
  | Sequence of expression * expression
  | Addpeptide of string * string
  | Addmolecule of string * string
  | AddResin of string * float * string
  | AddRV of string * string * float
  | Solvent of string
  | Solution of string * sollist * solvnlist
  | AASolution of string * char * sollist * solvnlist
  | Molsolution of string * string * float * string
  | CalculateAverageMass of string
  | GenerateSmiles of string
  | Protocol of string * return_type * arg list * expression
  | Dispense of string
  | FindLocation of string
  | Combine of string * string * string
  | ChangeTemp of string * float
  | Mix of string * string * string * float * float * volume_type * volume_unit
  | Agitate of string
  | AddTo of string * volume_type * volume_unit * string
  | Drain of string
  | Return of string
  | Deagitate of string
  | Wait of int
  | Print
  | Call of string * arg list
  | Call_2 of string * arg list
  | Call_void of string * arg list
  | Call_solution of string * string *  arg list
  | Call_solution_2 of string * string * arg list

and return_type =
  | VoidType
  | SolutionType







 (*A map to keep track of peptide variables declared in scope*)
module SoluteKey =
  struct
    type t = string
    let compare = compare
  end


module SoluteMap = Map.Make(SoluteKey)

(*Same logic as above for solvents*)
  module SolventKey =
  struct
    type t = string
    let compare = compare
  end

module SolventMap = Map.Make(SolventKey)

 (*for solutions*)
module SolutionKey =
  struct
    type t = string
    let compare = compare
  end

module SolutionMap = Map.Make(SolutionKey)

module AASolutionKey =
  struct
    type t = string
    let compare = compare
  end

module AASolutionMap = Map.Make(AASolutionKey)



(*maps to keep track of user protocols*)

type protocol ={
  name : string;
  arglist : arg list;
  expressions: expression;
  solutions: solution SolutionMap.t;
  returntype: return_type;
  returnSolution : solution option;
}

module ProtocolKey =
  struct
    type t = string
    let compare = compare
  end

module ProtocolMap = Map.Make(ProtocolKey)

module ResinKey =
  struct
    type t = string
    let compare = compare
  end

module ResinMap = Map.Make(ResinKey)

module RVKey =
  struct
    type t = string
    let compare = compare
  end

module RVMap = Map.Make(RVKey)


type env = {

  solutes: solute SoluteMap.t;
  solvents : solvent SolventMap.t;
  solutions : solution SolutionMap.t;
  aa_solutions : aa_solution AASolutionMap.t;
  protocols : protocol ProtocolMap.t;
  resins : resin ResinMap.t;
  rvs: rv RVMap.t;
  }
