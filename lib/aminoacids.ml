(*The purpose of this module is to parse a CSV file and populate a list of amino acids. 
Currently this only includes the 20 naturally occuring amino acids but there is scope to expand this*)
open Types

(*function for parsing lines of the CSV file and creating an amino acid type with the info populated*)
let parse_line line = 
  match String.split_on_char ',' line with 
  | [number; name; three_letter_code; one_letter_code; monoisotopic_mass; average_mass; formula; smiles ] ->
    {
      number = int_of_string number;
      name;
      three_letter_code;
      one_letter_code = String.get one_letter_code 0;
      monoisotopic_mass = float_of_string monoisotopic_mass;
      average_mass = float_of_string average_mass;  
      formula; 
      smiles;
    }
    | _ -> failwith "You suck at coding" 


(*function to read in CSV*)
let read_csv filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line channel in
      let amino_acid = parse_line line in
      read_lines (amino_acid :: acc)
    with
        End_of_file -> close_in channel; List.rev acc
    in
      read_lines []

(*create a list of amino_acids that comprise of the 20 naturally ocurring ones*)
let natural_amino_acids = read_csv "data/aa.csv"
