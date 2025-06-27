(*The purpose of this module is to parse a CSV and generate a list of solvents that can be used*)
open Types
let parse_line_solvent line = 
  match String.split_on_char ',' line with 
  | [solname; formula; polar; protic ] ->
    {
     solname;
      formula;
      polar = bool_of_string polar;
      protic = bool_of_string protic;
    }
    | _ -> failwith "You suck at coding" 


(*function to read in CSV*)
let read_csv filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line channel in
      let solvent = parse_line_solvent line in
      read_lines (solvent :: acc)
    with
        End_of_file -> close_in channel; List.rev acc
    in
      read_lines []

(*create a list of amino_acids that comprise of the 20 naturally ocurring ones*)
let solvent_list = read_csv "solvents.csv"
