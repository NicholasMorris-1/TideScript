type atom = {
  atomic_number : int;
  element : string;
  symbol : string;
  atomic_mass : float;
  (*no_of_neutrons : int;
  no_of_protons : int;
  no_of_electrons : int;
  period : int;
  group : int;
  phase : string;
  radioactive : bool;
  natural : bool;
  metal : bool;
  nonmetal : bool;
  metalloid : bool;
  type_of_element : string;
  atomic_radius : float option;
  electronegativity : float option;
  first_ionization_energy : float option;
  density : float option;
  melting_point : float option;
  boiling_point : float option;
  number_of_isotopes : int option;
  discoverer : string;
  year_of_discovery : int option;
  specific_heat_capacity : float option;
  number_of_shells : int;
    number_of_valence : int option;*)
}

let parse_line line =
    match String.split_on_char ',' line with
      |  atomic_number ::  element :: symbol ::  atomic_mass :: _ (* no_of_neutrons; no_of_protons; no_of_electrons; period; group; phase; radioactive; natural; metal; nonmetal; metalloid; type_of_element; atomic_radius; electronegativity; first_ionization_energy; density; melting_point; boiling_point; number_of_isotopes; discoverer; year_of_discovery; specific_heat_capacity; number_of_shells; number_of_valence*) ->
        {
          atomic_number = int_of_string atomic_number;
          element;
          symbol;
          atomic_mass = float_of_string atomic_mass;
         (* no_of_neutrons = int_of_string no_of_neutrons;
          no_of_protons = int_of_string no_of_protons;
          no_of_electrons = int_of_string no_of_electrons;
          period = int_of_string period;
          group = int_of_string group;
          phase;
          radioactive = (match radioactive with "1" -> true | _ -> false);
          natural = (match natural with "1" -> true | _ -> false);
          metal = (match metal with "1" -> true | _ -> false);
          nonmetal = (match nonmetal with "1" -> true | _ -> false);
          metalloid = (match metalloid with "1" -> true | _ -> false);
          type_of_element;
          atomic_radius = (match atomic_radius with "" -> None | v -> Some (float_of_string v));
          electronegativity = (match electronegativity with "" -> None | v -> Some (float_of_string v));
          first_ionization_energy = (match first_ionization_energy with "" -> None | v -> Some (float_of_string v));
          density = (match density with "" -> None | v -> Some (float_of_string v));
          melting_point = (match melting_point with "" -> None | v -> Some (float_of_string v));
          boiling_point = (match boiling_point with "" -> None | v -> Some (float_of_string v));
          number_of_isotopes = (match number_of_isotopes with "" -> None | v -> Some (int_of_string v));
          discoverer;
          year_of_discovery = (match year_of_discovery with "" -> None | v -> Some (int_of_string v));
          specific_heat_capacity = (match specific_heat_capacity with "" -> None | v -> Some (float_of_string v));
          number_of_shells = int_of_string number_of_shells;
            number_of_valence = (match number_of_valence with "" -> None | v -> Some (int_of_string v));*)
        }
        | _ -> failwith "Csv not formatted correctly"


let read_csv filename =
    let channel = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line channel in
        let atom = parse_line line in
        read_lines (atom :: acc)
      with
          End_of_file -> close_in channel; List.rev acc
      in
        read_lines []

let periodic_table = read_csv "periodic_table.csv"
