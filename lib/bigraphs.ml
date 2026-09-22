let path_to_bigrapher_exe = "../vendor/bigraph-tools/_build/install/default/bin/bigrapher"

let get_bigraph_help () =
  let cmd = path_to_bigrapher_exe ^ " --help" in
  let ic = Unix.open_process_in cmd in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []


let gen_tikz_of_brs (input_file : string) (output_file : string) : unit =
  let cmd = path_to_bigrapher_exe ^ " sim -T 5 -s -t" ^ output_file ^ " > " ^ input_file in
  let exit_status = Sys.command cmd in
  if exit_status <> 0 then
    failwith ("Error generating TikZ from bigraphs: " ^ string_of_int exit_status)
