type t = {
  base_url : string;
}

type http_method =
  | GET
  | POST

type request = {
  meth : http_method;
  url : string;
  headers : (string * string) list;
  body : string option;
}

exception Request_failed of string

type dataset =
  | UniProtKB
  | UniRef
  | UniParc
  | Proteomes
  | Taxonomy
  | GeneCentric
  | ARBA
  | UniRule

type format =
  | Json
  | Tsv
  | Fasta
  | Xml
  | List
  | Xlsx
  | Obo

let default_base_url = "https://rest.uniprot.org"

let open_api ?(base_url = default_base_url) () =
  { base_url = String.trim base_url }

let base_url api = api.base_url

let default_headers = [
  ("User-Agent", "TideScript");
]

let dataset_path = function
  | UniProtKB -> "uniprotkb"
  | UniRef -> "uniref"
  | UniParc -> "uniparc"
  | Proteomes -> "proteomes"
  | Taxonomy -> "taxonomy"
  | GeneCentric -> "genecentric"
  | ARBA -> "arba"
  | UniRule -> "unirule"

let format_extension = function
  | Json -> "json"
  | Tsv -> "tsv"
  | Fasta -> "fasta"
  | Xml -> "xml"
  | List -> "list"
  | Xlsx -> "xlsx"
  | Obo -> "obo"

let trim_trailing_slash s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '/' then
    String.sub s 0 (len - 1)
  else
    s

let is_unreserved = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '-'
  | '.'
  | '_'
  | '~' -> true
  | _ -> false

let percent_encode s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_unreserved c then
        Buffer.add_char buffer c
      else
        Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buffer

let query params =
  params
  |> List.filter (fun (_, value) -> value <> "")
  |> List.map (fun (key, value) -> percent_encode key ^ "=" ^ percent_encode value)
  |> String.concat "&"

let path api segments =
  trim_trailing_slash api.base_url ^ "/" ^ String.concat "/" (List.map percent_encode segments)

let with_query url params =
  match query params with
  | "" -> url
  | q -> url ^ "?" ^ q

let comma_separated values = String.concat "," values

let request ?body meth url = {
  meth;
  url;
  headers = default_headers;
  body;
}

let http_method_name = function
  | GET -> "GET"
  | POST -> "POST"

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let read_all channel =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buffer (input_line channel);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  Buffer.contents buffer

let command_of_args args =
  String.concat " " (List.map Filename.quote args)

let run_command args =
  let command = command_of_args args in
  let stdout_channel, stdin_channel, stderr_channel =
    Unix.open_process_full command (Unix.environment ())
  in
  close_out stdin_channel;
  let output = read_all stdout_channel in
  let error_output = read_all stderr_channel in
  let status =
    Unix.close_process_full (stdout_channel, stdin_channel, stderr_channel)
  in
  match status with
  | Unix.WEXITED 0 -> output
  | Unix.WEXITED code ->
      let details = String.trim error_output in
      if details = "" then
        raise (Request_failed (Printf.sprintf "curl exited with status %d" code))
      else
        raise (Request_failed (Printf.sprintf "curl exited with status %d: %s" code details))
  | Unix.WSIGNALED signal ->
      raise (Request_failed (Printf.sprintf "curl was killed by signal %d" signal))
  | Unix.WSTOPPED signal ->
      raise (Request_failed (Printf.sprintf "curl was stopped by signal %d" signal))

let curl_args req =
  [
    "curl";
    "-sS";
    "-L";
    "--fail";
    "--max-time";
    "20";
    "-X";
    http_method_name req.meth;
  ]
  @ List.concat_map
      (fun (name, value) -> ["-H"; name ^ ": " ^ value])
      req.headers
  @
  (match req.body with
   | None -> []
   | Some body -> ["--data"; body])
  @ [req.url]

let perform_request req =
  run_command (curl_args req)

let split_lines s =
  s
  |> String.split_on_char '\n'
  |> List.map String.trim

let parse_status response =
  response
  |> split_lines
  |> List.fold_left (fun latest line ->
      if starts_with ~prefix:"HTTP/" line then
        match String.split_on_char ' ' line with
        | _ :: code :: _ ->
            (match int_of_string_opt code with
             | Some _ as status -> status
             | None -> latest)
        | _ -> latest
      else
        latest)
      None

let parse_header name response =
  let wanted = String.lowercase_ascii name ^ ":" in
  response
  |> split_lines
  |> List.find_map (fun line ->
      let lower = String.lowercase_ascii line in
      if starts_with ~prefix:wanted lower then
        Some (String.trim (String.sub line (String.length wanted) (String.length line - String.length wanted)))
      else
        None)

let parse_retry_after response =
  match parse_header "retry-after" response with
  | None -> None
  | Some value -> int_of_string_opt value

let response_body response =
  match String.split_on_char '\n' response with
  | [] -> ""
  | lines ->
      let rec drop_headers = function
        | [] -> []
        | line :: rest ->
            if String.trim line = "" then rest else drop_headers rest
      in
      drop_headers lines |> String.concat "\n" |> String.trim

let peptide_search_job sequence =
  let response =
    run_command
      [
        "curl";
        "-sS";
        "-i";
        "--suppress-connect-headers";
        "--fail";
        "--max-time";
        "60";
        "-X";
        "POST";
        "-H";
        "Content-Type: application/x-www-form-urlencoded; version=1.1";
        "--data-binary";
        query [("peps", sequence)];
        "https://peptidesearch.uniprot.org/asyncrest/";
      ]
  in
  match parse_status response, parse_header "location" response with
  | Some 202, Some location -> location
  | Some status, _ ->
      raise (Request_failed (Printf.sprintf "peptide search returned HTTP %d" status))
  | None, _ ->
      raise (Request_failed "peptide search returned an invalid HTTP response")

let peptide_search_accessions ?(max_attempts = 10) ?(wait_seconds = 3) sequence =
  let job_url = peptide_search_job sequence in
  let rec poll attempt =
    if attempt > max_attempts then
      raise (Request_failed "peptide search timed out before returning results")
    else
      let response =
        run_command
          [
            "curl";
            "-sS";
            "-i";
            "--suppress-connect-headers";
            "--max-time";
            "30";
            job_url;
          ]
      in
      match parse_status response with
      | Some 200 ->
          response_body response
          |> String.split_on_char ','
          |> List.map String.trim
          |> List.filter (fun accession -> accession <> "")
      | Some (202 | 303) ->
          let delay =
            match parse_retry_after response with
            | Some seconds when seconds > 0 -> seconds
            | _ -> wait_seconds
          in
          Unix.sleep delay;
          poll (attempt + 1)
      | Some status ->
          raise (Request_failed (Printf.sprintf "peptide search job returned HTTP %d" status))
      | None ->
          raise (Request_failed "peptide search job returned an invalid HTTP response")
  in
  poll 1

let entry_request ?(format = Json) api dataset accession =
  let entry = accession ^ "." ^ format_extension format in
  request GET (path api [dataset_path dataset; entry])

let search_request ?(format = Json) ?(fields = []) ?size api dataset search_query =
  let params = [
    ("query", search_query);
    ("format", format_extension format);
    ("fields", comma_separated fields);
  ] in
  let params =
    match size with
    | None -> params
    | Some size -> params @ [("size", string_of_int size)]
  in
  request GET (with_query (path api [dataset_path dataset; "search"]) params)

let stream_request ?(format = Json) ?(fields = []) api dataset search_query =
  request GET
    (with_query
       (path api [dataset_path dataset; "stream"])
       [
         ("query", search_query);
         ("format", format_extension format);
         ("fields", comma_separated fields);
       ])

let id_mapping_run_request api ~from_db ~to_db ~ids =
  let body = query [
    ("from", from_db);
    ("to", to_db);
    ("ids", comma_separated ids);
  ] in
  let req = request ~body POST (path api ["idmapping"; "run"]) in
  { req with headers = ("Content-Type", "application/x-www-form-urlencoded") :: req.headers }

let id_mapping_status_request api job_id =
  request GET (path api ["idmapping"; "status"; job_id])

let id_mapping_results_request ?(format = Json) api job_id =
  request GET
    (with_query
       (path api ["idmapping"; "results"; job_id])
       [("format", format_extension format)])
