type t

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

val default_base_url : string
val open_api : ?base_url:string -> unit -> t
val base_url : t -> string
val default_headers : (string * string) list
val dataset_path : dataset -> string
val format_extension : format -> string
val perform_request : request -> string
val peptide_search_accessions : ?max_attempts:int -> ?wait_seconds:int -> string -> string list
val entry_request : ?format:format -> t -> dataset -> string -> request
val search_request : ?format:format -> ?fields:string list -> ?size:int -> t -> dataset -> string -> request
val stream_request : ?format:format -> ?fields:string list -> t -> dataset -> string -> request
val id_mapping_run_request : t -> from_db:string -> to_db:string -> ids:string list -> request
val id_mapping_status_request : t -> string -> request
val id_mapping_results_request : ?format:format -> t -> string -> request
