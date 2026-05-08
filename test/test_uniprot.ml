open OUnit2
open Tidescript

let test_entry_request _ =
  let api = Uniprot.open_api () in
  let req = Uniprot.entry_request api Uniprot.UniProtKB "P12345" in
  assert_equal Uniprot.GET req.meth;
  assert_equal "https://rest.uniprot.org/uniprotkb/P12345.json" req.url;
  assert_equal None req.body

let test_search_request _ =
  let api = Uniprot.open_api () in
  let req =
    Uniprot.search_request
      ~format:Uniprot.Tsv
      ~fields:["accession"; "sequence"]
      ~size:25
      api
      Uniprot.UniProtKB
      "gene:insulin AND organism_id:9606"
  in
  assert_equal Uniprot.GET req.meth;
  assert_equal
    "https://rest.uniprot.org/uniprotkb/search?query=gene%3Ainsulin%20AND%20organism_id%3A9606&format=tsv&fields=accession%2Csequence&size=25"
    req.url

let test_id_mapping_run_request _ =
  let api = Uniprot.open_api () in
  let req =
    Uniprot.id_mapping_run_request
      api
      ~from_db:"UniProtKB_AC-ID"
      ~to_db:"GeneID"
      ~ids:["P12345"; "Q8N158"]
  in
  assert_equal Uniprot.POST req.meth;
  assert_equal "https://rest.uniprot.org/idmapping/run" req.url;
  assert_equal
    (Some "from=UniProtKB_AC-ID&to=GeneID&ids=P12345%2CQ8N158")
    req.body

let suite =
  "uniprot" >::: [
    "entry request" >:: test_entry_request;
    "search request" >:: test_search_request;
    "id mapping run request" >:: test_id_mapping_run_request;
  ]

let () = run_test_tt_main suite
