let project_root =
  Sys.executable_name |> Filename.dirname |> Filename.dirname
  |> Filename.dirname |> Filename.dirname

let out_dir = ref "tracer-out"

let sig_dir =
  ref (Filename.concat (Filename.dirname project_root) "signature-db")

let signatures = ref []

let feat = ref "manual1"

let feat_compare = ref "max-num"

let analysis_results_dir = ref ""

let threshold = ref 0.8

let train = ref false

let n_weighted_traces = ref 5

let opts =
  [
    ( "-out_dir",
      Arg.Set_string out_dir,
      "Output directory (default: " ^ !out_dir ^ ")" );
    ( "-sig_dir",
      Arg.Set_string sig_dir,
      "Signature directory (default: " ^ !sig_dir ^ ")" );
    ("-feat", Arg.Set_string feat, "Feature (default: " ^ !feat ^ ")");
    ( "-sig",
      Arg.String (fun x -> signatures := x :: !signatures),
      "Signatures for comparison" );
    ("-train", Arg.Set train, "Training mode (default: false)");
    ( "-n_weighted_traces",
      Arg.Set_int n_weighted_traces,
      "Number of weighted traces in result json file (default: "
      ^ string_of_int !n_weighted_traces
      ^ ")" );
    ( "-feat_compare",
      Arg.Set_string feat_compare,
      "Feature comparison method (default: " ^ !feat_compare ^ ")" );
  ]

let usage = "Usage: ./tracer [ ANALYSIS_RESULTS_DIR ]"
