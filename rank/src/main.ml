module F = Format

let init_out_dir () =
  try Unix.mkdir !Options.out_dir 0o775
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let init_dir name =
  let final_dir = Filename.concat !Options.out_dir name in
  (try Unix.mkdir (final_dir |> Filename.dirname |> Filename.dirname) 0o775
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir (final_dir |> Filename.dirname) 0o775
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir final_dir 0o775
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  final_dir

let get_report_json target_name =
  Filename.concat
    (Filename.concat
       (Filename.concat !Options.analysis_results_dir target_name)
       "infer-out")
    "report.json"

let get_signature_trace_lst signature_program_name =
  try
    Filename.concat !Options.sig_dir signature_program_name
    |> Sys.readdir |> Array.to_list
  with Sys_error s ->
    F.printf "WARN: %s\n" s;
    []

let read_signatures () =
  match !Options.signatures with
  | [] ->
      (* read all signatures *)
      let sig_list = !Options.sig_dir |> Sys.readdir |> Array.to_list in
      List.fold_left
        (fun sigs program ->
          let traces = get_signature_trace_lst program in
          List.fold_left
            (fun sigs filename ->
              let sig_name =
                program ^ "-" ^ (String.split_on_char '.' filename |> List.hd)
              in
              let pathname =
                Filename.concat
                  (Filename.concat !Options.sig_dir program)
                  filename
              in
              let sig_trace = Infer.Signature.parse sig_name pathname in
              sig_trace :: sigs)
            sigs traces)
        [] sig_list
  | lst ->
      List.map
        (fun x ->
          let sig_name =
            (Filename.dirname x |> Filename.basename)
            ^ "-"
            ^ (Filename.basename x |> String.split_on_char '.' |> List.hd)
          in
          Infer.Signature.parse sig_name x)
        lst

let read_target_program_lst () =
  !Options.analysis_results_dir
  |> Sys.readdir |> Array.to_list
  |> List.filter (fun d ->
         Filename.concat !Options.analysis_results_dir d |> Sys.is_directory)

let report_time analyze_time_lst =
  let oname = Filename.concat !Options.out_dir "analyze_time.json" in
  let oc = open_out oname in
  List.iter
    (fun (name, time) -> Printf.fprintf oc "%s : %f\n" name time)
    analyze_time_lst

let run feat_m =
  let module Feat = (val feat_m : Feature.FEAT_VEC) in
  let module Rank = Rank.Make (Feat) in
  let module SigRankMap = Rank.SigRankMap in
  let signatures = read_signatures () |> Rank.encode_signatures in
  let target_program_lst = read_target_program_lst () in
  let rank compute signatures target_program_name map =
    let target_report_json_file_name = get_report_json target_program_name in
    let report = Infer.Report.parse target_report_json_file_name in
    let signatures =
      if !Options.train then
        List.filter
          (fun s ->
            Str.string_match
              (Str.regexp (target_program_name ^ ".*"))
              s.Rank.EncodedSignature.id 0
            |> not)
          signatures
      else signatures
    in
    compute target_program_name signatures report map
  in
  List.iter
    (fun target_program_name ->
      F.printf "Scanning %s\n" target_program_name;
      let target_program_map =
        rank Rank.compute signatures target_program_name SigRankMap.empty
      in
      Rank.report target_program_map)
    target_program_lst

let main () =
  Arg.parse Options.opts
    (fun x -> Options.analysis_results_dir := x)
    Options.usage;
  F.printf "sig_dir: %s\n" !Options.sig_dir;

  init_out_dir ();
  match !Options.feat with
  | "manual1" -> run (module Feature.Manual1)
  | "manual2" -> run (module Feature.Manual2)
  | "manual3" -> run (module Feature.Manual3)
  | "manual4" -> run (module Feature.Manual4)
  | "general" -> run (module Feature.General)
  | "high" -> run (module Feature.HighLevel)
  | "high_equal_input" -> run (module Feature.HighLevelEqualInput)
  | _ -> failwith "Unknown feature encoding"

let _ = main ()
