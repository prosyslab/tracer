module P = Printf
module F = Format
module Trace = Infer.Trace
module TraceSet = Infer.TraceSet

module Make (FeatVec : Feature.FEAT_VEC) = struct
  module WeightedTrace = struct
    type t = {
      signature : string;
      trace : Trace.t;
      vec : FeatVec.t;
      score : float;
    }

    let make signature trace vec score = { signature; trace; vec; score }

    let compare a b =
      if b.score -. a.score = 0. then 0
      else if b.score -. a.score > 0. then 1
      else -1

    let to_json wt =
      `Assoc
        [
          ("signature", `String wt.signature);
          ("trace", Trace.to_yojson wt.trace);
          ("vec", FeatVec.to_yojson wt.vec);
          ("score", `Float wt.score);
        ]
  end

  module WeightedAlarm = struct
    type t = {
      program : string;
      src : Location.t;
      sink : Location.t;
      bug_type : string;
      weighted_traces : WeightedTrace.t list;
    }

    let compare a b =
      match (a.weighted_traces, b.weighted_traces) with
      | [], [] -> 0
      | _, [] -> -1
      | [], _ -> 1
      | a1 :: _, b1 :: _ -> WeightedTrace.compare a1 b1

    let list_of_weighted_traces n wtraces =
      let result =
        List.filteri (fun i _ -> i < n) wtraces
        |> List.map WeightedTrace.to_json
      in
      result

    let list_of_n_weighted_traces n wtraces = list_of_weighted_traces n wtraces

    let make program src sink bug_type weighted_traces =
      { program; src; sink; bug_type; weighted_traces }

    let to_json stat =
      `Assoc
        [
          ("program", `String stat.program);
          ("src", `Int stat.src.line);
          ("sink", `Int stat.sink.line);
          ( "weighted_traces",
            `List
              (list_of_n_weighted_traces !Options.n_weighted_traces
                 stat.weighted_traces) );
        ]
  end

  let maximum_feature_trace traces =
    ( traces
    |> List.map (fun tr -> (tr, FeatVec.of_trace tr))
    |> List.sort (fun (_, fv1) (_, fv2) ->
           FeatVec.num_of_features fv2 - FeatVec.num_of_features fv1)
    |> List.nth_opt )
      0

  let filter_vectors_no_filter target_vectors = target_vectors

  let filter_vectors_max_num target_vectors =
    if List.length target_vectors == 0 then target_vectors
    else
      let feature_num_max =
        List.map (fun (_, fv) -> FeatVec.num_of_features fv) target_vectors
        |> List.sort (fun a b -> b - a)
        |> List.hd
      in
      List.filter
        (fun (_, fv) -> FeatVec.num_of_features fv == feature_num_max)
        target_vectors

  let filter_vectors_max_var target_vectors =
    if List.length target_vectors == 0 then target_vectors
    else
      let feature_var_max =
        List.map
          (fun (_, fv) -> FeatVec.variation_of_features fv)
          target_vectors
        |> List.sort (fun a b -> b - a)
        |> List.hd
      in
      List.filter
        (fun (_, fv) -> FeatVec.variation_of_features fv == feature_var_max)
        target_vectors

  let filter_vectors_at_least target_vectors =
    let result =
      List.filter (fun (tr, _) -> List.length tr > 3) target_vectors
    in
    if List.length result == 0 then target_vectors else result

  let filter_vectors =
    match !Options.feat_compare with
    | "no-filter" -> filter_vectors_no_filter
    | "max-num" -> filter_vectors_max_num
    | "max-var" -> filter_vectors_max_var
    | "at-least" -> filter_vectors_at_least
    | _ -> failwith "Unknown feature comparison method"

  let filter_traces target_traces =
    let target_vectors =
      target_traces |> List.map (fun tr -> (tr, FeatVec.of_trace tr))
    in
    filter_vectors target_vectors

  module EncodedSignature = struct
    type t = { id : string; label : string option; trace : FeatVec.t }
    [@@deriving to_yojson]

    let of_signature { Infer.Signature.id; traces } =
      let substr_until_rdash s = String.sub s 0 (String.rindex s '-') in
      let label = Some (id |> substr_until_rdash |> substr_until_rdash) in
      match maximum_feature_trace traces with
      | Some (_, trace) -> { id; label; trace }
      | None ->
          raise
            (Invalid_argument (Printf.sprintf "signature %s has no trace" id))
  end

  module SigRankMap = Map.Make (String)

  let compute_weighted_traces signatures target_traces =
    let target_vectors = filter_traces target_traces in
    List.fold_left
      (fun lst signature ->
        let measure = FeatVec.similarity signature.EncodedSignature.trace in
        let weighted_traces =
          List.map
            (fun (tr, fv) ->
              WeightedTrace.make signature.EncodedSignature.id tr fv
                (measure fv))
            target_vectors
        in
        List.append weighted_traces lst)
      [] signatures
    |> List.sort WeightedTrace.compare

  let encode_signatures raw_signatures =
    List.map EncodedSignature.of_signature raw_signatures

  let compute program signatures report map =
    let wreport =
      List.fold_left
        (fun lst { Infer.Alarm.bug_type; src; sink; traces } ->
          let wtraces = compute_weighted_traces signatures traces in
          WeightedAlarm.make program src sink bug_type wtraces :: lst)
        [] report
      |> List.sort WeightedAlarm.compare
    in
    SigRankMap.add program wreport map

  let report_text map =
    SigRankMap.iter
      (fun program wreport ->
        let oname = Filename.concat !Options.out_dir (program ^ ".txt") in
        let oc = open_out oname in
        List.iteri
          (fun i walarm ->
            let score =
              match walarm.WeightedAlarm.weighted_traces with
              | [] -> 0.0
              | h :: _ -> h.WeightedTrace.score
            in
            Printf.fprintf oc
              "%d. src: %s:%d, sink: %s:%d, score: %f, bug_type: %s\n" (i + 1)
              walarm.WeightedAlarm.src.file walarm.WeightedAlarm.src.line
              walarm.WeightedAlarm.sink.file walarm.WeightedAlarm.sink.line
              score walarm.WeightedAlarm.bug_type)
          wreport;
        close_out oc)
      map

  let report_json map =
    SigRankMap.iter
      (fun program wreport ->
        let oname = Filename.concat !Options.out_dir (program ^ ".json") in
        let oc = open_out oname in
        let a = `List (List.map WeightedAlarm.to_json wreport) in
        let b = Yojson.Safe.to_string a in
        let c = Yojson.Safe.prettify b in
        Printf.fprintf oc "%s" c;
        close_out oc)
      map

  let report map =
    report_text map;
    report_json map

  let write_csv csv_file_name sr_map =
    let legend_lst, rank_lst =
      SigRankMap.fold
        (fun sig_prog_name piled_rank (sig_prog_acc, rank_acc) ->
          if rank_acc = [] then ([ sig_prog_name ], piled_rank)
          else
            ( sig_prog_acc @ [ sig_prog_name ],
              List.fold_left2
                (fun acc a b ->
                  let same_rank_concatenated = [ a @ b ] in
                  if acc = [] then same_rank_concatenated
                  else acc @ same_rank_concatenated)
                [] rank_acc piled_rank ))
        sr_map ([], [])
    in
    let legend_str = legend_lst |> String.concat ",," in
    let rank_str =
      rank_lst |> List.map (String.concat ",") |> String.concat "\n"
    in
    let oc = open_out (Filename.concat !Options.out_dir csv_file_name) in
    Printf.fprintf oc "%s" (legend_str ^ "\n" ^ rank_str);
    close_out oc
end
