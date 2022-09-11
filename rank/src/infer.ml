module P = Printf
module F = Format

let compare_int = compare

let compare_string = String.compare

module Feature = struct
  (* consistent with infer/infer/src/api-misuse/APIMisuseTrace.ml *)

  type typ = Unsupported [@@deriving compare, show, yojson]

  type const = Cint of int | UnknownConst [@@deriving compare, show, yojson]

  type exp =
    | Var
    | Const of const
    | BinOp of string * exp * exp
    | UnOp of string * exp
    | Cast of typ * exp
    | Sizeof
    | UnknownExp
  [@@deriving compare, show, yojson]

  let rec is_const e =
    match e with
    | Var | UnknownExp -> false
    | Const _ | Sizeof -> true
    | BinOp (_, e1, e2) -> is_const e1 && is_const e2
    | UnOp (_, e) | Cast (_, e) -> is_const e

  let rec eval_const e =
    match e with
    | Const const -> ( match const with Cint i -> Some i | _ -> None )
    | BinOp (bop, e1, e2) -> (
        match (eval_const e1, eval_const e2) with
        | Some v1, Some v2 -> (
            match bop with
            | "+" -> Some (v1 + v2)
            | "-" -> Some (v1 - v2)
            | "*" -> Some (v1 * v2)
            | "/" -> Some (v1 / v2)
            | _ -> None )
        | _, _ -> None )
    | UnOp (unop, e) -> (
        match eval_const e with
        | Some v -> (
            match unop with "+" -> Some v | "-" -> Some (-v) | _ -> None )
        | None -> None )
    | Var | Cast _ | Sizeof | UnknownExp -> None

  let is_const_int n e =
    match eval_const e with Some v -> v = n | None -> false

  let is_var e = match e with UnknownExp -> false | _ -> not (is_const e)

  type t =
    | Input of string
    | Store of exp * exp
    | Prune of exp
    | Call of string
    | LibraryCall of string
    | IntOverflow of string * exp
    | IntUnderflow of string * exp
    | FormatString of string * exp
    | CmdInjection of string * exp
    | BufferOverflow of string * exp
    | Allocate of string
    | Free of string * exp
    | Unknown
  [@@deriving compare, show, yojson]

  let const_of_json c =
    try
      match c with
      | `List [ `String "Cint"; `String i ] -> Cint (int_of_string i)
      | _ -> UnknownConst
    with _ -> UnknownConst

  let rec exp_of_json = function
    | `List [ `String "Var" ] -> Var
    | `List [ `String "Const"; const ] -> Const (const_of_json const)
    | `List [ `String "BinOp"; `String bop; lexp; rexp ] ->
        BinOp (bop, exp_of_json lexp, exp_of_json rexp)
    | `List [ `String "UnOp"; `String uop; exp ] -> UnOp (uop, exp_of_json exp)
    | `List [ `String "Cast"; _; exp ] -> Cast (Unsupported, exp_of_json exp)
    | `List [ `String "Sizeof"; _ ] -> Sizeof
    | _ -> UnknownExp

  let from_json = function
    | `List [ `String "Input"; `String pname ] -> Input pname
    | `List [ `String "Store"; lexp; rexp ] ->
        Store (exp_of_json lexp, exp_of_json rexp)
    | `List [ `String "Prune"; exp ] -> Prune (exp_of_json exp)
    | `List [ `String "Call"; `String pname ] -> Call pname
    | `List [ `String "LibraryCall"; `String pname; _ ] -> LibraryCall pname
    | `List [ `String "IntOverflow"; `String pname; exp ] ->
        IntOverflow (pname, exp_of_json exp)
    | `List [ `String "IntUnderflow"; `String pname; exp ] ->
        IntUnderflow (pname, exp_of_json exp)
    | `List [ `String "FormatString"; `String pname; exp ] ->
        FormatString (pname, exp_of_json exp)
    | `List [ `String "CmdInjection"; `String pname; exp ] ->
        CmdInjection (pname, exp_of_json exp)
    | `List [ `String "BufferOverflow"; `String pname; exp ] ->
        BufferOverflow (pname, exp_of_json exp)
    | `List [ `String "Allocate"; `String pname] -> Allocate pname
    | `List [ `String "Free"; `String pname; exp ] ->
      Free (pname, exp_of_json exp)
    | json ->
        F.eprintf "WARN: unknown feature %a\n" Yojson.Safe.pp json;
        Unknown

  let make_feature str = Yojson.Safe.from_string str |> from_json
end

module Step = struct
  type t = { file : string; line : int; col : int; feature : Feature.t }
  [@@deriving compare, show, yojson]

  let convert json =
    {
      file =
        json |> Yojson.Safe.Util.member "filename" |> Yojson.Safe.Util.to_string;
      line =
        json |> Yojson.Safe.Util.member "line_number" |> Yojson.Safe.Util.to_int;
      col =
        json
        |> Yojson.Safe.Util.member "column_number"
        |> Yojson.Safe.Util.to_int;
      feature =
        json
        |> Yojson.Safe.Util.member "feature"
        |> Yojson.Safe.Util.to_string |> Feature.make_feature;
    }
end

module Trace = struct
  type t = Step.t list [@@deriving show, yojson]

  let compare = compare
end

module TraceSet = struct
  type t = Trace.t list

  let of_list x = x

  let cardinal = List.length

  let choose = List.hd

  let fold f s a = List.fold_left (fun acc e -> f e acc) a s

  let union a b = List.append a b
end

module Signature = struct
  type t = { id : string; traces : TraceSet.t }

  let parse id json_file_name =
    let traces =
      json_file_name |> Yojson.Safe.from_file
      |> Yojson.Safe.Util.member "bug_trace"
      |> Yojson.Safe.Util.convert_each (fun json ->
             json |> Yojson.Safe.Util.convert_each Step.convert)
    in
    { id; traces }
end

module Alarm = struct
  type t = {
    bug_type : string;
    src : Location.t;
    sink : Location.t;
    traces : TraceSet.t;
  }

  let make bug_type src sink traces = { bug_type; src; sink; traces }

  let get_bug_type a = a.bug_type

  let get_src a = a.src

  let get_sink a = a.sink

  let get_traces a = a.traces

  let equal { bug_type = bt1; src = src1; sink = sink1; _ }
      { bug_type = bt2; src = src2; sink = sink2; _ } =
    String.equal bt1 bt2 && Location.equal src1 src2
    && Location.equal sink1 sink2

  let parse alarm =
    let get_nth_loc n (trace : Step.t list) =
      try
        let elem = List.nth trace n in
        { Location.file = elem.file; line = elem.line }
      with _ -> { file = ""; line = -1 }
    in
    let bug_type =
      alarm |> Yojson.Safe.Util.member "qualifier" |> Yojson.Safe.to_string
    in
    let traces =
      alarm
      |> Yojson.Safe.Util.member "bug_trace"
      |> Yojson.Safe.Util.convert_each (fun json ->
             json |> Yojson.Safe.Util.convert_each Step.convert)
      |> TraceSet.of_list
    in
    let selected_trace = try TraceSet.choose traces with _ -> [] in
    let src = get_nth_loc 0 selected_trace in
    let sink = get_nth_loc (List.length selected_trace - 1) selected_trace in
    { bug_type; src; sink; traces }
end

module Report = struct
  type t = Alarm.t list

  let remove_dup_alarms alarms =
    List.fold_left
      (fun result alarm ->
        let visited = List.exists (fun a -> Alarm.equal a alarm) result in
        if visited then result
        else
          let combined_traceset =
            List.fold_left
              (fun ts a ->
                if Alarm.equal a alarm then
                  TraceSet.union ts (Alarm.get_traces a)
                else ts)
              (TraceSet.of_list []) alarms
          in
          let new_alarm =
            Alarm.make (Alarm.get_bug_type alarm) (Alarm.get_src alarm)
              (Alarm.get_sink alarm) combined_traceset
          in
          new_alarm :: result)
      [] alarms

  let parse json_file_name =
    try
      json_file_name |> Yojson.Safe.from_file
      |> Yojson.Safe.Util.convert_each Alarm.parse
      |> remove_dup_alarms
    with Sys_error _ -> []
end
