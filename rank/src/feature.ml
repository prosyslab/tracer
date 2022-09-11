module Step = Infer.Step
module IFeat = Infer.Feature

let compare_string = String.compare

let compare_bool = compare

module type FEAT_VEC = sig
  type t

  val of_trace : Infer.Trace.t -> t

  val similarity : t -> t -> float

  val to_yojson : t -> Yojson.Safe.t

  val num_of_features : t -> int

  val variation_of_features : t -> int
end

module Manual1 = struct
  module Feature = struct
    type op = string [@@deriving compare, show, yojson]

    type is_const = bool [@@deriving compare, show, yojson]

    type t =
      | Input of string
      | BinOp of string
      | Prune of op * is_const
      | LibraryCall of string
      | IntOverflow
      | IntUnderflow
      | FormatString
      | CmdInjection
      | BufferOverflow
      | Unknown
    [@@deriving compare, show, yojson]
  end

  module FeatMap = Map.Make (Feature)

  type t = int FeatMap.t

  let num_of_features t = FeatMap.fold (fun _ n cnt -> cnt + n) t 0

  let variation_of_features t = FeatMap.cardinal t

  let to_yojson m : Yojson.Safe.t =
    `List
      (FeatMap.fold
         (fun feat num lst -> `List [ Feature.to_yojson feat; `Int num ] :: lst)
         m [])

  let update = function Some n -> Some (n + 1) | None -> Some 1

  let rec of_exp exp map =
    match exp with
    | IFeat.BinOp (op, e1, e2) ->
        FeatMap.update (BinOp op) update map |> of_exp e1 |> of_exp e2
    | IFeat.Cast (_, e) -> of_exp e map
    | _ -> map

  let rel_op = [ "=="; "!="; "<"; ">"; "<="; ">=" ]

  let rec of_prune e map =
    match e with
    | IFeat.BinOp (op, IFeat.Var, IFeat.Const _)
    | IFeat.BinOp (op, IFeat.Var, IFeat.Cast (_, IFeat.Const _))
    | IFeat.BinOp (op, IFeat.Const _, IFeat.Var)
    | IFeat.BinOp (op, IFeat.Cast (_, IFeat.Const _), IFeat.Var)
      when List.mem op rel_op ->
        FeatMap.update (Prune (op, true)) update map
    | IFeat.UnOp (_, e) -> of_prune e map
    | _ -> map

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input s -> FeatMap.update (Input s) update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.LibraryCall s -> FeatMap.update (LibraryCall s) update map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | _ -> map

  let of_trace tr =
    List.fold_left (fun map step -> of_instr step map) FeatMap.empty tr

  let empty_vec = FeatMap.empty

  let norm v =
    FeatMap.fold (fun _ n acc -> (n * n) + acc) v 0
    |> float_of_int |> Float.sqrt

  let type_of v =
    let filtered_map =
      v
      |> FeatMap.filter (fun feat _ ->
             match feat with
             | Feature.IntOverflow | IntUnderflow | CmdInjection | FormatString
             | BufferOverflow ->
                 true
             | _ -> false)
    in
    (* Empty trace is possible *)
    assert (FeatMap.cardinal filtered_map <= 1);
    try filtered_map |> FeatMap.choose |> fst with Not_found -> Unknown

  let similarity v1 v2 =
    let v1_typ = v1 |> type_of in
    let v2_typ = v2 |> type_of in
    if v1_typ = v2_typ then
      let merged =
        FeatMap.merge
          (fun feat n1 n2 ->
            match (feat, n1, n2) with
            | Unknown, _, _ -> Some 0
            | _, Some c1, Some c2 -> Some (c1 * c2)
            | _, Some _, None | _, None, Some _ -> Some 0
            | _ -> None)
          v1 v2
      in
      let unnormalized =
        FeatMap.fold (fun _ v acc -> v + acc) merged 0 |> float_of_int
      in
      let nv1 = v1 |> norm in
      let nv2 = v2 |> norm in
      unnormalized /. (nv1 *. nv2)
    else 0.0
end

module Manual2 = struct
  module Feature = struct
    type op = string [@@deriving compare, show, yojson]

    type is_const = bool [@@deriving compare, show, yojson]

    type t =
      | Input of string
      | BinOp of string
      | UnOp of string
      | Prune of op * is_const
      | IntOverflow
      | IntUnderflow
      | FormatString
      | CmdInjection
      | BufferOverflow
      | Unknown
    [@@deriving compare, show, yojson]
  end

  module FeatMap = Map.Make (Feature)

  type t = int FeatMap.t

  let num_of_features t = FeatMap.fold (fun _ n cnt -> cnt + n) t 0

  let variation_of_features t = FeatMap.cardinal t

  let to_yojson m : Yojson.Safe.t =
    `List
      (FeatMap.fold
         (fun feat num lst -> `List [ Feature.to_yojson feat; `Int num ] :: lst)
         m [])

  let update = function Some n -> Some (n + 1) | None -> Some 1

  let rec of_exp exp map =
    match exp with
    | IFeat.BinOp (op, e1, e2) ->
        FeatMap.update (BinOp op) update map |> of_exp e1 |> of_exp e2
    | IFeat.Cast (_, e) -> of_exp e map
    | _ -> map

  let rel_op = [ "=="; "!="; "<"; ">"; "<="; ">=" ]

  let rec of_prune e map =
    match e with
    | IFeat.BinOp (op, e1, e2) when List.mem op rel_op ->
        FeatMap.update (Prune (op, true)) update map
        |> of_prune e1 |> of_prune e2
    | IFeat.UnOp (op, e) ->
        FeatMap.update (Prune (op, true)) update map |> of_prune e
    | _ -> map

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input s -> FeatMap.update (Input s) update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | _ -> map

  let of_trace tr =
    List.fold_left (fun map step -> of_instr step map) FeatMap.empty tr

  let norm v =
    FeatMap.fold (fun _ n acc -> (n * n) + acc) v 0
    |> float_of_int |> Float.sqrt

  let type_of v =
    let filtered_map =
      v
      |> FeatMap.filter (fun feat _ ->
             match feat with
             | Feature.IntOverflow | IntUnderflow | CmdInjection | FormatString
             | BufferOverflow ->
                 true
             | _ -> false)
    in
    (* Empty trace is possible *)
    assert (FeatMap.cardinal filtered_map <= 1);
    try filtered_map |> FeatMap.choose |> fst with Not_found -> Unknown

  let similarity v1 v2 =
    let v1_typ = v1 |> type_of in
    let v2_typ = v2 |> type_of in
    if v1_typ = v2_typ then
      let merged =
        FeatMap.merge
          (fun feat n1 n2 ->
            match (feat, n1, n2) with
            | Unknown, _, _ -> Some 0
            | _, Some c1, Some c2 -> Some (c1 * c2)
            | _, Some _, None | _, None, Some _ -> Some 0
            | _ -> None)
          v1 v2
      in
      let unnormalized =
        FeatMap.fold (fun _ v acc -> v + acc) merged 0 |> float_of_int
      in
      let nv1 = v1 |> norm in
      let nv2 = v2 |> norm in
      unnormalized /. (nv1 *. nv2)
    else 0.0
end

module Manual3 = struct
  include Manual1

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input _ -> FeatMap.update (Input "") update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | _ -> map
end

module Manual4 = struct
  include Manual2

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input _ -> FeatMap.update (Input "") update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | _ -> map
end

module General = struct
  module Feature = struct
    type op = string [@@deriving compare, show, yojson]

    type is_const = bool [@@deriving compare, show, yojson]

    type t =
      | Input of string
      | BinOp of string
      | UnOp of string
      | Prune of op * is_const
      | LibraryCall of string
      | IntOverflow
      | IntUnderflow
      | FormatString
      | CmdInjection
      | BufferOverflow
      | Allocate
      | Free
      | Unknown
    [@@deriving compare, show, yojson]

    let to_string t =
      let concat_with_space sl = String.concat " " sl in
      match t with
      | Input s -> concat_with_space [ "Input"; s ]
      | BinOp s -> concat_with_space [ "BinOp"; s ]
      | UnOp s -> concat_with_space [ "UnOp"; s ]
      | Prune (op, is_const) ->
          concat_with_space [ "Prune"; op; Bool.to_string is_const ]
      | LibraryCall s -> concat_with_space [ "LibraryCall"; s ]
      | IntOverflow -> "IntOverflow"
      | IntUnderflow -> "IntUnderflow"
      | FormatString -> "FormatString"
      | CmdInjection -> "CmdInjection"
      | BufferOverflow -> "BufferOverflow"
      | Allocate -> "Allocate"
      | Free -> "Free"
      | Unknown -> "Unknown"
  end

  module FeatMap = Map.Make (Feature)

  type t = int FeatMap.t

  let num_of_features t = FeatMap.fold (fun _ n cnt -> cnt + n) t 0

  let variation_of_features t = FeatMap.cardinal t

  let to_yojson m : Yojson.Safe.t =
    `Assoc
      (FeatMap.fold
         (fun feat num lst -> (Feature.to_string feat, `Int num) :: lst)
         m [])

  let update = function Some n -> Some (n + 1) | None -> Some 1

  let rec of_exp exp map =
    match exp with
    | IFeat.UnOp (op, e) -> FeatMap.update (UnOp op) update map |> of_exp e
    | IFeat.BinOp (op, e1, e2) ->
        FeatMap.update (BinOp op) update map |> of_exp e1 |> of_exp e2
    | IFeat.Cast (_, e) -> of_exp e map
    | _ -> map

  let rel_op = [ "=="; "!="; "<"; ">"; "<="; ">=" ]

  let rec of_prune e map =
    match e with
    | IFeat.BinOp (op, e1, e2) when List.mem op rel_op ->
        FeatMap.update (Prune (op, true)) update map
        |> of_prune e1 |> of_prune e2
    | IFeat.UnOp (op, e) ->
        FeatMap.update (Prune (op, true)) update map |> of_prune e
    | _ -> map

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input s -> FeatMap.update (Input s) update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.LibraryCall s -> FeatMap.update (LibraryCall s) update map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | IFeat.Allocate _ -> FeatMap.update Allocate update map
    | IFeat.Free _ -> FeatMap.update Free update map
    | _ -> map

  let of_trace tr =
    List.fold_left (fun map step -> of_instr step map) FeatMap.empty tr

  let empty_vec = FeatMap.empty

  let norm v =
    FeatMap.fold (fun _ n acc -> (n * n) + acc) v 0
    |> float_of_int |> Float.sqrt

  let type_of v =
    let filtered_map =
      v
      |> FeatMap.filter (fun feat _ ->
             match feat with
             | Feature.IntOverflow | IntUnderflow | CmdInjection | FormatString
             | BufferOverflow ->
                 true
             | _ -> false)
    in
    (* Empty trace is possible *)
    assert (FeatMap.cardinal filtered_map <= 1);
    try filtered_map |> FeatMap.choose |> fst with Not_found -> Unknown

  let similarity v1 v2 =
    let v1_typ = v1 |> type_of in
    let v2_typ = v2 |> type_of in
    if v1_typ = v2_typ then
      let merged =
        FeatMap.merge
          (fun feat n1 n2 ->
            match (feat, n1, n2) with
            | Unknown, _, _ -> Some 0
            | _, Some c1, Some c2 -> Some (c1 * c2)
            | _, Some _, None | _, None, Some _ -> Some 0
            | _ -> None)
          v1 v2
      in
      let unnormalized =
        FeatMap.fold (fun _ v acc -> v + acc) merged 0 |> float_of_int
      in
      let nv1 = v1 |> norm in
      let nv2 = v2 |> norm in
      unnormalized /. (nv1 *. nv2)
    else 0.0
end

module HighLevel = struct
  module Feature = struct
    type op = string [@@deriving compare, show, yojson]

    type is_const = bool [@@deriving compare, show, yojson]

    type t =
      | Input of string
      | BinOp of string
      | UnOp of string
      | Prune of op * is_const
      | LibraryCall of string
      | IntOverflow
      | IntUnderflow
      | FormatString
      | CmdInjection
      | BufferOverflow
      | Allocate
      | Free
      | IfLargerThanConst
      | IfSmallerThanConst
      | IfEqualToVar
      | IfNotEqualToVar
      | IfEqualToPercentage
      | Unknown
    [@@deriving compare, show, yojson]

    let to_string t =
      let concat_with_space sl = String.concat " " sl in
      match t with
      | Input s -> concat_with_space [ "Input"; s ]
      | BinOp s -> concat_with_space [ "BinOp"; s ]
      | UnOp s -> concat_with_space [ "UnOp"; s ]
      | Prune (op, is_const) ->
          concat_with_space [ "Prune"; op; Bool.to_string is_const ]
      | LibraryCall s -> concat_with_space [ "LibraryCall"; s ]
      | IntOverflow -> "IntOverflow"
      | IntUnderflow -> "IntUnderflow"
      | FormatString -> "FormatString"
      | CmdInjection -> "CmdInjection"
      | BufferOverflow -> "BufferOverflow"
      | Allocate -> "Allocate"
      | Free -> "Free"
      | IfLargerThanConst -> "IfLargerThanConst"
      | IfSmallerThanConst -> "IfSmallerThanConst"
      | IfEqualToVar -> "IfEqualToVar"
      | IfNotEqualToVar -> "IfNotEqualToVar"
      | IfEqualToPercentage -> "IfEqualToPercentage"
      | Unknown -> "Unknown"
  end

  module FeatMap = Map.Make (Feature)

  type t = int FeatMap.t

  let num_of_features t = FeatMap.fold (fun _ n cnt -> cnt + n) t 0

  let variation_of_features t = FeatMap.cardinal t

  let to_yojson m : Yojson.Safe.t =
    `Assoc
      (FeatMap.fold
         (fun feat num lst -> (Feature.to_string feat, `Int num) :: lst)
         m [])

  let update = function Some n -> Some (n + 1) | None -> Some 1

  let rec of_exp exp map =
    match exp with
    | IFeat.UnOp (op, e) -> FeatMap.update (UnOp op) update map |> of_exp e
    | IFeat.BinOp (op, e1, e2) ->
        FeatMap.update (BinOp op) update map |> of_exp e1 |> of_exp e2
    | IFeat.Cast (_, e) -> of_exp e map
    | _ -> map

  let rel_op = [ "=="; "!="; "<"; ">"; "<="; ">=" ]

  let update_map_by_pred pred feature map =
    if pred then FeatMap.update feature update map else map

  let check_if_larger_than_const op e1 e2 =
    if
      (String.equal op ">" || String.equal op ">=")
      && Infer.Feature.is_const e2
      || (String.equal op "<" || String.equal op "<=")
         && Infer.Feature.is_const e1
    then true
    else false

  let check_if_smaller_than_const op e1 e2 =
    if
      (String.equal op "<" || String.equal op "<=")
      && Infer.Feature.is_const e2
      || (String.equal op ">" || String.equal op ">=")
         && Infer.Feature.is_const e1
    then true
    else false

  let check_if_equal_to_var op e1 e2 =
    if
      String.equal op "==" && Infer.Feature.is_var e1 && Infer.Feature.is_var e2
    then true
    else false

  let check_if_not_equal_to_var op e1 e2 =
    if
      String.equal op "!=" && Infer.Feature.is_var e1 && Infer.Feature.is_var e2
    then true
    else false

  let check_if_equal_to_percentage op e1 e2 =
    if
      String.equal op "=="
      && ( (Infer.Feature.is_var e1 && Infer.Feature.is_const_int 37 e2)
         || (Infer.Feature.is_var e2 && Infer.Feature.is_const_int 37 e1) )
    then true
    else false

  let rec of_prune e map =
    match e with
    | IFeat.BinOp (op, e1, e2) when List.mem op rel_op ->
        FeatMap.update (Prune (op, true)) update map
        |> update_map_by_pred
             (check_if_larger_than_const op e1 e2)
             IfLargerThanConst
        |> update_map_by_pred
             (check_if_smaller_than_const op e1 e2)
             IfSmallerThanConst
        |> update_map_by_pred (check_if_equal_to_var op e1 e2) IfEqualToVar
        |> update_map_by_pred
             (check_if_not_equal_to_var op e1 e2)
             IfNotEqualToVar
        |> update_map_by_pred
             (check_if_equal_to_percentage op e1 e2)
             IfEqualToPercentage
        |> of_prune e1 |> of_prune e2
    | IFeat.UnOp (op, e) ->
        FeatMap.update (Prune (op, true)) update map |> of_prune e
    | _ -> map

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input s -> FeatMap.update (Input s) update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.LibraryCall s -> FeatMap.update (LibraryCall s) update map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | IFeat.Allocate _ -> FeatMap.update Allocate update map
    | IFeat.Free _ -> FeatMap.update Free update map
    | _ -> map

  let of_trace tr =
    List.fold_left (fun map step -> of_instr step map) FeatMap.empty tr

  let empty_vec = FeatMap.empty

  let norm v =
    FeatMap.fold (fun _ n acc -> (n * n) + acc) v 0
    |> float_of_int |> Float.sqrt

  let type_of v =
    let filtered_map =
      v
      |> FeatMap.filter (fun feat _ ->
             match feat with
             | Feature.IntOverflow | IntUnderflow | CmdInjection | FormatString
             | BufferOverflow ->
                 true
             | _ -> false)
    in
    (* Empty trace is possible *)
    assert (FeatMap.cardinal filtered_map <= 1);
    try filtered_map |> FeatMap.choose |> fst with Not_found -> Unknown

  let similarity v1 v2 =
    let v1_typ = v1 |> type_of in
    let v2_typ = v2 |> type_of in
    if v1_typ = v2_typ then
      let merged =
        FeatMap.merge
          (fun feat n1 n2 ->
            match (feat, n1, n2) with
            | Unknown, _, _ -> Some 0
            | _, Some c1, Some c2 -> Some (c1 * c2)
            | _, Some _, None | _, None, Some _ -> Some 0
            | _ -> None)
          v1 v2
      in
      let unnormalized =
        FeatMap.fold (fun _ v acc -> v + acc) merged 0 |> float_of_int
      in
      let nv1 = v1 |> norm in
      let nv2 = v2 |> norm in
      unnormalized /. (nv1 *. nv2)
    else 0.0
end

module HighLevelEqualInput = struct
  module Feature = struct
    type op = string [@@deriving compare, show, yojson]

    type is_const = bool [@@deriving compare, show, yojson]

    type t =
      | Input of string
      | BinOp of string
      | UnOp of string
      | Prune of op * is_const
      | LibraryCall of string
      | IntOverflow
      | IntUnderflow
      | FormatString
      | CmdInjection
      | BufferOverflow
      | IfLargerThanConst
      | IfSmallerThanConst
      | IfEqualToVar
      | IfNotEqualToVar
      | IfEqualToPercentage
      | Unknown
    [@@deriving compare, show, yojson]

    let to_string t =
      let concat_with_space sl = String.concat " " sl in
      match t with
      | Input s -> concat_with_space [ "Input"; s ]
      | BinOp s -> concat_with_space [ "BinOp"; s ]
      | UnOp s -> concat_with_space [ "UnOp"; s ]
      | Prune (op, is_const) ->
          concat_with_space [ "Prune"; op; Bool.to_string is_const ]
      | LibraryCall s -> concat_with_space [ "LibraryCall"; s ]
      | IntOverflow -> "IntOverflow"
      | IntUnderflow -> "IntUnderflow"
      | FormatString -> "FormatString"
      | CmdInjection -> "CmdInjection"
      | BufferOverflow -> "BufferOverflow"
      | IfLargerThanConst -> "IfLargerThanConst"
      | IfSmallerThanConst -> "IfSmallerThanConst"
      | IfEqualToVar -> "IfEqualToVar"
      | IfNotEqualToVar -> "IfNotEqualToVar"
      | IfEqualToPercentage -> "IfEqualToPercentage"
      | Unknown -> "Unknown"
  end

  module FeatMap = Map.Make (Feature)

  type t = int FeatMap.t

  let num_of_features t = FeatMap.fold (fun _ n cnt -> cnt + n) t 0

  let variation_of_features t = FeatMap.cardinal t

  let to_yojson m : Yojson.Safe.t =
    `Assoc
      (FeatMap.fold
         (fun feat num lst -> (Feature.to_string feat, `Int num) :: lst)
         m [])

  let update = function Some n -> Some (n + 1) | None -> Some 1

  let rec of_exp exp map =
    match exp with
    | IFeat.UnOp (op, e) -> FeatMap.update (UnOp op) update map |> of_exp e
    | IFeat.BinOp (op, e1, e2) ->
        FeatMap.update (BinOp op) update map |> of_exp e1 |> of_exp e2
    | IFeat.Cast (_, e) -> of_exp e map
    | _ -> map

  let rel_op = [ "=="; "!="; "<"; ">"; "<="; ">=" ]

  let update_map_by_pred pred feature map =
    if pred then FeatMap.update feature update map else map

  let check_if_larger_than_const op e1 e2 =
    if
      (String.equal op ">" || String.equal op ">=")
      && Infer.Feature.is_const e2
      || (String.equal op "<" || String.equal op "<=")
         && Infer.Feature.is_const e1
    then true
    else false

  let check_if_smaller_than_const op e1 e2 =
    if
      (String.equal op "<" || String.equal op "<=")
      && Infer.Feature.is_const e2
      || (String.equal op ">" || String.equal op ">=")
         && Infer.Feature.is_const e1
    then true
    else false

  let check_if_equal_to_var op e1 e2 =
    if
      String.equal op "==" && Infer.Feature.is_var e1 && Infer.Feature.is_var e2
    then true
    else false

  let check_if_not_equal_to_var op e1 e2 =
    if
      String.equal op "!=" && Infer.Feature.is_var e1 && Infer.Feature.is_var e2
    then true
    else false

  let check_if_equal_to_percentage op e1 e2 =
    if
      String.equal op "=="
      && ( (Infer.Feature.is_var e1 && Infer.Feature.is_const_int 37 e2)
         || (Infer.Feature.is_var e2 && Infer.Feature.is_const_int 37 e1) )
    then true
    else false

  let rec of_prune e map =
    match e with
    | IFeat.BinOp (op, e1, e2) when List.mem op rel_op ->
        FeatMap.update (Prune (op, true)) update map
        |> update_map_by_pred
             (check_if_larger_than_const op e1 e2)
             IfLargerThanConst
        |> update_map_by_pred
             (check_if_smaller_than_const op e1 e2)
             IfSmallerThanConst
        |> update_map_by_pred (check_if_equal_to_var op e1 e2) IfEqualToVar
        |> update_map_by_pred
             (check_if_not_equal_to_var op e1 e2)
             IfNotEqualToVar
        |> update_map_by_pred
             (check_if_equal_to_percentage op e1 e2)
             IfEqualToPercentage
        |> of_prune e1 |> of_prune e2
    | IFeat.UnOp (op, e) ->
        FeatMap.update (Prune (op, true)) update map |> of_prune e
    | _ -> map

  let of_instr step map =
    match step.Infer.Step.feature with
    | IFeat.Input _ -> FeatMap.update (Input "input") update map
    | IFeat.Store (_, e) -> of_exp e map
    | IFeat.Prune e -> of_prune e map
    | IFeat.LibraryCall s -> FeatMap.update (LibraryCall s) update map
    | IFeat.IntOverflow (_, e) ->
        FeatMap.update IntOverflow update map |> of_exp e
    | IFeat.IntUnderflow _ -> FeatMap.update IntUnderflow update map
    | IFeat.FormatString _ -> FeatMap.update FormatString update map
    | IFeat.CmdInjection _ -> FeatMap.update CmdInjection update map
    | IFeat.BufferOverflow _ -> FeatMap.update BufferOverflow update map
    | _ -> map

  let of_trace tr =
    List.fold_left (fun map step -> of_instr step map) FeatMap.empty tr

  let empty_vec = FeatMap.empty

  let norm v =
    FeatMap.fold (fun _ n acc -> (n * n) + acc) v 0
    |> float_of_int |> Float.sqrt

  let type_of v =
    let filtered_map =
      v
      |> FeatMap.filter (fun feat _ ->
             match feat with
             | Feature.IntOverflow | IntUnderflow | CmdInjection | FormatString
             | BufferOverflow ->
                 true
             | _ -> false)
    in
    (* Empty trace is possible *)
    assert (FeatMap.cardinal filtered_map <= 1);
    try filtered_map |> FeatMap.choose |> fst with Not_found -> Unknown

  let similarity v1 v2 =
    let v1_typ = v1 |> type_of in
    let v2_typ = v2 |> type_of in
    if v1_typ = v2_typ then
      let merged =
        FeatMap.merge
          (fun feat n1 n2 ->
            match (feat, n1, n2) with
            | Unknown, _, _ -> Some 0
            | _, Some c1, Some c2 -> Some (c1 * c2)
            | _, Some _, None | _, None, Some _ -> Some 0
            | _ -> None)
          v1 v2
      in
      let unnormalized =
        FeatMap.fold (fun _ v acc -> v + acc) merged 0 |> float_of_int
      in
      let nv1 = v1 |> norm in
      let nv2 = v2 |> norm in
      unnormalized /. (nv1 *. nv2)
    else 0.0
end
