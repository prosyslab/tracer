type t = { file : string; line : int }

let equal { file = f1; line = l1 } { file = f2; line = l2 } =
  String.equal f1 f2 && Int.equal l1 l2

let get_line t = t.line