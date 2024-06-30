type constant = Pi | E

let float_of_cst (c : constant) =
  match c with
  | Pi -> acos (-1.)
  | E -> exp 1.

let string_of_cst (c : constant) =
  match c with
  | Pi -> "pi"
  | E -> "e"
