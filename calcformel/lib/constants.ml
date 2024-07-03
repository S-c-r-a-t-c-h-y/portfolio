type constant = Pi | E

let float_of_cst (c : constant) =
  match c with
  | Pi -> acos (-1.)
  | E -> exp 1.

let string_of_cst (c : constant) =
  match c with
  | Pi -> "pi"
  | E -> "e"

let cst_of_string (s : string) =
  match s with
  | "pi" -> Pi
  | "e" -> E
  | _ -> raise (Invalid_argument "Constants.cst_of_string")

let cst_litteral = [ "pi"; "e" ]
