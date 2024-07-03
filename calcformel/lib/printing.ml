open Expr
open Func
open Constants

let nb_to_string (i : float) =
  let s = string_of_float i in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let rec string_of_expr (e : expr) =
  let rec print_list (l : expr list) (sep : string) =
    match l with
    | [] -> ""
    | [ x ] -> string_of_expr x
    | x :: (Times [ Nb -1.; _ ] as y) :: xs when sep = "+" ->
        string_of_expr x ^ print_list (y :: xs) sep
    | x :: xs -> string_of_expr x ^ sep ^ print_list xs sep
  in
  match e with
  | Nb i -> nb_to_string i
  | Cst c -> string_of_cst c
  | Var i -> i
  | Plus l -> "(" ^ print_list l "+" ^ ")"
  | Times l -> "(" ^ print_list l "*" ^ ")"
  | Frac (e1, e2) -> "(" ^ string_of_expr e1 ^ "/" ^ string_of_expr e2 ^ ")"
  | Pow (e1, e2) -> "(" ^ string_of_expr e1 ^ "^" ^ string_of_expr e2 ^ ")"
  | Func (f, e2) -> string_of_func f ^ "(" ^ string_of_expr e2 ^ ")"

(* let rec latex_of_expr ?(variable_format : string = "v") (e : expr) =
   match e with
   | Nb i -> nb_to_string i
   | Var i -> variable_format ^ string_of_int i
   | Plus (e1, e2) ->
       "("
       ^ latex_of_expr e1 ~variable_format
       ^ "+"
       ^ latex_of_expr e2 ~variable_format
       ^ ")"
   | Times (e1, e2) ->
       "("
       ^ latex_of_expr e1 ~variable_format
       ^ "\\times"
       ^ latex_of_expr e2 ~variable_format
       ^ ")"
   | Frac (e1, e2) ->
       "\\dfrac{"
       ^ latex_of_expr e1 ~variable_format
       ^ "}{"
       ^ latex_of_expr e2 ~variable_format
       ^ "}"
   | Pow (e1, e2) ->
       "{"
       ^ latex_of_expr e1 ~variable_format
       ^ "}^{"
       ^ latex_of_expr e2 ~variable_format
       ^ "}"
   | Func (f, e2) ->
       latex_of_func f ^ "{" ^ latex_of_expr e2 ~variable_format ^ "}" *)

let print_expr (e : expr) = e |> string_of_expr |> print_endline
