open Expr
open Func
open Constants

let nb_to_string (i : float) =
  let s = string_of_float i in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let rec string_of_expr ?(implicit_mul : bool = false) (e : expr) =
  let rec print_list (l : expr list) (sep : string) =
    match l with
    | [] -> ""
    | [ x ] -> aux x false
    | x :: (Times [ Nb -1.; _ ] as y) :: xs when sep = "+" ->
        aux x false ^ print_list (y :: xs) sep
    | x :: xs -> aux x false ^ sep ^ print_list xs sep
  and aux e encapsulate =
    match e with
    | Nb i -> nb_to_string i
    | Cst c -> string_of_cst c
    | Var i -> i
    | Plus l ->
        if encapsulate then "(" ^ print_list l "+" ^ ")" else print_list l "+"
    | Times l ->
        if encapsulate then
          "("
          ^ (if implicit_mul then print_list l "" else print_list l "*")
          ^ ")"
        else if implicit_mul then print_list l ""
        else print_list l "*"
    | Frac (e1, e2) ->
        if encapsulate then "(" ^ aux e1 true ^ "/" ^ aux e2 true ^ ")"
        else aux e1 true ^ "/" ^ aux e2 true
    | Pow (e1, e2) -> "(" ^ aux e1 true ^ "^" ^ aux e2 true ^ ")"
    | Func (f, e2) -> string_of_func f ^ "(" ^ aux e2 false ^ ")"
  in
  aux e false

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

let print_expr ?(implicit_mul : bool = false) (e : expr) =
  e |> string_of_expr ~implicit_mul |> print_endline
