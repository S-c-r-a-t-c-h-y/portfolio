open Calculformel
open Expr
open Func
open Conversion
open Derivation
open Simplification

let _ =
  expr_of_string "1 + 2x(x+1)"
  |> derive "x" |> simplify |> string_of_expr |> print_string
