open Calcformel
open Func
open Expr
open Derivation
open Printing
open Parsing

(* let () = derive_n (parse "1/x") "x" ~n:2 |> print_expr *)
let () =
  parse "(1+x)+(x+1)" |> print_expr;
  (* parse "2x" |> simplify |> print_expr; *)
  (* parse "(1+x)" |> simplify |> print_expr;
     parse "(x+1)" |> simplify |> print_expr; *)
  parse "(1+x)+(x+1)" |> simplify |> print_expr;
  parse "(2*x)+(x*2)" |> simplify |> print_expr;
  parse "x*(2/y)" |> simplify |> print_expr
