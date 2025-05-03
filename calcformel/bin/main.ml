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
  (* parse "x(2/(3*y))" |> simplify |> print_expr; *)
  Printf.printf "parse \"exp(x)*exp(y)\" |> simplify : ";
  parse "exp(x)*exp(y)" |> simplify |> print_expr;
  (* parse "xyz" |> simplify |> print_expr ~implicit_mul:true *)
  parse "(1+2*exp(x+1))/x" |> simplify |> print_expr ~implicit_mul:true;
  parse "(1+2*exp(x+1)*ln(1))/x" |> print_expr ~implicit_mul:true;
  parse "(1+2*exp(x+1)*ln(1))/x" |> simplify |> print_expr ~implicit_mul:true;
  parse "cos(x)^2 + 1 + sin(x)^2 + cos(x)^2 + 1 + sin(x)^2"
  |> simplify
  |> print_expr ~implicit_mul:true;
  parse "x^2*x^3" |> simplify |> print_expr ~implicit_mul:true;
  parse "2*x+3*x" |> simplify |> print_expr ~implicit_mul:true;
  parse "(2*x)/(4*y)" |> simplify |> print_expr ~implicit_mul:true
