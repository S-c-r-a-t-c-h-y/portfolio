open Calcformel
open Func
open Expr
open Derivation
open Printing

(* let e1 = Times (Plus (Nb 1., Var 0), Plus (Var 0, Nb 4.))
   let e2 = Func (Exp, Times (Var 0, Var 0) |> neg) *)
let e3 = Func (Exp, Func (Ln, Var 0))
let e4 = Times [ Nb 2.; Frac (Nb 3., Var 0) ]
let e5 = Frac (Nb 1., Var 0)
let e6 = Plus [ Nb 2.; Times [ Var 0; Nb 3. ]; Plus [ Var 0; Nb 3. ] ]
let e7 = Frac (Plus [ Var 0; Nb 1. ], Plus [ Nb 1.; Var 0 ])
let e8 = Frac (Times [ Nb 3.; Var 0 ], Times [ Nb 2.; Var 0 ])

let () =
  e3 |> print_expr;
  e3 |> simplify |> print_expr;
  derive e3 0 |> simplify |> print_expr;
  e4 |> simplify |> print_expr;
  derive_n e5 0 ~n:3 |> print_expr;
  (* (if equal (Plus [ Nb 2.; Var 0; Nb 3. ]) (Plus [ Nb 2.; Nb 3.; Var 0 ]) then
        "true"
      else "false")
     |> print_endline; *)
  e6 |> print_expr;
  e6 |> simplify |> print_expr;
  e7 |> simplify |> print_expr;
  e8 |> simplify |> print_expr
