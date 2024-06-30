open Expr
open Func
open Constants

type valuation = float array
(** A valuation is an array [v] where [v.(i)] is the value assigned to the variable [i] *)

let rec eval (e : expr) (valuation : valuation) =
  match e with
  | Nb i -> i
  | Var i -> valuation.(i)
  | Cst c -> float_of_cst c
  | Plus l -> List.fold_left (fun acc x -> acc +. eval x valuation) 0. l
  | Times l -> List.fold_left (fun acc x -> acc *. eval x valuation) 1. l
  | Frac (e1, e2) -> eval e1 valuation /. eval e2 valuation
  | Pow (e1, e2) -> eval e1 valuation ** eval e2 valuation
  | Func (f, e) -> eval e valuation |> func_to_fun f

(* let rec expr_to_fun (e : expr) =
   match e with
   | Nb i -> fun _ -> i
   | Var 0 -> fun x -> x
   | Var _ -> failwith "not implemented"
   | Plus (e1, e2) -> fun x -> expr_to_fun e1 x +. expr_to_fun e2 x
   | Times (e1, e2) -> fun x -> expr_to_fun e1 x *. expr_to_fun e2 x
   | Frac (e1, e2) -> fun x -> expr_to_fun e1 x /. expr_to_fun e2 x
   | Pow (e1, e2) -> fun x -> expr_to_fun e1 x ** expr_to_fun e2 x
   | Func (f, e) -> fun x -> func_to_fun f (expr_to_fun e x) *)
