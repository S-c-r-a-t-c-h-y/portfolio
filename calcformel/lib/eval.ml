open Expr
open Func
open Constants

type valuation = (string, float) Hashtbl.t
(** A valuation is an hashtable [v] storing the value for each individual variable present in the expression *)

let count_var (e : expr) =
  let tbl = Hashtbl.create 100 in
  let res = ref 0 in
  let rec count_aux e =
    match e with
    | Var s -> (
        match Hashtbl.find_opt tbl s with
        | None ->
            incr res;
            Hashtbl.add tbl s 0
        | Some _ -> ())
    | Plus l | Times l -> List.iter count_aux l
    | Frac (e1, e2) | Pow (e1, e2) ->
        count_aux e1;
        count_aux e2
    | Func (f, e) -> count_aux e
    | _ -> ()
  in
  count_aux e;
  !res

let rec eval (e : expr) (valuation : valuation) =
  match e with
  | Nb i -> i
  | Var s -> (
      match Hashtbl.find_opt valuation s with
      | None -> raise (Invalid_argument "eval : valuation incomplete")
      | Some f -> f)
  | Cst c -> float_of_cst c
  | Plus l -> List.fold_left (fun acc x -> acc +. eval x valuation) 0. l
  | Times l -> List.fold_left (fun acc x -> acc *. eval x valuation) 1. l
  | Frac (e1, e2) -> eval e1 valuation /. eval e2 valuation
  | Pow (e1, e2) -> eval e1 valuation ** eval e2 valuation
  | Func (f, e) -> eval e valuation |> func_to_fun f

let rec expr_to_fun (e : expr) =
  if count_var e > 1 then
    raise (Invalid_argument "Eval.expr_to_fun : more than one variable");
  match e with
  | Nb i -> fun _ -> i
  | Var _ -> fun x -> x
  | Cst c -> fun _ -> float_of_cst c
  | Plus l -> fun x -> List.fold_left (fun acc y -> acc +. expr_to_fun y x) 0. l
  | Times l ->
      fun x -> List.fold_left (fun acc y -> acc *. expr_to_fun y x) 1. l
  | Frac (e1, e2) -> fun x -> expr_to_fun e1 x /. expr_to_fun e2 x
  | Pow (e1, e2) -> fun x -> expr_to_fun e1 x ** expr_to_fun e2 x
  | Func (f, e) -> fun x -> func_to_fun f (expr_to_fun e x)
