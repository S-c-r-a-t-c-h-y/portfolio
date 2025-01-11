open Expr
open Func

let simplify_plus (l : Expr.t list) : Expr.t =
  let rec factorize_constants l =
    match l with
    | [] -> ([], 0.)
    | x :: xs -> (
        let l', acc = factorize_constants xs in
        match x with
        | Float i -> (l', acc +. i)
        | _ -> (x :: l', acc))
  in
  let l', acc = factorize_constants l in
  let l = if acc <> 0. then Float acc :: l' else l' in
  match l with
  | [] -> failwith "invalid expression"
  | [ x ] -> x
  | _ -> Plus l

let simplify_times (l : Expr.t list) : Expr.t =
  if List.exists (fun x -> x = Float 0.) l then Float 0.
  else
    let rec factorize_constants l =
      match l with
      | [] -> ([], 1.)
      | x :: xs -> (
          let l', acc = factorize_constants xs in
          match x with
          | Float i -> (l', acc *. i)
          | _ -> (x :: l', acc))
    in
    let l', acc = factorize_constants l in
    let l = if acc <> 1. then Float acc :: l' else l' in
    match l with
    | [] -> failwith "invalid expression"
    | [ x ] -> x
    | _ -> Times l

let rec simplify (e : Expr.t) =
  match e with
  | Float i -> e
  | Var i -> e
  | Plus l -> simplify_plus (List.map simplify l)
  | Times l -> simplify_times (List.map simplify l)
  | Frac (e1, e2) -> Frac (simplify e1, simplify e2)
  | Power (e1, e2) -> Power (simplify e1, simplify e2)
  | Func (f, e2) -> Func (f, simplify e2)
