open Func
open Constants

type expr =
  | Nb of float
  | Var of int
  | Cst of constant
  | Plus of expr list
  | Times of expr list
  (* Frac (numerator, denominator) *)
  | Frac of expr * expr
  (* Pow (expr, exponent) *)
  | Pow of expr * expr
  | Func of func * expr

let neg (e : expr) = Times [ Nb (-1.); e ]
let square (e : expr) = Pow (e, Nb 2.)

let rec nested_level (e : expr) =
  match e with
  | Nb _ | Var _ | Cst _ -> 0
  | Plus l | Times l ->
      1 + List.fold_left (fun acc x -> max acc (nested_level x)) 0 l
  | Frac (e1, e2) | Pow (e1, e2) -> 1 + max (nested_level e1) (nested_level e2)
  | Func (_, e) -> 1 + nested_level e

(**
Remark : extensive use of pattern matching cannot be done due to the need to simplify inner expressions first
Pattern simplifications done :
- x + 0 = 0 + x = 0
- 1 * x = x * 1 = x
- 0 * x = x * 0 = 0
- 0 / x = 0
- x / x = 1
- y * 1/x = 1/x * y = y/x TODO
- x ^ 1 = x
- x / 0 -> raise Division_by_zero
- nb i / nb j = nb (i / j)
- (nb i)^(nb j) = nb (i^j)
- x^p * x^q = x^(p+q) TODO
- exp(ln(x)) = ln(exp(x)) = x
- exp(y*ln(x)) = exp(ln(x)*y) = x^y
- exp(x) * exp(y) = exp(x + y) TODO
- cos^2(x) + sin^2(x) = 1 TODO
Commuting simplifications
*)
let rec simplify (e : expr) =
  (* simplification of Times statement *)
  let rec simplify_times (l : expr list) =
    let l' = List.map simplify l in
    if List.exists (fun x -> x = Nb 0.) l' then [ Nb 0. ] (* x * 0 = 0 *)
    else
      (* multiply all numbers together *)
      (* after simplifications, numerical factors are first in the list *)
      let rec compute_factor (l : expr list) =
        match l with
        | [] -> (1., [])
        | Nb i :: xs ->
            let fac, xs' = compute_factor xs in
            (fac *. i, xs')
        | x :: xs ->
            let fac, xs' = compute_factor xs in
            (fac, x :: xs')
      in
      let fac, l' = compute_factor l' in
      let l' = if fac = 1. then l' else Nb fac :: l' in
      (* merging nested Times *)
      let rec merge_times (l : expr list) =
        match l with
        | [] -> (false, [])
        | Times l' :: xs ->
            let _, l = merge_times xs in
            (true, l' @ l)
        | x :: xs ->
            let b, xs' = merge_times xs in
            (b, x :: xs')
      in
      let b, l' = merge_times l' in
      let l' = if b then simplify_times l' else l' in
      let l' =
        List.stable_sort (fun x y -> nested_level x - nested_level y) l'
      in
      l'
  in

  (* simplification of Plus statement *)
  let rec simplify_plus (l : expr list) =
    let l' = List.map simplify l in
    (* sum all numbers together *)
    (* after simplifications, numerical factors are first in the list *)
    let rec compute_factor (l : expr list) =
      match l with
      | [] -> (0., [])
      | Nb i :: xs ->
          let fac, xs' = compute_factor xs in
          (fac +. i, xs')
      | x :: xs ->
          let fac, xs' = compute_factor xs in
          (fac, x :: xs')
    in
    let fac, l' = compute_factor l' in
    let l' = if fac = 0. then l' else Nb fac :: l' in
    (* merging nested Plus *)
    let rec merge_plus (l : expr list) =
      match l with
      | [] -> (false, [])
      | Plus l' :: xs ->
          let _, l = merge_plus xs in
          (true, l' @ l)
      | x :: xs ->
          let b, xs' = merge_plus xs in
          (b, x :: xs')
    in
    let b, l' = merge_plus l' in
    let l' = if b then simplify_plus l' else l' in
    let l' = List.stable_sort (fun x y -> nested_level x - nested_level y) l' in
    (* merge duplicates *)
    let rec count_and_delete (l : expr list) (e : expr) =
      match l with
      | [] -> (0., [])
      | (Times [ Nb i; x ] as y) :: xs ->
          let cnt, l = count_and_delete xs e in
          if equal x e then (cnt +. i, l) else (cnt, y :: l)
      | x :: xs ->
          let cnt, l = count_and_delete xs e in
          if equal x e then (cnt +. 1., l) else (cnt, x :: l)
    in
    let rec factor_duplicates (l : expr list) =
      (* Printf.printf "%d\n" (List.length l); *)
      match l with
      | [] -> []
      | x :: _ ->
          let cnt, l = count_and_delete l x in
          if cnt = 1. then x :: factor_duplicates l
          else Times [ Nb cnt; x ] :: factor_duplicates l
    in
    (* let l' = factor_duplicates l' in *)
    l'
  in

  (* main simplification *)
  let pattern_simplify (e : expr) =
    match e with
    | Plus l -> (
        match simplify_plus l with
        | [ x ] -> x
        | l' -> Plus l')
    | Times l -> (
        match simplify_times l with
        | [ x ] -> x
        | l' ->
            Times l'
            (* exp(x) * exp(y) = exp(x + y)
                  | Func (Exp, x), Func (Exp, y) -> Func (Exp, Plus (x, y)) |> simplify
                  (* x * y/z = y/z * x = (x*y)/z *)
                  | x, Frac (y, z) | Frac (y, z), x -> Frac (Times (x, y), z) |> simplify
                  (* x * x = x^2 *)
                  | e1', e2' ->
                      if equal e1' e2' then Pow (e1', Nb 2.) else Times (e1', e2')) *)
        )
    | Frac (e1, e2) -> (
        match (simplify e1, simplify e2) with
        (* 0 / x = 0 *)
        | Nb 0., _ -> Nb 0.
        (* x / 0 *)
        | _, Nb 0. -> raise Division_by_zero
        (* nb i / nb j = nb (i / j) *)
        | Nb i, Nb j -> Nb (i /. j)
        (* fraction simplification *)
        (* | Times (Nb i, e1'), Times (Nb j, e2') ->
            Times (Nb (i /. j), Frac (e1', e2')) |> simplify *)
        | e1', e2' -> if equal e1' e2' then Nb 1. else Frac (e1', e2'))
    | Pow (e1, e2) -> (
        match (simplify e1, simplify e2) with
        (* (x^y)^z = x^(y*z) *)
        | Pow (x, y), z -> Pow (x, Times [ y; z ]) |> simplify
        (* x^1 = x *)
        | e1', Nb 1. -> e1'
        (* (nb i)^(nb j) = nb (i^j) *)
        | Nb i, Nb j -> Nb (i ** j)
        | e1', e2' -> Pow (e1', e2'))
    | Func (f, e) -> (
        match f with
        (* ln(exp(x)) = x *)
        | Ln -> (
            match simplify e with
            | Func (Exp, e') -> e'
            | e' -> Func (Ln, e'))
        | Exp -> (
            match simplify e with
            (* exp(ln(x)) = x *)
            | Func (Ln, e') -> e'
            (* exp(y*ln(x)) = exp(ln(x)*y) = x^y *)
            (* | Times (e1', e2') as e' -> (
                match (e1', e2') with
                | y, Func (Ln, x) | Func (Ln, x), y -> Pow (x, y)
                | _ -> Func (Exp, e')) *)
            | e' -> Func (Exp, e'))
        | _ -> Func (f, simplify e))
    | e -> e
  in
  pattern_simplify e

and equal (e1 : expr) (e2 : expr) =
  let rec find_and_remove (l : expr list) (e : expr) =
    match l with
    | [] -> (false, l)
    | x :: xs ->
        if equal x e then (true, xs)
        else
          let b, xs' = find_and_remove xs e in
          (b, x :: xs')
  in
  let rec equal_list (l1 : expr list) (l2 : expr list) =
    match l1 with
    | [] -> l2 = []
    | x :: xs ->
        let b, l2' = find_and_remove l2 x in
        b && equal_list xs l2'
  in
  nested_level e1 = nested_level e2
  &&
  match (simplify e1, simplify e2) with
  | Nb i, Nb j -> i = j
  | Var i, Var j -> i = j
  | Cst c1, Cst c2 -> c1 = c2
  | Plus l1, Plus l2 | Times l1, Times l2 -> equal_list l1 l2
  | Frac (x1, y1), Frac (x2, y2) -> equal x1 x2 && equal y1 y2
  | Pow (x1, y1), Pow (x2, y2) -> equal x1 x2 && equal y1 y2
  | Func (f1, e1), Func (f2, e2) -> f1 = f2 && equal e1 e2
  | _ -> false
