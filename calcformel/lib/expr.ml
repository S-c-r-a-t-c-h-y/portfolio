open Func
open Constants

type expr =
  | Nb of float
  | Var of string
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

(* Returns the height of the expression as represented by a tree *)
let rec nested_level (e : expr) =
  match e with
  | Nb _ | Var _ | Cst _ -> 0
  | Plus l | Times l ->
      1 + List.fold_left (fun acc x -> max acc (nested_level x)) 0 l
  | Frac (e1, e2) | Pow (e1, e2) -> 1 + max (nested_level e1) (nested_level e2)
  | Func (_, e) -> 1 + nested_level e

(* [exists2 f l] checks if at least two distinct elements of the list [l] that satisfies the predicate [f] *)
let rec exists2 (f : 'a -> 'a -> bool) (l : 'a list) =
  match l with
  | [] | [ _ ] -> false
  | x :: xs ->
      List.exists (fun y -> f x y) xs
      || List.exists (fun y -> f y x) xs
      || exists2 f xs

(* [find2_opt f l] returns the first two distinct elements of the list [l] that satisfies the predicate [f]. *)
(* Returns [None] if there is no value that satisfies [f] in the list [l]. *)
let rec find2_opt (f : 'a -> 'a -> bool) (l : 'a list) =
  match l with
  | [] | [ _ ] -> None
  | x :: xs -> (
      match List.find_opt (fun y -> f x y) xs with
      | Some y -> Some (x, y)
      | None -> (
          match List.find_opt (fun y -> f y x) xs with
          | Some y -> Some (y, x)
          | None -> find2_opt f xs))

(* [remove e l] removes the first instance of [e] in the list [l]. *)
(* Does nothing if [e] is not in the list [l]. *)
let rec remove (e : 'a) (l : 'a list) =
  match l with
  | [] -> []
  | x :: xs -> if x = e then xs else x :: remove e xs

(**
Remark : extensive use of pattern matching cannot be done due to the need to simplify inner expressions first
Pattern simplifications done :
- x + 0 = 0 + x = 0
- 1 * x = x * 1 = x
- 0 * x = x * 0 = 0
- 0 / x = 0
- x / x = 1
- y * 1/x = 1/x * y = y/x
- x ^ 1 = x
- x / 0 -> raise Division_by_zero
- nb i / nb j = nb (i / j)
- (nb i)^(nb j) = nb (i^j)
- x^p * x^q = x^(p+q) TODO
- nb i * x + nb j * x = nb (i+j) * y
- exp(ln(x)) = ln(exp(x)) = x
- exp(y*ln(x)) = exp(ln(x)*y) = x^y
- exp(x) * exp(y) = exp(x + y)
- exp(1) = e
- exp(0) = 1
- ln(e) = 1
- ln(1) = 0
- cos^2(x) + sin^2(x) = 1
Commuting simplifications
*)
let rec simplify (e : expr) =
  (* returns the numerator and the denominator of the expression if fractions are present *)
  let rec simplify_frac (l : expr list) =
    match l with
    | [] -> ([], [])
    | Frac (x, y) :: xs ->
        let num, den = simplify_frac xs in
        (x :: num, y :: den)
    | x :: xs ->
        let num, den = simplify_frac xs in
        (x :: num, den)
  in

  (* simplification of Times statement *)
  let rec simplify_times (l : expr list) =
    let l' = List.map simplify l in
    if List.exists (fun x -> x = Nb 0.) l' then [ Nb 0. ] (* x * 0 = 0 *)
    else
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
      let l' =
        List.stable_sort (fun x y -> nested_level x - nested_level y) l'
      in
      (* merge duplicates *)
      let rec count_and_delete (l : expr list) (e : expr) =
        match l with
        | [] -> (0., [])
        | (Pow (x, Nb i) as y) :: xs ->
            let cnt, new_l = count_and_delete xs e in
            if equal x e then (cnt +. i, new_l)
            else if equal y e then (cnt +. 1., new_l)
            else (cnt, y :: new_l)
        | x :: xs ->
            let cnt, new_l = count_and_delete xs e in
            if equal x e then (cnt +. 1., new_l) else (cnt, x :: new_l)
      in
      let rec factor_duplicates (l : expr list) =
        match l with
        | [] -> []
        | Pow (x, Nb i) :: _ ->
            let cnt, l = count_and_delete l x in
            if cnt = 1. then x :: factor_duplicates l
            else (Pow (x, Nb cnt) |> simplify) :: factor_duplicates l
        | x :: _ ->
            let cnt, l = count_and_delete l x in
            if cnt = 1. then x :: factor_duplicates l
            else (Pow (x, Nb cnt) |> simplify) :: factor_duplicates l
      in
      let l' = factor_duplicates l' in
      l'
  in
  let simplify_times_pattern (l : expr list) =
    (* exp(x) * exp(y) = exp(x + y) *)
    let l =
      match
        find2_opt
          (fun x y ->
            match (x, y) with
            | Func (Exp, _), Func (Exp, _) -> true
            | _ -> false)
          l
      with
      | Some ((Func (Exp, x) as e1), (Func (Exp, y) as e2)) ->
          let l' = remove e1 (remove e2 l) in
          Func (Exp, Plus [ x; y ]) :: l' |> simplify_times
      | _ -> l
    in
    (* simplification of fractions *)
    match simplify_frac l with
    | l, [] -> Times l
    | num, den -> Frac (Times num, Times den) |> simplify
  in

  (* simplification of Plus statement *)
  let rec simplify_plus (l : expr list) =
    let l' = List.map simplify l in
    (* merging nested Plus *)
    let rec merge_plus (l : expr list) =
      match l with
      | [] -> (false, [])
      | Plus l' :: xs ->
          let _, new_l = merge_plus xs in
          (true, l' @ new_l)
      | x :: xs ->
          let b, xs' = merge_plus xs in
          (b, x :: xs')
    in
    let b, l' = merge_plus l' in
    let l' = if b then simplify_plus l' else l' in
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
    let l' = List.stable_sort (fun x y -> nested_level x - nested_level y) l' in
    (* merge duplicates *)
    let rec count_and_delete (l : expr list) (e : expr) =
      match l with
      | [] -> (0., [])
      | (Times [ Nb i; x ] as y) :: xs ->
          let cnt, new_l = count_and_delete xs e in
          if equal x e then (cnt +. i, new_l)
          else if equal y e then (cnt +. 1., new_l)
          else (cnt, y :: new_l)
      | x :: xs ->
          let cnt, new_l = count_and_delete xs e in
          if equal x e then (cnt +. 1., new_l) else (cnt, x :: new_l)
    in
    let rec factor_duplicates (l : expr list) =
      match l with
      | [] -> []
      | Times [ Nb i; x ] :: _ ->
          let cnt, new_l = count_and_delete l x in
          if cnt = 1. then x :: factor_duplicates new_l
          else (Times [ Nb cnt; x ] |> simplify) :: factor_duplicates new_l
      | x :: _ ->
          let cnt, new_l = count_and_delete l x in
          if cnt = 1. then x :: factor_duplicates new_l
          else (Times [ Nb cnt; x ] |> simplify) :: factor_duplicates new_l
    in
    let l' = factor_duplicates l' in
    l'
  in
  let simplify_plus_pattern (l : expr list) =
    (* cos^2(x) + sin^2(x) = 1 *)
    let l =
      match
        find2_opt
          (fun x y ->
            match (x, y) with
            | Pow (Func (Cos, e1), Nb 2.), Pow (Func (Sin, e2), Nb 2.)
              when equal e1 e2 -> true
            | _ -> false)
          l
      with
      | Some (e1, e2) ->
          let l' = remove e1 (remove e2 l) in
          Nb 1. :: l' |> simplify_plus
      | _ -> l
    in
    Plus l
  in

  (* main simplification *)
  let pattern_simplify (e : expr) =
    match e with
    | Plus l -> (
        match simplify_plus l with
        | [ x ] -> x
        | l' -> simplify_plus_pattern l')
    | Times l -> (
        match simplify_times l with
        | [ x ] -> x
        | l' -> simplify_times_pattern l')
    | Frac (e1, e2) -> (
        match (simplify e1, simplify e2) with
        (* 0 / x = 0 *)
        | Nb 0., _ -> Nb 0.
        (* x / 0 *)
        | _, Nb 0. -> raise Division_by_zero
        (* nb i / nb j = nb (i / j) *)
        | Nb i, Nb j -> Nb (i /. j)
        (* fraction simplification *)
        | Times [ Nb i; e1' ], Times [ Nb j; e2' ] ->
            Times [ Nb (i /. j); Frac (e1', e2') ] |> simplify
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
        | Ln -> (
            match simplify e with
            (* ln(exp(x)) = x *)
            | Func (Exp, e') -> e'
            (* ln(e) = 1 *)
            | Cst E -> Nb 1.
            (* ln(1) = 0 *)
            | Nb 1. -> Nb 0.
            | e' -> Func (Ln, e'))
        | Exp -> (
            match simplify e with
            (* exp(ln(x)) = x *)
            | Func (Ln, e') -> e'
            (* exp(y*ln(x)) = exp(ln(x)*y) = x^y *)
            | Times [ e1'; e2' ] as e' -> (
                match (e1', e2') with
                | y, Func (Ln, x) | Func (Ln, x), y -> Pow (x, y)
                | _ -> Func (Exp, e'))
            (* exp(1) = e *)
            | Nb 1. -> Cst E
            (* exp(0) = 1 *)
            | Nb 0. -> Nb 1.
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
  match (simplify e1, simplify e2) with
  | Nb i, Nb j -> i = j
  | Var i, Var j -> i = j
  | Cst c1, Cst c2 -> c1 = c2
  | Plus l1, Plus l2 | Times l1, Times l2 -> equal_list l1 l2
  | Frac (x1, y1), Frac (x2, y2) -> equal x1 x2 && equal y1 y2
  | Pow (x1, y1), Pow (x2, y2) -> equal x1 x2 && equal y1 y2
  | Func (f1, e1), Func (f2, e2) -> f1 = f2 && equal e1 e2
  | _ -> false
