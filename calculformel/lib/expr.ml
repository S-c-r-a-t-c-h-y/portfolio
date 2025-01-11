type t =
  | Float of float
  | Var of string
  | Func of (string * t)
  | Plus of t list
  | Times of t list
  | Frac of (t * t)
  | Power of (t * t)

let neg e = Times [ Float (-1.); e ]
let square e = Times [ e; e ]

(* makes sure there are no Plus or Times list nested *)
let rec normalize (e : t) =
  match e with
  | Plus l ->
      let l = List.map normalize l in
      Plus
        (List.fold_right
           (fun x acc ->
             match x with
             | Plus l' -> l' @ acc
             | _ -> x :: acc)
           l [])
  | Times l ->
      let l = List.map normalize l in
      Times
        (List.fold_right
           (fun x acc ->
             match x with
             | Times l' -> l' @ acc
             | _ -> x :: acc)
           l [])
  | Func (ident, e2) -> Func (ident, normalize e2)
  | Frac (e1, e2) -> Frac (normalize e1, normalize e2)
  | Power (e1, e2) -> Power (normalize e1, normalize e2)
  | _ -> e
