open Expr
open Func

let derive (var : string) (e : Expr.t) =
  let rec derive_product (l : Expr.t list) =
    match l with
    | [] -> []
    | u :: xs ->
        [ aux u :: xs ] @ List.map (fun x -> u :: x) (derive_product xs)
  and aux e =
    match e with
    | Float _ -> Float 0.
    | Var i when i <> var -> Float 0.
    | Var _ -> Float 1.
    (* (u+v)' = u' + v' *)
    | Plus l -> Plus (List.map aux l)
    (* (uv)' = u'v + uv' *)
    | Times l -> Plus (List.map (fun x -> Times x) (derive_product l))
    (* (u/v)' = (u'v - uv')/(v^2) *)
    | Frac (e1, e2) ->
        Frac
          ( Plus [ Times [ aux e1; e2 ]; Times [ e1; aux e2 ] |> neg ],
            Times [ e2; e2 ] )
    | Power (e1, e2) -> aux (Func ("exp", Times [ e2; Func ("ln", e1) ]))
    | Func (f, e) -> Times [ aux e; (func_of_string f).derivative e ]
  in
  e |> aux

let rec derive_n (var : string) (e : Expr.t) ~(n : int) =
  if n = 0 then e else derive_n var (derive var e) ~n:(n - 1)
