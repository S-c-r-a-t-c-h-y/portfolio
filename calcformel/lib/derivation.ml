open Func
open Expr

let derive_func (f : func) (e : expr) =
  match f with
  | Ln -> Frac (Nb 1., e)
  | Exp -> Func (Exp, e)
  | Sin -> Func (Cos, e)
  | Cos -> Func (Sin, e) |> neg
  | Tan -> Plus [ Nb 1.; Func (Tan, e) |> square ]
  | Asin -> Frac (Nb 1., Pow (Plus [ Nb 1.; e |> square |> neg ], Nb 0.5))
  | Acos ->
      Frac (Nb 1., Pow (Plus [ Nb 1.; e |> square |> neg ], Nb 0.5)) |> neg
  | Atan -> Frac (Nb 1., Plus [ Nb 1.; e |> square ])
  | Sinh -> Func (Cosh, e)
  | Cosh -> Func (Sinh, e)
  | Tanh -> Plus [ Nb 1.; Func (Tanh, e) |> square |> neg ]

let derive (e : expr) (var : string) =
  (* let rec derive_product (l : expr list) =
     match l with
     | [] | [ _ ] -> failwith "syntax error"
     | [ u; v ] -> [ [ aux u; v ]; [ u; aux v ] ]
     | u :: xs -> (aux u :: xs) :: List.map (fun x -> u :: x) (derive_product xs) *)
  let rec derive_product (l : expr list) =
    match l with
    | [] -> []
    | u :: xs ->
        [ aux u :: xs ] @ List.map (fun x -> u :: x) (derive_product xs)
  and aux e =
    match e with
    | Nb _ | Cst _ -> Nb 0.
    | Var i when i <> var -> Nb 0.
    | Var _ -> Nb 1.
    (* (u+v)' = u' + v' *)
    | Plus l -> Plus (List.map aux l)
    (* (uv)' = u'v + uv' *)
    | Times l -> Plus (List.map (fun x -> Times x) (derive_product l))
    (* (u/v)' = (u'v - uv')/(v^2) *)
    | Frac (e1, e2) ->
        Frac
          ( Plus [ Times [ aux e1; e2 ]; Times [ e1; aux e2 ] |> neg ],
            Times [ e2; e2 ] )
    | Pow (e1, e2) -> aux (Func (Exp, Times [ e2; Func (Ln, e1) ]))
    | Func (f, e) -> Times [ aux e; derive_func f e ]
  in
  e |> simplify |> aux |> simplify

let rec derive_n (e : expr) (var : string) ~(n : int) =
  if n = 0 then e else derive_n (derive e var) var ~n:(n - 1)
