open Expr

type func = {
  str : string;
  func : float -> float;
  latex : string;
  derivative : Expr.t -> Expr.t;
}

let func_list =
  [
    {
      str = "cos";
      func = cos;
      latex = "\\cos";
      derivative = (fun e -> Func ("sin", e) |> neg);
    };
    {
      str = "sin";
      func = sin;
      latex = "\\sin";
      derivative = (fun e -> Func ("cos", e));
    };
    {
      str = "exp";
      func = exp;
      latex = "\\exp";
      derivative = (fun e -> Func ("exp", e));
    };
    {
      str = "ln";
      func = log;
      latex = "\\ln";
      derivative = (fun e -> Frac (Float 1., e));
    };
    {
      str = "tan";
      func = tan;
      latex = "\\tan";
      derivative = (fun e -> Plus [ Float 1.; Func ("tan", e) |> square ]);
    };
    {
      str = "asin";
      func = asin;
      latex = "\\asin";
      derivative =
        (fun e ->
          Frac
            (Float 1., Power (Plus [ Float 1.; e |> square |> neg ], Float 0.5)));
    };
    {
      str = "acos";
      func = acos;
      latex = "\\acos";
      derivative =
        (fun e ->
          Frac
            (Float 1., Power (Plus [ Float 1.; e |> square |> neg ], Float 0.5))
          |> neg);
    };
    {
      str = "atan";
      func = atan;
      latex = "\\atan";
      derivative = (fun e -> Frac (Float 1., Plus [ Float 1.; e |> square ]));
    };
    {
      str = "sinh";
      func = sinh;
      latex = "\\sinh";
      derivative = (fun e -> Func ("cosh", e));
    };
    {
      str = "cosh";
      func = cosh;
      latex = "\\cosh";
      derivative = (fun e -> Func ("sinh", e));
    };
    {
      str = "tanh";
      func = tanh;
      latex = "\\tanh";
      derivative =
        (fun e -> Plus [ Float 1.; Func ("tanh", e) |> square |> neg ]);
    };
  ]

let is_function s = s = "sqrt" || List.exists (fun f -> f.str = s) func_list
let func_of_string s = List.find (fun f -> f.str = s) func_list
