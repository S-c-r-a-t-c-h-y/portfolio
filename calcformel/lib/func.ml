type func =
  | Ln
  | Exp
  | Sin
  | Cos
  | Tan
  | Acos
  | Asin
  | Atan
  | Cosh
  | Sinh
  | Tanh

let func_to_fun (f : func) =
  match f with
  | Ln -> log
  | Exp -> exp
  | Cos -> cos
  | Sin -> sin
  | Tan -> tan
  | Acos -> acos
  | Asin -> asin
  | Atan -> atan
  | Cosh -> cosh
  | Sinh -> sinh
  | Tanh -> tanh

let string_of_func (f : func) =
  match f with
  | Ln -> "ln"
  | Exp -> "exp"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Asin -> "asin"
  | Acos -> "acos"
  | Atan -> "atan"
  | Sinh -> "sinh"
  | Cosh -> "cosh"
  | Tanh -> "tanh"

let func_of_string (s : string) =
  match s with
  | "ln" -> Ln
  | "exp" -> Exp
  | "sin" -> Sin
  | "cos" -> Cos
  | "tan" -> Tan
  | "asin" -> Asin
  | "acos" -> Acos
  | "atan" -> Atan
  | "sinh" -> Sinh
  | "cosh" -> Cosh
  | "tanh" -> Tanh
  | _ -> raise (Invalid_argument "Func.func_of_string")

let latex_of_func (f : func) =
  match f with
  | Ln -> "\\ln"
  | Exp -> "\\exp"
  | Sin -> "\\sin"
  | Cos -> "\\cos"
  | Tan -> "\\tan"
  | Asin -> "\\asin"
  | Acos -> "\\acos"
  | Atan -> "\\atan"
  | Sinh -> "\\sinh"
  | Cosh -> "\\cosh"
  | Tanh -> "\\tanh"

let func_litteral =
  [
    "ln";
    "exp";
    "sin";
    "cos";
    "tan";
    "asin";
    "acos";
    "atan";
    "sinh";
    "cosh";
    "tanh";
  ]
