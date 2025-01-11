open Expr

let pretty_string_of_float (f : float) =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let preprocess_string s =
  let lex = Lexing.from_string s in
  try Parser.preprocess Lexer.token lex
  with e ->
    flush stdout;
    Printf.eprintf "Error : %s near '%s'.\n" (Printexc.to_string e)
      (Lexing.lexeme lex);
    flush stderr;
    failwith "preprocess_string"

let expr_of_string s =
  let lex = Lexing.from_string (preprocess_string s) in
  try Parser.expr Lexer.token lex
  with e ->
    flush stdout;
    Printf.eprintf "Error : %s near '%s'.\n" (Printexc.to_string e)
      (Lexing.lexeme lex);
    flush stderr;
    failwith "expr_of_string"

let string_of_expr (e : t) =
  let rec print_list (l : t list) (sep : string) =
    match l with
    | [] -> ""
    | [ x ] -> (
        match x with
        | Plus _ -> if sep = "*" then aux x true else aux x false
        | _ -> aux x false)
    | Times [ Float -1.; y ] :: xs when sep = "+" ->
        "-" ^ print_list (y :: xs) sep
    | x :: xs ->
        (match x with
        | Plus _ -> if sep = "*" then aux x true else aux x false
        | _ -> aux x false)
        ^ sep ^ print_list xs sep
  and aux e encapsulate =
    match e with
    | Float i -> pretty_string_of_float i
    | Var i -> i
    | Plus l ->
        if encapsulate then "(" ^ print_list l "+" ^ ")" else print_list l "+"
    | Times l ->
        if encapsulate then "(" ^ print_list l "*" ^ ")" else print_list l "*"
    | Frac (e1, e2) ->
        if encapsulate then "(" ^ aux e1 true ^ "/" ^ aux e2 true ^ ")"
        else aux e1 true ^ "/" ^ aux e2 true
    | Power (e1, e2) ->
        if encapsulate then "(" ^ aux e1 true ^ "^" ^ aux e2 true ^ ")"
        else aux e1 true ^ "^" ^ aux e2 true
    | Func (f, e2) -> f ^ "(" ^ aux e2 false ^ ")"
  in
  aux e false
