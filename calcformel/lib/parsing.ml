open Expr
open Constants
open Func

exception ParsingError
exception AmbiguousExpression

let int_char_list = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
let op_list = [ '+'; '*'; '^'; '/'; '-' ]

(* removes all ' ' characters from the string *)
let remove_blank (s : string) =
  String.fold_left
    (fun acc x -> if x <> ' ' then acc ^ String.make 1 x else acc)
    "" s

(* Same as String.split_on_char but ignores occurences of [sep] inside parentheses *)
let rec split_on_char (s : string) (sep : char) =
  let n = String.length s in
  let parts = ref [] in
  let rec aux (i : int) (nb_paren : int) (acc : string) =
    if i = n then parts := acc :: !parts
    else
      let c = s.[i] in
      if c = sep && nb_paren = 0 then (
        parts := acc :: !parts;
        aux (i + 1) 0 "")
      else if c = '(' then aux (i + 1) (nb_paren + 1) (acc ^ "(")
      else if c = ')' then aux (i + 1) (nb_paren - 1) (acc ^ ")")
      else aux (i + 1) nb_paren (acc ^ String.make 1 c)
  in
  aux 0 0 "";
  !parts |> List.rev

(* checks whether the expression is well parenthesized *)
(* (opening parentheses THEN closing and no unclosed parentheses) *)
let well_parenthesized (s : string) =
  let n = String.length s in
  let nb_open = ref 0 in
  let res = ref true in
  for i = 0 to n - 1 do
    if s.[i] = '(' then incr nb_open else if s.[i] = ')' then decr nb_open;
    res := !res && !nb_open >= 0
  done;
  !res && !nb_open = 0

(* adds '*' character in the expression where multiplications are implicit *)
let add_implicit_mul (s : string) =
  let n = String.length s in
  if n < 2 then s
  else
    let s' = ref @@ String.make 1 s.[0] in
    for i = 1 to n - 1 do
      let c1 = s.[i - 1] in
      let c2 = s.[i] in
      if List.mem c1 op_list || List.mem c2 op_list then
        s' := !s' ^ String.make 1 c2
      else if c2 = '(' then s' := !s' ^ "*("
      else if c1 = ')' then s' := !s' ^ "*" ^ String.make 1 c2
      else if
        (not
           (List.mem c1 ('.' :: int_char_list)
           && List.mem c2 ('.' :: int_char_list)))
        && c2 <> ')' && c1 <> '('
      then s' := !s' ^ "*" ^ String.make 1 c2
      else s' := !s' ^ String.make 1 c2
    done;
    !s'

(* considers - signs as -1*... *)
let parse_neg (s : string) =
  String.fold_left
    (fun acc x -> if x = '-' then acc ^ "-1*" else acc ^ String.make 1 x)
    "" s

(* parse string s from index i and returns the float + the index *)
(* where the search should continue from *)
let parse_float (s : string) (i : int) =
  (* decimal : true if and only if a '.' was already parsed *)
  let n = String.length s in
  let rec aux (i : int) (decimal : bool) =
    if i >= n then ("", n)
    else
      let c = s.[i] in
      if c = '.' then
        if decimal then raise ParsingError
        else
          let s', i' = aux (i + 1) true in
          (String.make 1 '.' ^ s', i')
      else if not @@ List.mem s.[i] int_char_list then ("", i)
      else
        let s', i' = aux (i + 1) decimal in
        (String.make 1 c ^ s', i')
  in
  if s.[i] = '-' then
    let parsed_string, i' = aux (i + 1) false in
    if parsed_string = "" then raise ParsingError
    else (float_of_string parsed_string *. -1., i')
  else
    let parsed_string, i' = aux i false in
    if parsed_string = "" then raise ParsingError
    else (float_of_string parsed_string, i')

(* searches for a function litteral and return the function associated with if it exists *)
(* otherwise raises Not_found *)
let find_func_litteral (s : string) (i : int) =
  let rec aux (l : string list) =
    match l with
    | [] -> raise Not_found
    | x :: xs -> (
        try
          let n = String.length x in
          let s' = String.sub s i n in
          if s' = x then (func_of_string x, i + n) else aux xs
        with Invalid_argument _ -> aux xs)
  in
  aux func_litteral

(* searches for a constant litteral and return the constant associated with if it exists *)
(* otherwise raises Not_found *)
let find_cst_litteral (s : string) (i : int) =
  let rec aux (l : string list) =
    match l with
    | [] -> raise Not_found
    | x :: xs -> (
        try
          let n = String.length x in
          let s' = String.sub s i n in
          if s' = x then (cst_of_string x, i + n) else aux xs
        with Invalid_argument _ -> aux xs)
  in
  aux cst_litteral

(* return the index of the matching parentheses, assuming the opening is at index i *)
(* raises ParsingError otherwise *)
let rec parentheses_group (s : string) (i : int) =
  let n = String.length s in
  if i >= n || s.[i] <> '(' then raise ParsingError
  else
    let open_paren = ref 1 in
    let j = ref i in
    while !open_paren <> 0 do
      incr j;
      if !j = n then raise ParsingError;
      if s.[!j] = '(' then incr open_paren
      else if s.[!j] = ')' then decr open_paren
    done;
    !j

(* parse_inner parses string without + or * symbols *)
let rec parse_inner (s : string) =
  if s = "" then raise ParsingError
  else
    let c = s.[0] in
    (* searches for parentheses *)
    if s.[0] = '(' then
      let j = parentheses_group s 0 in
      let e = parse (String.sub s 1 (j - 1)) in
      e
    else if
      (* searches for numbers *)
      c = '.' || c = '-' || List.mem c int_char_list
    then
      let f, _ = parse_float s 0 in
      Nb f
    else
      try
        (* searches for constants *)
        let c, _ = find_cst_litteral s 0 in
        Cst c
      with Not_found -> parse_func s

(* parses functions with the form func(args) *)
and parse_func (s : string) =
  try
    let f, len = find_func_litteral s 0 in
    if s.[len] <> '(' then raise ParsingError
    else
      let e = parse (String.sub s len (String.length s - len)) in
      Func (f, e)
  with
  | Invalid_argument _ -> raise ParsingError
  | Not_found -> parse_var s

(* parses any remaining alphabetic characters as product of variables *)
and parse_var (s : string) =
  let var_list = ref [] in
  for i = 0 to String.length s - 1 do
    (* variable cannot be number *)
    if List.mem s.[i] int_char_list then raise ParsingError
    else var_list := Var (String.make 1 s.[i]) :: !var_list
  done;
  match !var_list with
  | [] -> raise ParsingError
  | [ x ] -> x
  | l -> Times l

and parse_pow (s : string) =
  match split_on_char s '^' |> List.map (fun x -> parse_inner x) with
  | [] -> raise ParsingError
  | [ x ] -> x
  | [ x1; x2 ] -> Pow (x1, x2)
  | _ -> raise AmbiguousExpression

and parse_frac (s : string) =
  match split_on_char s '/' |> List.map (fun x -> parse_pow x) with
  | [] -> raise ParsingError
  | [ x ] -> x
  | [ x1; x2 ] -> Frac (x1, x2)
  | _ -> raise AmbiguousExpression

and parse_times (s : string) =
  match split_on_char s '*' |> List.map (fun x -> parse_frac x) with
  | [] -> raise ParsingError
  | [ x ] -> x
  | l -> Times l

and parse_plus (s : string) =
  match split_on_char s '+' |> List.map (fun x -> parse_times x) with
  | [] -> raise ParsingError
  | [ x ] -> x
  | l -> Plus l

and parse (s : string) =
  if not @@ well_parenthesized s then raise ParsingError
  else s |> remove_blank |> add_implicit_mul |> parse_neg |> parse_plus
