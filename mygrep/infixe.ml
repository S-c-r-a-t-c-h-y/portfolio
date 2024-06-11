(** On considère l'expression régulière non-vide *)
type regexp =
  | Epsilon
  | Lettre of int
  | Or of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp
  | Optional of regexp
  (* La suite sert lors de la construction de l'expression régulière *)
  | Car of char
  | Any
  (* Littéraux pour les caractères d'opération *)
  | Op_or
  | Op_star
  | Op_opt
  (* Caractère utilisé pour le remplissage de la fin du tableau lors de la construction de la regexp *)
  | End

type automate = {
  nb_etats : int;
  initial : int;
  terminaux : bool array;
  transitions : (int * int) list array; (* destination, étiquette *)
}

exception Mal_parenthese
exception Expression_invalide

(** Raise Mal_parenthese si la string passée en paramètre est mal parenthésée *)
let bien_parenthese str =
  let nb_ouvert = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = '(' then incr nb_ouvert
    else if str.[i] = ')' then
      if !nb_ouvert > 0 then decr nb_ouvert else raise Mal_parenthese
  done;
  if !nb_ouvert <> 0 then raise Mal_parenthese

(** [length_group str i] renvoie la longueur du groupe contenu entre les parenthèses commençants à l'indice [i] dans [str] *)
let length_group str i =
  assert (str.[i] = '(');
  let num_parenthese = ref 0 in
  let j = ref @@ (i + 1) in
  while str.[!j] != ')' || !num_parenthese <> 0 do
    if str.[!j] = '(' then incr num_parenthese;
    if str.[!j] = ')' then decr num_parenthese;
    incr j
  done;
  !j - i - 1

(** [index arr elem] renvoie l'indice de la première occurence de elem dans arr si elle existe et None sinon *)
let index arr elem =
  let res = ref None in
  for i = 0 to Array.length arr - 1 do
    if arr.(i) = elem then if Option.is_none !res then res := Some i
  done;
  !res

(** Permet de parser les concaténations implicites dans une liste d'expressions régulières représentant l'expression régulière *)
let rec parser_concat l =
  match l with
  | [] -> raise Expression_invalide
  | [ e ] -> e
  | x :: xs -> Concat (x, parser_concat xs)

(** Permet de parser les * et les ? dans un tableau d'expressions régulières représentant l'expression régulière  *)
let parser_etoile_opt tab =
  let n = Array.length tab in
  let res = ref [] in
  let i = ref @@ (n - 1) in
  while !i >= 0 do
    if tab.(!i) = Op_star then
      if !i = 0 then raise Expression_invalide
      else (
        res := Star tab.(!i - 1) :: !res;
        i := !i - 2)
    else if tab.(!i) = Op_opt then
      if !i = 0 then raise Expression_invalide
      else (
        res := Optional tab.(!i - 1) :: !res;
        i := !i - 2)
    else (
      if tab.(!i) <> End then res := tab.(!i) :: !res;
      decr i)
  done;
  parser_concat !res

(** Permet de parser les | dans un tableau d'expressions régulières représentant l'expression régulière *)
let rec parser_or (tab : regexp array) =
  match index tab Op_or with
  | None -> parser_etoile_opt tab
  | Some i ->
      Or
        ( parser_or (Array.sub tab 0 i),
          parser_or (Array.sub tab (i + 1) (Array.length tab - i - 1)) )

(* let parser_or tab = End *)

(** Créé l'expression régulière associée à la string str donnée en notation INFIXE, de la même manière qu'on l'écrirait à la main  *)
let rec creer_regexp str =
  bien_parenthese str;
  let n = String.length str in
  let tab = Array.make n End in
  let j = ref 0 in
  let i = ref 0 in
  while !i < n do
    let c = str.[!i] in
    if c = '(' then (
      let len_group = length_group str !i in
      tab.(!j) <- creer_regexp (String.sub str (!i + 1) len_group);
      i := !i + len_group + 1)
    else if c = '*' then tab.(!j) <- Op_star
    else if c = '?' then tab.(!j) <- Op_opt
    else if c = '|' then tab.(!j) <- Op_or
    else if c = '.' then tab.(!j) <- Any
    else tab.(!j) <- Car c;
    incr j;
    incr i
  done;
  parser_or tab

(** Créé une expression régulière correspondant à une alternative de tous les caractères et met à jour le dictionnaire  *)
let rec create_any (dict : (int, char) Hashtbl.t) (num : int) i =
  Hashtbl.add dict num (char_of_int i);
  if i = 255 then Lettre num
  else Or (Lettre num, create_any dict (num + 1) (i + 1))

(** Linéarise l'expression régulière en assignant chaque caractère à un nombre unique et renvoie l'expression linéarisée ainsi que le dictionnaire permettant de démarquer l'expression  *)
let linearise exp =
  let hash = Hashtbl.create 1000 in
  let rec aux num exp : regexp * int =
    match exp with
    | Epsilon -> (Epsilon, num)
    | Lettre i -> (Lettre i, num)
    | Or (e1, e2) ->
        let new_exp1, new_num1 = aux num e1 in
        let new_exp2, new_num2 = aux new_num1 e2 in
        (Or (new_exp1, new_exp2), new_num2)
    | Concat (e1, e2) ->
        let new_exp1, new_num1 = aux num e1 in
        let new_exp2, new_num2 = aux new_num1 e2 in
        (Concat (new_exp1, new_exp2), new_num2)
    | Star e ->
        let new_exp, new_num = aux num e in
        (Star new_exp, new_num)
    | Optional e ->
        let new_exp, new_num = aux num e in
        (Optional new_exp, new_num)
    | Car c ->
        Hashtbl.add hash num c;
        (Lettre num, num + 1)
    | Any -> (create_any hash num 0, num + 256)
    | Op_or | Op_star | Op_opt | End -> raise Expression_invalide
  in

  let exp, _ = aux 1 exp in
  (exp, hash)

(* Partie commmune  *)

(** Donne le nombre de lettres dans une expression régulière *)
let rec nb_lettres exp =
  match exp with
  | Epsilon -> 0
  | Lettre _ -> 1
  | Or (e1, e2) | Concat (e1, e2) -> nb_lettres e1 + nb_lettres e2
  | Star e -> nb_lettres e
  | Optional e -> nb_lettres e
  | _ -> failwith "ne devrais pas arriver"

(** Détermine si une expression régulière contient epsilon ou non *)
let rec a_epsilon exp =
  match exp with
  | Epsilon -> true
  | Lettre _ -> false
  | Or (e1, e2) -> a_epsilon e1 || a_epsilon e2
  | Concat (e1, e2) -> a_epsilon e1 && a_epsilon e2
  | Star e -> true
  | Optional e -> true
  | _ -> failwith "ne devrais pas arriver"

(** Réalise le produit cartésien de deux listes *)
let produit_cartesien l1 l2 =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) l2) l1

(** Renvoie les préfixes d'une expression régulière sous la forme d'une liste *)
let prefixe exp =
  let rec aux exp =
    match exp with
    | Epsilon -> []
    | Lettre l -> [ l ]
    | Or (e1, e2) -> aux e1 @ aux e2
    | Concat (e1, e2) -> if a_epsilon e1 then aux e1 @ aux e2 else aux e1
    | Star e -> aux e
    | Optional e -> aux e
    | _ -> failwith "ne devrais pas arriver"
  in

  aux exp

(** Renvoie les suffixes d'une expression régulière sous la forme d'une liste *)
let suffixe exp =
  let rec aux exp =
    match exp with
    | Epsilon -> []
    | Lettre l -> [ l ]
    | Or (e1, e2) -> aux e1 @ aux e2
    | Concat (e1, e2) -> if a_epsilon e2 then aux e1 @ aux e2 else aux e2
    | Star e -> aux e
    | Optional e -> aux e
    | _ -> failwith "ne devrais pas arriver"
  in

  aux exp

(** Renvoie les facteurs d'une expression régulière sous la forme d'une liste de couple de deux lettres (représentés par des int) *)
let rec facteur exp =
  match exp with
  | Epsilon -> []
  | Lettre l -> []
  | Or (e1, e2) -> facteur e1 @ facteur e2
  | Concat (e1, e2) ->
      facteur e1 @ facteur e2 @ produit_cartesien (suffixe e1) (prefixe e2)
  | Star e -> facteur e @ produit_cartesien (suffixe e) (prefixe e)
  | Optional e -> facteur e
  | _ -> failwith "ne devrais pas arriver"

(** Renvoie une liste telle que pour chaque élément x de la liste prise en entrée, il devient un couple (x,x) *)
let rec liste_pref_couple pref =
  match pref with [] -> [] | h :: q -> (h, h) :: liste_pref_couple q

(** Renvoie un tableaux de booléens où la case d'indice i prend la valeur true si l'état i est terminal, false sinon *)
let etats_terminaux exp =
  let tab_term = Array.make (nb_lettres exp + 1) false in
  let tab_suff = Array.of_list (suffixe exp) in
  let n = Array.length tab_suff in
  for i = 0 to n - 1 do
    tab_term.(tab_suff.(i)) <- true
  done;
  if a_epsilon exp then tab_term.(0) <- true;
  tab_term

(** Renvoie un tableau composée de listes de couples (transitions, chemins) : pour l'indice i, il s'agit de la liste des couples (transitions, chemins) à partir de l'état i *)
let transi exp =
  let tab_transi = Array.make (nb_lettres exp + 1) [] in
  tab_transi.(0) <- liste_pref_couple (prefixe exp);
  let tab_fac = Array.of_list (facteur exp) in
  let n = Array.length tab_fac in
  for i = 0 to n - 1 do
    let depart, arrivee = tab_fac.(i) in
    tab_transi.(depart) <- (arrivee, arrivee) :: tab_transi.(depart)
  done;
  tab_transi

(** Renvoie l'automate de Glushkov construit à partir d'une expression régulière prise en entrée *)
let construction_automate exp =
  {
    nb_etats = nb_lettres exp + 1;
    initial = 0;
    terminaux = etats_terminaux exp;
    transitions = transi exp;
  }

(** Démarque les sommets de l'automate obtenu après sa construction initiale en gardant les mêmes sommets  *)
let demarquer (aut : automate) (dict : (int, char) Hashtbl.t) =
  {
    nb_etats = aut.nb_etats;
    initial = aut.initial;
    terminaux = aut.terminaux;
    transitions =
      Array.map
        (fun l ->
          List.map
            (fun (x, etiquette) ->
              (x, Hashtbl.find dict etiquette |> Char.code))
            l)
        aut.transitions;
  }

(** Construit l'automate de Glushkov à partir d'une expression régulière INFIXE sous forme de string *)
let automate_from_string s =
  let reg = creer_regexp s in
  let reg, hash = linearise reg in
  let aut = construction_automate reg in
  let aut = demarquer aut hash in
  aut

(** Détermine si un mot représenté sous forme de int list des codes ASCII de ses caractères est reconnu par l'automate *)
let reconnu a l =
  let rec aux q l =
    match l with
    | [] -> [ q ]
    | x :: xs ->
        List.fold_left
          (fun acc (q', y) -> if y = x then aux q' xs @ acc else acc)
          [] a.transitions.(q)
  in
  aux a.initial l |> List.exists (fun x -> a.terminaux.(x))

(** Donne la liste correspondant aux codes ASCII caractères du mot s *)
let string_to_int_list s =
  List.init (String.length s) (fun i -> Char.code s.[i])

let process_line aut line =
  if reconnu aut (string_to_int_list line) then Printf.printf "%s\n%!" line

(* Lecture de l'entrÃ©e, ligne par ligne *)
let process aut input =
  try
    while true do
      let line = Stdlib.input_line input in
      process_line aut line
    done
  with End_of_file -> ()

let main () =
  (* VÃ©rifie que l'expression rÃ©guliÃ¨re est bien prÃ©sente en premier
     argument. Sinon, on affiche un message indiquant comment utiliser
     ce programme et on quitte avec un code d'erreur de `1`. *)
  let argc = Array.length Sys.argv in
  if argc < 2 || argc > 3 then (
    Printf.printf "usage : %s regex [file]\n%!" Sys.argv.(0);
    exit 1);
  (* S'il y a un deuxiÃ¨me argument c'est qu'il faut lire dans ce
     fichier, sinon, on utilise l'entrÃ©e standard. *)
  let input = if argc = 3 then Stdlib.open_in Sys.argv.(2) else Stdlib.stdin in
  Printf.printf "* Regexp you entered is '%s'\n* Reading from %s\n\n%!"
    Sys.argv.(1)
    (if argc = 3 then Sys.argv.(2) else "stdin");
  let aut = automate_from_string Sys.argv.(1) in
  process aut input;
  if argc = 3 then Stdlib.close_in input

(* let start_time = Sys.time () in
   main ();
   Printf.printf "(* code *) %fs\n" (Sys.time () -. start_time) *)

let () = main ()
