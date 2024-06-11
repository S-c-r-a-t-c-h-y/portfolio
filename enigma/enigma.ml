(** [pow x n] calcul [x^n] avec la technique d'exponentiation rapide *)
let rec pow x n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let res = pow x (n / 2) in
    res * res
  else x * pow x (n - 1)

(** [g % f] renvoie la fonction composée (g o f)  *)
let ( % ) g f x = f x |> g

(** On récupère la valeur de u_0 en argument de ligne de commande. u_0 = 42 par défaut  *)
let u0 = if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 42

let m = pow 2 20 + 7

(** formule de récurrence en passant en paramètre u_n  *)
let rec u_next u = 2035 * u mod m

(** Séquence infinie des u_n  *)
let u_seq = Seq.iterate u_next u0 |> Seq.memoize

(** Renvoie un array avec les (n+1) premiers termes de u_n *)
let u_array n = Seq.take (n + 1) u_seq |> Array.of_seq

(** Renvoie la valeur de v_{i,j} en connaissant les valeurs de u_n *)
let v i j u =
  ((j + 1) * u.(i + j) |> float) /. float m |> Float.floor |> int_of_float

(** Calcul la permutation pi_{n,i}  *)
let pi n i =
  let x = Array.make n 0 in
  let u = u_array (n + i) in
  for j = 0 to n - 1 do
    let vij = v i j u in
    x.(j) <- x.(vij);
    x.(vij) <- j
  done;
  fun y -> x.(y)

(** Donne la caractéristique de la permutation pi n i *)
let caracteristique_int n i =
  let s = pi n i in
  let carac = ref [] in
  let vu = Array.make n false in
  let rec calculer_longueur_cycle v =
    vu.(v) <- true;
    let valeur = s v in
    if vu.(valeur) then 1 else 1 + calculer_longueur_cycle valeur
  in
  for i = 0 to n - 1 do
    if not vu.(i) then carac := calculer_longueur_cycle (s i) :: !carac
  done;
  !carac |> List.fast_sort compare

(** Bijection phi entre l'ensemble [[0, 25]] et l'ensemble des lettres de l'alphabet  *)
let phi n = Char.chr (n + 65)

(** Bijection réciproque de phi  *)
let phi_inv lettre = Char.code lettre - 65

(** Donne la caractéristique d'une permutation des lettres de l'alphabet  *)
let caracteristique_char s =
  let carac = ref [] in
  let vu = Array.make 26 false in
  let rec calculer_longueur_cycle v =
    vu.(phi_inv v) <- true;
    let valeur = s v in
    if vu.(phi_inv valeur) then 1 else 1 + calculer_longueur_cycle valeur
  in
  for i = 0 to 25 do
    if not vu.(i) then carac := calculer_longueur_cycle (s (phi i)) :: !carac
  done;
  !carac |> List.sort compare

(** Donne la configuration pseudo-aléatoire k_i associée aux paramètres m n et i *)
let k m n i =
  let pi1 = pi 5 i in
  let pi2 = pi 26 (i + 10) in
  let u = u_array (i + m + 4) in
  let r = Array.init m (fun j -> pi1 j)
  and p = Array.init m (fun j -> phi (u.(i + 5 + j) mod 26))
  and c =
    Array.init n (fun j -> (phi (pi2 (2 * j)), phi (pi2 ((2 * j) + 1))))
  in
  (r, p, c)

(** Permutation circulaire des lettres de l'alphabet  *)
let rho l = (phi_inv l + 1) mod 26 |> phi

(** Itérée de la permutation rho : rho_n n = rho^n  *)
let rec rho_n n l =
  if n < 0 then rho_n (26 + (n mod 26)) l
  else if n = 0 then l
  else rho (rho_n (n - 1) l)

(** Donne la permutation inverse d'une permutation des lettres de l'alphabet  *)
let inverser_permutation (s : char -> char) =
  let l = Array.init 26 (fun i -> phi i |> s |> phi_inv) in
  let inverse = Array.make 26 (-1) in
  for i = 0 to 25 do
    inverse.(l.(i)) <- i
  done;
  fun x -> inverse.(phi_inv x) |> phi

(* Permutation des rotors + réflécteur *)

let sigma1 c = "EKMFLGDQVZNTOWYHXUSPAIBRCJ".[phi_inv c]
let sigma2 c = "AJDKSIRUXBLHWTMCQGZNPYFVOE".[phi_inv c]
let sigma3 c = "BDFHJLCPRTXVZNYEIWGAKMUSQO".[phi_inv c]
let sigma4 c = "ESOVPZJAYQUIRHXLNFTGKDCMWB".[phi_inv c]
let sigma5 c = "VZBRGITYUPSDNHLXAWMJQOFECK".[phi_inv c]
let sigmaB c = "YRUHQSLDPXNGOKMIEBFZCWVJAT".[phi_inv c]

(** Calcul la permutation sigmaC en connaissance du tableau de connexions  *)
let sigmaC (c : (char * char) array) =
  let permu = Array.init 26 (fun i -> phi i) in
  Array.iter
    (fun (a, b) ->
      permu.(phi_inv a) <- b;
      permu.(phi_inv b) <- a)
    c;
  fun x -> permu.(phi_inv x)

(** Bijection entre [[0, 4]] et l'ensemble des permutations des rotors  *)
let psi = function
  | 0 -> sigma1
  | 1 -> sigma2
  | 2 -> sigma3
  | 3 -> sigma4
  | 4 -> sigma5
  | _ -> failwith "identifiant incorrect"

(** Permutation sigmaRP pour une configuration r, p donnée  *)
let sigmaRP r p =
  let m = Array.length r in
  let rec aux n =
    if n = m - 1 then rho_n (-phi_inv p.(n)) % psi r.(n) % rho_n (phi_inv p.(n))
    else rho_n (-phi_inv p.(n)) % psi r.(n) % rho_n (phi_inv p.(n)) % aux (n + 1)
  in
  aux 0

(** Permutation sigmaRP chapeau pour une configuration r, p donnée  *)
let sigmaRP_hat r p =
  let s = sigmaRP r p in
  inverser_permutation s % sigmaB % s

(* Permutation sigmaK pour une configuration k = r, p, c donnée *)
let sigmaK r p c =
  let sc = sigmaC c in
  sc % sigmaRP_hat r p % sc

(** Applique la fonction delta d'avancement des rotors pour une configuration r, p donnée et renvoie p  *)
let deltaR r p =
  let m = Array.length r in
  let entrainement = [| 'Q'; 'E'; 'V'; 'J'; 'Z' |] in
  let rec tourner i =
    if p.(i) = entrainement.(r.(i)) && i <> 0 then tourner (i - 1);
    p.(i) <- rho p.(i)
  in
  tourner (m - 1);
  p

(** Itérée de la fonction deltaR : deltaR_itere k = deltaR^k  *)
let deltaR_itere k r p =
  let rec aux k p = if k = 0 then p else deltaR r (aux (k - 1) p) in
  aux k (Array.copy p)

(** Même fonction que deltaR_itere mais renvoie la configuration k entière au lieu de p  *)
let delta_itere n k =
  let r, p, c = k in
  (r, deltaR_itere n r p, c)

(** Chiffre un message en utilisant la machine Enigma avec une configuration initiale k  *)
let chiffrer k message =
  let r, p, c = k in
  let p = ref p in
  let code = ref "" in
  for i = 0 to String.length message - 1 do
    deltaR r !p |> ignore;
    let sk = sigmaK r !p c in
    code := !code ^ Char.escaped (sk message.[i])
  done;
  !code

(** Permutation S_k chapeau pour une configuration r, p donnée  *)
let sK_hat r p =
  let m = Array.length r in
  sigmaRP_hat r (deltaR_itere m r p) % sigmaRP_hat r p

(** Permutation S_k pour une configuration k donnée  *)
let sK k =
  let r, p, c = k in
  let sc = sigmaC c in
  sc % sK_hat r p % sc

(** Répète deux fois les éléments d'une liste dans le même ordre i.e [repeter_liste [1; 2; 3] = [1; 1; 2; 2; 3; 3]] *)
let repeter_liste l = List.fold_right (fun y acc -> y :: y :: acc) l []

(** Renvoie une liste de toutes les configurations possibles pour R avec m rotors *)
let configR m =
  let set t i v =
    Array.init (Array.length t) (fun j -> if j = i then v else t.(j))
  in
  let combi_from_indices t m =
    let res = Array.make m 0 in
    for j = 0 to 4 do
      if t.(j) <> -1 then res.(t.(j)) <- j
    done;
    res
  in
  let res = ref [] in
  let rec aux n k choix =
    if n = 0 then res := combi_from_indices choix m :: !res
    else
      for i = 0 to 4 do
        if choix.(i) = -1 then aux (n - 1) (k + 1) (set choix i k)
      done
  in
  aux m 0 [| -1; -1; -1; -1; -1 |];
  !res |> List.rev (* on inverse l'ordre pour obtenir l'ordre lexicographique *)

(** Renvoie une liste de toutes les configurations possibles pour P avec m rotors *)
let configP m =
  let alphabet = List.init 26 (fun i -> phi i) in
  let rec aux m acc pref =
    if m = 0 then Array.of_list (List.rev pref) :: acc
    else
      List.fold_left (fun acc' x -> aux (m - 1) acc' (x :: pref)) acc alphabet
  in
  aux m [] []
  |> List.rev (* on inverse l'ordre pour obtenir l'ordre lexicographique *)

(** Réalise le produit cartésien des lists l1 et l2 *)
let produit_cartesien l1 l2 =
  List.concat_map (fun a -> List.map (fun b -> (a, b)) l2) l1

(** Donne la réponse à la Q9  *)
let q9 m j =
  let check_list = List.init m (fun i -> i) in
  let car_list =
    Array.init m (fun i ->
        caracteristique_int 13 ((13 * i) + j) |> repeter_liste)
  in
  List.fold_left
    (fun (cnt, conf) (r, p) ->
      if
        List.for_all
          (fun i ->
            caracteristique_char (sK_hat r (deltaR_itere (i + 1) r p))
            = car_list.(i))
          check_list
      then if cnt = 0 then (1, Some (r, p)) else (cnt + 1, conf)
      else (cnt, conf))
    (0, None)
    (produit_cartesien (configR m) (configP m))

(** Génère la chaine de caractère pseudo-aléatoire mu_{l, i} pour l et i donné  *)
let mu l i =
  let u = u_array (i + l - 1) in
  String.init l (fun j -> phi (u.(i + j) mod 26))

(** Donne le nombre d'alignement possible pour le crib z dans m' et si un tel alignement existe le premier alignement possible  *)
let alignements_valides z m' =
  let l = String.length m' in
  let k = String.length z in
  let nb = ref 0 in
  let premier_alignement = ref None in
  let check_list = List.init k (fun i -> i) in
  for j = 0 to l - k do
    if not @@ List.exists (fun n -> z.[n] = m'.[n + j]) check_list then (
      if Option.is_none !premier_alignement then premier_alignement := Some j;
      incr nb)
  done;
  (!nb, !premier_alignement)

(** On représente les menus par des listes d'adjacences en identifiant une lettre à sa place dans l'alphabet en partant de 0  *)
type menu = (int * int) list array
(** Le graphe est non orienté  *)

(** Créé un menu vide *)
let create_menu () : menu = Array.make 26 []

(** Ajoute une arête entre deux sommets l1 et l2 en l'étiquettant par v  *)
let add_edge (m : menu) l1 l2 v =
  m.(phi_inv l1) <- (phi_inv l2, v) :: m.(phi_inv l1);
  m.(phi_inv l2) <- (phi_inv l1, v) :: m.(phi_inv l2)

(** Renvoi le menu associé aux paramètres k et i  *)
let menu k i =
  let m' = mu 100 (i + k) in
  let z = mu k i in
  let _, d = alignements_valides z m' in
  let d = Option.get d in
  let m = create_menu () in
  for j = 0 to k - 1 do
    add_edge m z.[j] m'.[d + j] (d + j)
  done;
  m

(** Réponse à la Q11  *)
let q11 k i =
  let m = menu k i in
  let deg_max = ref @@ List.length m.(0) in
  let sommet_max = ref 0 in
  for i = 1 to 25 do
    let deg = List.length m.(i) in
    if deg > !deg_max then (
      deg_max := deg;
      sommet_max := i)
  done;
  (phi !sommet_max, List.map (fun (j, v) -> (phi j, v)) m.(!sommet_max))

(* Affichage visuel pour les réponses aux questions  *)

let pp_int_list outchan l =
  let rec print_list = function
    | [] -> ()
    | [ e ] -> Printf.fprintf outchan "%d" e
    | e :: q ->
        Printf.fprintf outchan "%d; " e;
        print_list q
  in
  Printf.fprintf outchan "[";
  print_list l;
  Printf.fprintf outchan "]"

let pp_char_int_list outchan l =
  let rec print_list = function
    | [] -> ()
    | [ (c, n) ] -> Printf.fprintf outchan "(%c, %d)" c n
    | (c, n) :: q ->
        Printf.fprintf outchan "(%c, %d); " c n;
        print_list q
  in
  Printf.fprintf outchan "[";
  print_list l;
  Printf.fprintf outchan "]"

let pp_char_array outchan arr =
  let n = Array.length arr in
  Printf.fprintf outchan "[|";
  for i = 0 to n - 2 do
    Printf.fprintf outchan "%c; " arr.(i)
  done;
  if n <> 0 then Printf.fprintf outchan "%c" arr.(n - 1);
  Printf.fprintf outchan "|]"

let pp_int_array outchan arr =
  let n = Array.length arr in
  Printf.fprintf outchan "[|";
  for i = 0 to n - 2 do
    Printf.fprintf outchan "%d; " arr.(i)
  done;
  if n <> 0 then Printf.fprintf outchan "%d" arr.(n - 1);
  Printf.fprintf outchan "|]"

let () =
  let u_arr = u_array 100030 in
  Printf.printf "\nQuestion 1 :\n\n%d, %d\n" u_arr.(10) (v 10 10 u_arr);
  Printf.printf "%d, %d\n" u_arr.(1000) (v 1000 20 u_arr);
  Printf.printf "%d, %d\n" u_arr.(100000) (v 100000 30 u_arr);

  let p = pi 5 10 in
  Printf.printf "\nQuestion 2 :\n\n%d, %d, %d\n" (p 0) (p 1) (p 4);
  let p = pi 50 1000 in
  Printf.printf "%d, %d, %d\n" (p 0) (p 1) (p 49);
  let p = pi 500 100000 in
  Printf.printf "%d, %d, %d\n" (p 0) (p 1) (p 499);

  Printf.printf "\nQuestion 3 :\n\n%a\n%a\n%a\n" pp_int_list
    (caracteristique_int 5 10) pp_int_list
    (caracteristique_int 50 1000)
    pp_int_list
    (caracteristique_int 500 100000);

  let r, p, _ = k 1 0 10 in
  let s = sigmaRP r p in
  let x = s 'A' in
  let y = sigmaB x in
  let z = inverser_permutation s y in
  Printf.printf "\nQuestion 4 :\n\n%c, %c, %c\n" x y z;
  let r, p, _ = k 2 0 100 in
  let s = sigmaRP r p in
  let x = s 'A' in
  let y = sigmaB x in
  let z = inverser_permutation s y in
  Printf.printf "%c, %c, %c\n" x y z;
  let r, p, _ = k 3 0 1000 in
  let s = sigmaRP r p in
  let x = s 'A' in
  let y = sigmaB x in
  let z = inverser_permutation s y in
  Printf.printf "%c, %c, %c\n" x y z;

  let r, p, c = k 1 5 20 in
  let s = sigmaC c in
  let x = s 'A' in
  let y = sigmaRP_hat r p x in
  let z = s y in
  Printf.printf "\nQuestion 5 :\n\n%c, %c, %c\n" x y z;
  let r, p, c = k 2 8 200 in
  let s = sigmaC c in
  let x = s 'A' in
  let y = sigmaRP_hat r p x in
  let z = s y in
  Printf.printf "%c, %c, %c\n" x y z;
  let r, p, c = k 3 10 2000 in
  let s = sigmaC c in
  let x = s 'A' in
  let y = sigmaRP_hat r p x in
  let z = s y in
  Printf.printf "%c, %c, %c\n" x y z;

  let r, p, _ = k 3 0 30 in
  Printf.printf "\nQuestion 6 :\n\n%a\n" pp_char_array (deltaR_itere 100 r p);
  let r, p, _ = k 4 0 300 in
  Printf.printf "%a\n" pp_char_array (deltaR_itere 1000 r p);
  let r, p, _ = k 5 0 3000 in
  Printf.printf "%a\n" pp_char_array (deltaR_itere 10000 r p);
  Printf.printf "\nQuestion 7 :\n\n%s\n%s\n%s\n"
    (chiffrer (k 1 5 40) "MESSAGE")
    (chiffrer (k 2 8 400) "MESSAGE")
    (chiffrer (k 3 10 4000) "MESSAGE");

  Printf.printf "\nQuestion 8 :\n\n%a\n%a\n%a\n" pp_int_list
    (caracteristique_char (sK (k 1 5 50)))
    pp_int_list
    (caracteristique_char (sK (k 2 8 500)))
    pp_int_list
    (caracteristique_char (sK (k 3 10 5000)));

  Printf.printf "\nQuestion 9 :\n\n";
  let n, o = q9 1 10 in
  (if n = 0 then Printf.printf "0\n"
   else
     let r, p = Option.get o in
     Printf.printf "%d, (%a, %a)\n" n pp_int_array r pp_char_array p);
  let n, o = q9 2 100 in
  (if n = 0 then Printf.printf "0\n"
   else
     let r, p = Option.get o in
     Printf.printf "%d, (%a, %a)\n" n pp_int_array r pp_char_array p);
  let n, o = q9 3 1000 in
  (if n = 0 then Printf.printf "0\n"
   else
     let r, p = Option.get o in
     Printf.printf "%d, (%a, %a)\n" n pp_int_array r pp_char_array p);

  let n, a = alignements_valides (mu 20 50) (mu 100 70) in
  Printf.printf "\nQuestion 10 :\n\n%d, %d\n" n (Option.get a);
  let n, a = alignements_valides (mu 50 500) (mu 1000 550) in
  Printf.printf "%d, %d\n" n (Option.get a);
  let n, a = alignements_valides (mu 100 5000) (mu 10000 5100) in
  Printf.printf "%d, %d\n" n (Option.get a);

  let c, l = q11 10 50 in
  Printf.printf "\nQuestion 11 :\n\n%c, %a\n" c pp_char_int_list l;
  let c, l = q11 15 500 in
  Printf.printf "%c, %a\n" c pp_char_int_list l;
  let c, l = q11 20 5000 in
  Printf.printf "%c, %a\n" c pp_char_int_list l
