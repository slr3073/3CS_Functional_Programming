(* Compte rendu TP3 - Terminaison d’une fonction récursive*)

(* 1. Ordre bien fondé *)

(* Retourne la suite bien fondé sur N à partir de n *)
(* val plusLongueSuiteDecroissante : int -> int list = <fun> *)
let rec plusLongueSuiteDecroissante (n: int) = match n with
    0 -> [0]
    | x -> n::(plusLongueSuiteDecroissante (n-1));;

(* Tests *)
(plusLongueSuiteDecroissante 5);;
- : int list = [5; 4; 3; 2; 1; 0]

(* Calcule la factorielle de n *)
(* val facto : int -> int = <fun> *)
let rec facto (n: int) = match n with
    0 -> 1
    | x -> x * (facto (x - 1));;

(* Tests *)
(facto 0);;
(* - : int = 0 *)
(facto 5);;
(* - : int = 120 *)

(* Retourne le nombre d'entiers positif dans une liste *)
(* val positif : int list -> int = <fun> *)
let rec positif (li: int list) = match li with
    a::l -> if a >= 0 then 1 + (positif l) else (positif l)
    | _ -> 0;;

(* Tests *)
positif [1;-2;-3;5];;
(* - : int = 2 *)

(* Compare des couples d'entier *)
(* val plusGrand : int * int -> int * int -> bool = <fun> *)
let plusGrand (couple1: int*int) ((a2: int), (b2: int)) = match couple1 with
    (a,b) -> a > a2 || ((a = a2) && b > b2);;

(* Tests *)
plusGrand (1,2) (2,3);;
(* - : bool = false *)

(* Fonction de ackermann *)
(* val ackermann : int * int -> int = <fun> *)
let rec ackermann (couple: int*int) = match couple with
    (0, y) -> y + 1
    | (x, 0) -> ackermann (x - 1, 1)
    | (x, y) -> ackermann (x - 1, ackermann (x, y - 1));;

(* Tests *)
ackermann (3,3);;
(* - : int = 61 *)

(* 2. Construire des ordres bien fondés *)
(* 2.1. La mesure *)
(* 2.2. Ordre lexicographique *)
(* 2.3. Multiensembles *)
(* Exercice d’Application *)

type 'a arbre_bin = 
    Feuille of 'a
    | Noeud of 'a * 'a arbre_bin * 'a arbre_bin;;

(* Echange partout les sous-arbres *)
(* val echanger : 'a arbre_bin -> 'a arbre_bin = <fun> *)
let rec echanger (arbre: 'a arbre_bin) = match arbre with
    (Feuille x) -> (Feuille x)
    | (Noeud (x, a1, a2)) -> (Noeud (x, (echanger a2), (echanger a1)));;

(* Tests *)
echanger (Noeud(1, Feuille 2, Noeud(3, Feuille 4, Feuille 5)));;
(* - : int arbre_bin = Noeud (1, Noeud (3, Feuille 5, Feuille 4), Feuille 2) *)




