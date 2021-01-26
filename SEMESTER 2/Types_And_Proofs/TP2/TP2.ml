(* Compte rendu TDP n°2 - Arbres binaires de recherche *)

(* Introduction, le type 'a arbre_binaire *)
type 'a arbre_binaire =
    Feuille
    | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire;;

let abr1 = Noeud( 8, Noeud (3, Noeud(2, Feuille, Feuille), Noeud(6, Feuille, Feuille)),Noeud (19, Feuille, Feuille));;
let abr2 = Noeud(true, Noeud(false, Feuille, Feuille), Feuille);;

(* Fonctions de base *)

exception PasDeRacine;;

(* Retourne la racine d'un arbre binaire *)
(* val racine : 'a arbre_binaire -> 'a = <fun> *)
let racine (abr: 'a arbre_binaire) = match abr with
    Feuille -> raise PasDeRacine
    | Noeud(x,_,_) -> x;;

(* Tests *)
racine abr1;;
(* - : int = 8 *)
racine abr2;;
(* - : bool = true *)
racine Feuille;;
(* Exception: PasDeRacine. *)

(* Recherche si un element est dans un arbre *)
(* val recherche : 'a -> 'a arbre_binaire -> bool = <fun> *)
let rec recherche (x: 'a) (abr:'a arbre_binaire) = match abr with
    Feuille -> false
    | Noeud(y,a1,a2) -> (y = x) || (recherche x a1) || (recherche x a2);;

(* Tests *)
recherche 6 abr1;;
(* - : bool = true *)
recherche 15 abr1;;
(* - : bool = false *)

(* Retourne le sous arbre *)
(* val cherche_sous_arbre : 'a -> 'a arbre_binaire -> 'a arbre_binaire = <fun> *)
let rec cherche_sous_arbre (x : 'a) (abr:'a arbre_binaire) = match abr with
    Noeud (y,a1,a2) -> 
        if y = x then
            Noeud (y,a1,a2)
        else
            if (cherche_sous_arbre x a1) = Feuille then
                (cherche_sous_arbre x a2)
            else
                (cherche_sous_arbre x a1)
    | Feuille -> Feuille;;

(* Tests *)
cherche_sous_arbre 2 abr1;;


