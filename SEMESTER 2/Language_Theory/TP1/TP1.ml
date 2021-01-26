(* Compte rendu TP1 - Mots et langages *)

(* Alphabets et mots *)
(* 1 - Conversion *)
let sigma = ['a';'b'];;
let w = ['a';'b';'b';'a'];;

(* Convertis une string en list de char *)
(* val conversion : string -> char list = <fun> *)
let rec conversion (s: string) = match s with
    "" -> []
    | _ -> s.[0]::(conversion (String.sub s 1 ((String.length s) - 1) ));;

(* Tests *)
conversion "bonjour";;
(* - : char list = ['b'; 'o'; 'n'; 'j'; 'o'; 'u'; 'r'] *)

(* 2 - Premières définitions *)

(* Retoune si une lettre est dans un alphabet *)
(* val teste_lettre : 'a -> 'a list -> bool = <fun> *)
let rec teste_lettre (le: 'a) (li: 'a list) = match li with
    a::l -> a = le || (teste_lettre le l)
    | _ -> false;;

(* Tests *)
(teste_lettre 'a' sigma);;
(* - : bool = true *)
(teste_lettre 'r' sigma);;
(* - : bool = false *)

(* Retourne si un mot est bien sur un alphabet *)
(* val teste_mot : 'a list -> 'a list -> bool = <fun> *)
let rec teste_mot (w: 'a list) (sigma: 'a list) = match w with
    a::l -> (teste_lettre a sigma) && (teste_mot l sigma)
    | _ -> true;;

(* Tests *)
(teste_mot ['b';'a';'b'] sigma );;
(* - : bool = true *)
(teste_mot ['b';'a';'c'] sigma );;
(* - : bool = false *)

(* Retourne si deux mots sont egaux *)
(* val egalite : 'a list * 'a list -> bool = <fun> *)
let egalite ((w1: 'a list), (w2: 'a list)) = w1 = w2;;

(* Tests *)
egalite (w,w) ;;
(* - : bool = true *)
egalite (w,[]) ;;
(* - : bool = false *)

(* Retourne la concatenation de deux mots *)
(* val concat : 'a list -> 'a list -> 'a list = <fun> *)
let concat (w1: 'a list) (w2: 'a list) = w1@w2;;

(* Tests *)
concat w w;;
(* - : char list = ['a'; 'b'; 'b'; 'a'; 'a'; 'b'; 'b'; 'a'] *)

(* 3 - Préfixe et puissance *)

exception Erreur;;

(* Retourne un mot w à la puissance p *)
(* val puissance : 'a list -> int -> 'a list = <fun> *)
let rec puissance (w: 'a list) (p: int) = match p with
    0 -> []
    | x -> 
        if x > 0 then
            w@(puissance w (p-1))
        else
            raise Erreur;;

(* Tests *)
(puissance w 2) ;;
(* - : char list = ['a'; 'b'; 'b'; 'a'; 'a'; 'b'; 'b'; 'a'] *)
(puissance w 0) ;;
(* - : char list = [] *)
(puissance w (-4)) ;;
(* Exception: Erreur. *)

(* Retourne le reste d'un mot après suppression du prefixe ou lève une Erreur *)
(* val prefixe_reste : 'a list * 'a list -> 'a list = <fun> *)
let rec prefixe_reste ((v: 'a list), (w: 'a list)) = match (v,w) with
    (a::l1, b::l2) -> if a = b then (prefixe_reste(l1, l2)) else raise Erreur
    | (a::l1, _) -> raise Erreur
    | (_, b) -> b;;

(* Tests *)
prefixe_reste (w, ['a' ;'b' ;'b' ;'a' ;'a']);;
(* - : char list = ['a'] *)
prefixe_reste (w, ['a' ;'b' ;'b' ;'b' ;'a']);;
(* Exception: Erreur. *)
prefixe_reste (w, ['a' ;'b']);;
(* Exception: Erreur. *)

let rec est_puiss ((v: 'a list), (w: 'a list)) = match (v,w) with ;;
(* Tests *)
est_puiss (w , ['a' ;'b' ;'b' ;'a' ;'a']);;
est_puiss (w, puissance w 3);;

(* 4 [F] - Mots conjugués, préfixe, le retour des héros *)

(* Langages *)
(* 1 - Les exemples *)

(* 2 - Opérations sur les langages *)

(* [F] Expressions régulières *)

(* 1 - Expressions régulières binaires... *)

(* 2 - * ... ou pas ! *)