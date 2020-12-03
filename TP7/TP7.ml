(* Compte rendu TP7 *)

(* 1 - Les couleurs *)

type couleur  = Blanc | Noir | Rouge | Vert | Bleu | Jaune | Cyan | Magenta;;

(* Retourne si c'est une couleur *)
(* est_couleur : couleur -> bool = <fun>*)
let est_couleur = fun
    |(Noir | Blanc) -> false
    |_ -> true;;

(* Tests *)
est_couleur(Noir);;
est_couleur(Blanc);;
est_couleur(Bleu);;

(* Retourne le complementaire d'une couleur *)
(* complementaire : couleur -> couleur = <fun> *)
let complementaire = fun
    |Rouge -> Vert
    |Vert -> Rouge
    |Jaune -> Bleu
    |Bleu -> Jaune
    |Cyan -> Magenta
    |Magenta -> Cyan
    |_ -> failwith("not a color");;

(* Tests *)
complementaire(Rouge);;
(* - : couleur = Vert *)

(* 2 - Nombres entiers, réels et complexes *)
 
type nombreNR = N of int | R of float;;

(* Retourne la somme de deux nombreNR *)
(* somme : nombreNR * nombreNR -> nombreNR = <fun> *)
let somme = fun
    |(N a, N b) -> N(a + b)
    |(N a, R b) -> R(float_of_int(a) +. b)
    |(R a, N b) -> R(a +. float_of_int(b))
    |(R a, R b) -> R(a +. b);;

(* Retourne le produit de deux nombreNR *)
(* produit : nombreNR * nombreNR -> nombreNR = <fun> *)
let produit = fun
    |(N a, N b) -> N(a * b)
    |(N a, R b) -> R(float_of_int(a) *. b)
    |(R a, N b) -> R(a *. float_of_int(b))
    |(R a, R b) -> R(a *. b);;

type nombreRC = R of float | C of float * float;;

(* Retourne la somme de deux nombreRC *)
(* somme : nombreRC * nombreRC -> nombreRC = <fun> *)
let somme = fun
    |(R a, R b) -> R(a +. b)
    |(R a, C b) -> let (i,r) = b in C(i, r +. a)
    |(C a, R b) -> let (i,r) = a in C(i, r +. b)
    |(C a, C b) -> let (i1,r1) = a and (i2,r2) = b in C(i1 +. i2, r1 +.r2);;

(* Retourne le produit de deux nombreNR *)
(* produit : nombreRC * nombreRC -> nombreRC = <fun> *)
let produit = fun
    |(R a, R b) -> R(a *. b)
    |(R a, C b) -> let (i,r) = b in C(i, r *. a)
    |(C a, R b) -> let (i,r) = a in C(i, r *. b)
    |(C a, C b) -> let (i1,r1) = a and (i2,r2) = b in C(i1 *. i2, r1 *.r2);;

(* 3 - Un premier type somme récursif : Les expressions logiques *)

type exprLogique =
    Vrai | Faux
    | Non of exprLogique
    | Et of exprLogique * exprLogique
    | Ou of exprLogique * exprLogique;;

let exp = Ou(Et(Vrai, Vrai), Et(Non(Ou(Faux, Non(Vrai))), Vrai));;

(* Inversse chaque valeur de l'expression *)
(* echange : exprLogique -> exprLogique = <fun> *)
let rec echange = fun
    |Vrai -> Faux
    |Faux -> Vrai
    |(Et(a,b)) -> Et(echange(a),echange(b))
    |(Ou(a,b)) -> Ou(echange(a),echange(b)) 
    |(Non(a)) -> Non(echange(a));;

(* Tests *)
echange(exp);;
(* - : exprLogique = Ou (Et (Faux, Faux), Et (Non (Ou (Vrai, Non Faux)), Faux)) *)

(* Evalue une expression booleene *)
(* evalue : exprLogique -> bool = <fun> *)
let rec evalue = fun 
    |Vrai -> true
    |Faux -> false
    |(Non(e)) -> not evalue(e)
    |(Ou(a,b)) -> evalue(a) or evalue(b)
    |(Et(a,b)) -> evalue(a) & evalue(b);;


(* 4 - Les arbres d’entiers *)

type arbre = 
    |Feuille of int
    | Noeud of int*arbre*arbre;;

let t= Noeud(4,Noeud(2,Noeud(3,Feuille 0, Feuille 1),Feuille 2),Noeud(1,Feuille 2,Noeud(3, Feuille 1, Feuille 2)));;

let max = fun (x,y) -> if x > y then x else y;;

(* Retourne la profondeur d'un arbre *)
(* profondeur : arbre -> int = <fun> *)
let rec profondeur = fun
    |(Feuille _) -> 0
    |(Noeud (_, a, b)) -> 1 + max(profondeur(a),profondeur(b));;

profondeur(t);;

(* Retourne un arbre complet de profondeur n et de valeur p *)
(* complet : int * int -> arbre = <fun> *)
let rec complet = fun
    | (0,p) -> Feuille p
    | (n,p) ->  let cp = complet(n-1,p) in Noeud(p,cp,cp);;

(* Liste les valeurs des feuilles et des noeuds d'un arbre *)
(* liste : arbre -> int list = <fun> *)
let rec liste = fun
    | (Feuille f) -> [f]
    | (Noeud (a, b, c)) -> [a]@liste(b)@liste(c);;

(* 5 - Les expressions arithmétiques *)

type expArithm = Nb of int | Oper of char * expArithm * expArithm;;

let exp = Oper(`+`,Oper(`*`,Oper(`+`,Nb 0, Nb 1),Nb 2),Oper(`/`,Nb 2,Oper(`-`,Nb 1,Nb 2)));;

(* Retourne le nombre d'operation *)
(* nbOPer : expArithm -> int = <fun> *)
let rec nbOPer = fun
    | (Nb n) -> 0
    | (Oper(a,b,c)) -> 1 + nbOPer(b) + nbOPer(c);;

nbOPer(exp);;

(* Evalue une expression *)
(* evalue : expArithm -> int = <fun> *)
let rec evalue = fun
    | (Nb n) -> n
    | (Oper(a,b,c)) ->
        match a with
            | `+` -> evalue(b) + evalue(c)
            | `*` -> evalue(b) * evalue(c)
            | `-` -> evalue(b) - evalue(c)
            | `/` -> evalue(b) / evalue(c);;

(* 6 - Les mobiles *)

type objet = Chat | Clown | Mouton;;
type mobile = Feuille of objet | Noeud of (int * int * mobile * mobile);;

(* Retourne le poid d'un objet *)
(* poids_f : objet -> int = <fun> *)
let poids_f = fun
    | Chat -> 1
    | Clown -> 3
    | Mouton -> 2;;

(* Retourne un mobile simple *)
(* fait_mob_simple : int * objet -> mobile = <fun> *)
let fait_mob_simple = fun
    (a,b) -> Noeud(a, a, Feuille b, Feuille b);;

(* Retourne le poids total des objets d'un mobile *)
(* poids_m : mobile -> int = <fun> *)
let rec poids_m = fun
    | (Noeud(_,_,c,d)) -> poids_m(c)+ poids_m(d)
    | (Feuille f) -> poids_f(f);;

(* Echange deux objets entre eux dans le mobile *)
(* echanger_m : objet * objet * mobile -> mobile = <fun> *)
let rec echanger_m = fun
    | (o1,o2,Noeud(a,b,c,d)) -> Noeud(a,b,echanger_m(o1,o2,c),echanger_m(o1,o2,d))
    | (o1,o2,Feuille f) -> 
        if poids_f(f) = poids_f(o1) then
            Feuille o2
        else
            if poids_f(f) = poids_f(o2) then
                Feuille o1
            else
                Feuille f;;

let m2 = Noeud(2,4, Feuille(Mouton),Noeud (3,4, Feuille (Chat), Feuille (Clown)));;             

echanger_m(Chat, Mouton, m2);;

(* Retourne la longueur totale des tiges d'un mobile *)
(* tiges_m : mobile -> int = <fun> *)
let rec tiges_m = fun
    | (Noeud(a,b,c,d)) -> a + b + tiges_m(c) + tiges_m(d)
    | (Feuille(f)) -> 0;;

(* Retourne la liste des longueurs des tiges *)
(* lg_tiges_m : mobile -> int list = <fun> *)
let rec lg_tiges_m = fun
    | (Noeud(a,b,c,d)) -> [a+b]@lg_tiges_m(c)@lg_tiges_m(d)
    | (Feuille(_)) -> [];;

(* Retourne la liste des objets d'un mobile *)
(* objets_m : mobile -> objet list = <fun> *)
let rec objets_m = fun
    | (Noeud(_,_,c,d)) -> objets_m(c)@objets_m(d)
    | (Feuille(f)) -> [f];;
