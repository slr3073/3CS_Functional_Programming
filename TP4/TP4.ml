(* Compte rendu du TP4 : Récursivité sur les nombres *)

(* 1. Puissance Naive *)

(* 1 *)

(* Retourne le puissance d'un entier *)
(* puissance : int * int -> int *)
let rec puissance = fun 
    (x,0) -> 1
    | (x,y) -> puissance(x,y-1) * x;;

(* Tests *)
puissance(3,3);;
(* - : int = 27 *)
puissance(0,3);;
(* - : int = 0 *)

(* 2 *)

(* pour puissance(3,4) il y a 4 appels récursifs *)
(* pour puissance(x,n) il y a n appels récursifs *)

(* 2. Quelques fonctions sur les nombres *)

(* Retourne le nombre construit en répétant n fois le chiffre c *)
(* repet : int * int -> int  *)
let rec repet = fun
    (x,0) -> 0
    | (x,y) -> repet(x,y-1) * 10 + x;;

(* Tests *)
repet(5,4);;
(* - : int = 5555 *)
repet(5,0);;
(* - : int = 0 *)
    
(* Retourne true si le nombre n est constitué seulement d'un chiffre c *)
(* unChiffre(n,c) : int * int -> bool *)
let rec unChiffre = fun
    (0,x) -> true
    | (x,y) -> x mod 10 = y & unChiffre( (x - x mod 10) / 10, y );;

(* Tests *)
unChiffre(575,5);;
unChiffre(555,5);;
unChiffre(0,5);;
unChiffre(5,0);;

(* Retourne le pgcd de deux entier *)
(* pgd : int * int -> int = <fun> *)
let rec pgd = fun
    (x,y) -> if(x mod y = 0) then
            y
        else
            pgcd(x,y-1);;

(* Tests *)
pgd(18,12);;
(* - : int = 9 *)
pgd(21,7);;
(* - : int = 7 *)
pgd(13,68);;
(* - : int = 13 *)

(* Retourne true ssi n contient un nombre pair de chiffre *)
(* nbPairChiffre : int -> bool = <fun> *)

let rec nbPairChiffre = fun
    x -> if (x / 100) = 0 then
        x / 10 <> 0
    else
        nbPairChiffre(x / 100);;

(* Tests *)
nbPairChiffre(487);;
(* - : bool = false *)
nbPairChiffre(4876);;
(* - : bool = true *)
nbPairChiffre(0);;
(* - : bool = false *)

(* 3 - Multiplication égypsienne *)
let rec multiplication_egpysienne = fun 
    (2,p) -> p + p
    |(n,p) -> 
        if(n mod 2 = 0) then
            multiplication_egpysienne(n/2, p+p)
        else
            multiplication_egpysienne(n - 1, p) + p;;
    
(* Tests *)
multiplication_egpysienne(2,3);;
(* - : int = 6 *)
multiplication_egpysienne(7,4);;
(* - : int = 28 *)
multiplication_egpysienne(9,3);;
(* - : int = 27 *)
multiplication_egpysienne(59,17);;
(* - : int = 1003 *)

(*#trace("multiplication_egpysienne");;
La fonction multiplication_egpysienne est dorénavant tracée.
#multiplication_egpysienne(59,17);;
multiplication_egpysienne <-- 59, 17
multiplication_egpysienne <-- 58, 17
multiplication_egpysienne <-- 29, 34
multiplication_egpysienne <-- 28, 34
multiplication_egpysienne <-- 14, 68
multiplication_egpysienne <-- 7, 136
multiplication_egpysienne <-- 6, 136
multiplication_egpysienne <-- 3, 272
multiplication_egpysienne <-- 2, 272
multiplication_egpysienne --> 544
multiplication_egpysienne --> 816
multiplication_egpysienne --> 816
multiplication_egpysienne --> 952
multiplication_egpysienne --> 952
multiplication_egpysienne --> 952
multiplication_egpysienne --> 986
multiplication_egpysienne --> 986
multiplication_egpysienne --> 1003 *)

(* 4 - Somme de chiffres *)

(* Retourne la somme des chiffres composant le nombre x *)
(* somme_chiffre : int -> int = <fun> *)
let rec s_chif = fun 
    0 -> 0
    | x -> somme_chiffre(x / 10) + x mod 10;;

(* Tests *)

s_chif(1111);;
(* - : int = 4 *)
s_chif(18925);;
(* - : int = 25 *)
s_chif(302091);;
(* - : int = 15 *)
s_chif(0);;
(* - : int = 0 *)

(* Retourne la somme des chiffres composant le nombre x *)
(* somme_chiffre : int -> int = <fun> *)
let som_chif = fun 
    x -> s_chif(s_chif(x));;

(* Test *)

som_chif(18925);;
(* - : int = 7 *)
som_chif(149925);;
(* - : int = 3 *)
            





