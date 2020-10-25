(* Compte rendu TP2 *)

(* 1. Quelques fonctions sur les nombres *)

(* 1 *)
(* Retourne la troncature d'un flottant *)
(* troncature : float -> int = <fun> *)
let troncature = fun x -> int_of_float(x);;

(* Tests *)
troncature(3.3);;
(* - : int = 3 *)
troncature(-3.3);;
(* - : int = -3 *)
troncature(3.0);;
(* - : int = 3 *)

(* 2 *)
(* Retourne la partie décimale d'un flottant *)
(* decimales : float -> float = <fun> *)
let decimales = fun x -> x -. float_of_int(troncature(x));;

(* Tests *)
decimales(3.3);;
(* - : float = 0.3 *)
decimales(3.0);;
(* - : float = 0.0 *)
decimales(-3.3);;
(* - : float = -0.3 *)

(* 3 *)
(* Retourne le plus grand entier inférieur ou égal à un flottant *)
(* partie_entiere : float -> int = <fun> *)
let partie_entiere = fun x -> 
    if x -. float_of_int(troncature(x)) >= 0.0 then
        troncature(x)     
    else
        troncature(x) - 1 ;;

(* Tests *)
partie_entiere(3.3);;
(* - : int = 3 *)
partie_entiere(-3.3);;
(* (* - : int = -4 *) *)

(* 4 *)
(* Retourne l'entier le plus proche d'un flottant *)
(* plus_proche_entier : float -> int = <fun> *)
let plus_proche_entier = fun x -> partie_entiere(x +. 0.5);;

(* Tests *)
plus_proche_entier(5.5);;
(* - : int = 6 *)
plus_proche_entier(-5.2);;
(* - : int = -5 *)
plus_proche_entier(5.8);;
(* - : int = 6 *)

(* 5 *)
(* Retourne l'arondie à deux décimales après le virgule d'un flottant *)
(* arrondi : float -> float = <fun> *)
let arrondi = fun x ->
    float_of_int(plus_proche_entier(x *. 100.0)) /. 100.0;;

    let arrondi x = float_of_int(plus_proche_entier(x *. 100.0)) /. 100.0;;

(* Tests *)
arrondi(52.6543);;
(* - : float = 52.65 *)
arrondi(52.6550);;
(* - : float = 52.66 *)
arrondi(52.6563);;
(* - : float = 52.66 *)

(* 2. Fonction de conversion francs-euros *)

(*  Retourne la valeur en euros de x francs *)
(* francs_en_euros : float -> float = <fun> *)
let francs_en_euros = fun x -> arrondi(x /. 6.55957);;

(* Tests *)
francs_en_euros(300.0);;
(* - : float = 45.73 *)
francs_en_euros(6.55957);;
(* - : float = 1.0 *)

(* 3. Quelle heure est-il *)

(* Retourne le nombre de minute *)
(* minute : float -> int = <fun> *)
let minute = fun x -> troncature(decimales(x) *. 100.0);;

(* Retourne le nombre d'heure *)
(* heure : float -> int = <fun> *)
let heure = fun x -> troncature(x);;

(* Retourne une chaine représentant une heure *)
(* quelle_heure_est_il : float -> string = <fun> *)
let quelle_heure_est_il = fun x -> 
    let minute = minute(x) and heure = heure(x) in
        if(minute = 0) then
            if(heure = 12) then 
                "Il est midi pile"
            else
                if(heure = 0) then
                    "Il est minuit pile"
                else
                    "Il est " ^ string_of_int(heure) ^ " heure pile"
        else
            if(heure = 12) then
                "Il est midi " ^ string_of_int(minute)
            else 
                if (heure = 0) then
                    "Il est minuit " ^ string_of_int(minute)
                else
                    "Il est " ^ string_of_int(heure) ^ " heure " ^ string_of_int(minute) ;;

(* Plus optimisé *)
let quelle_heure_est_il = fun x ->
    let minute = minute(x) and heure = heure(x) in
        let partie_horaire = 
            if(heure = 0) then
                "minuit "
            else
                if(heure = 12) then
                    "midi "
                else
                    string_of_int(heure) ^ " heure "
        and partie_minutaire =
            if(minute = 0) then
                "pile"
            else
                string_of_int(minute) 
        in
            "Il est " ^ partie_horaire ^ partie_minutaire;;

(* Tests *)

quelle_heure_est_il(12.0);;
(* - : string = "Il est midi pile" *)
quelle_heure_est_il(0.0);;
(* - : string = "Il est minuit pile" *)
quelle_heure_est_il(0.2);;
(* - : string = "Il est minuit 20" *)
quelle_heure_est_il(8.3);;
(* - : string = "Il est 8 heure 30" *)

(* Fonctions sur un produit cartésien *)

(* Retourne la valeur qui associe la troncature absolue et le maximum à un réel*)
(* - : int * int * float -> float = <fun> *)
let reel  = fun (x,y,z) ->
    if(x > y) then
        float_of_int(x) +. abs_float(decimales(z))
    else 
        float_of_int(y) +. abs_float(decimales(z));;

(* Tests *)
reel(4,5,-4.123);;
(* - : float = 5.123 *)
reel(7,5,4.123);;
(* - : float = 7.123 *)


(* 5. Ecrire les fonctions entrières suivantes *)

(* Retourne le dernier chiffre d'un entier *)
(* - : int -> int = <fun> *)
let chiffre = fun x -> abs(x mod 10);;

(* Tests *)
chiffre(-11);;
(* - : int = 1 *)
chiffre(11);;
(* - : int = 1 *)

(* Retourne un entier obtenu en remplacent le dernier chiffre de x par celui de p  *)
let echange = fun (x,y) ->
    let result = x - chiffre(x) in
        result + chiffre(y);;

(* 6. Fonction booléennes *)
let meme_valeur = fun (x,y,z) -> x = y & x = z ;;

let different_de_z = fun (x,y,z) ->
    let result = x = y in
        result & result != (x = z) ;;

let y_entre_x_et_z = fun (x,y,z) -> y > x & y < z;;

let deux_identiques = fun (x,y,z) -> x = y or x = z or y = z;;

let strictement_deux_identiques = fun (x,y,z) -> 
    (x = y & x != z) or (x = z & x != y) or (y = z & y != x) ;;

let deux_identiques_max = fun (x,y,z) ->
    (x != y & x != z & y != z) or ( x = y & x != z & y != z) or
    ( x != y & x = z & y != z) or ( x != y & x != z & y = z) ;;

(* 7. Résolution de l'équation de second degré *)

(* Retourne la valeur delta *)
(* delta : int * int * int -> int = <fun> *)
let delta = fun (a,b,c) -> b*b - 4*a*c;;

(* Retourne le nombre de solution d'une equation du second degre *)
(* nb_sol : int * int * int -> int = <fun> *)
let nb_sol = fun (a,b,c) ->
    let delta = delta(a,b,c) in
        if(delta = 0) then
            1
        else if (delta > 0) then
            2
        else
            0
    ;;

(* Tests *)
nb_sol(4,2,0);;
(* - : int = 2 *)
nb_sol(0,0,0);;
(* - : int = 1 *)
nb_sol(40,0,3000);;
(* - : int = 0 *)

(* 8. Evaluation d'un appel de fonction *)

(* A. *)
(* Env1 = [] *)
(* Env2 = [(x,3) >< Env1] *)
(* Env3 = [(y,4) >< Env2]*)
(* Env4 = [(f,<x,x -> 3*x + y*2, Env3>) >< Env3] *)
(* EnvT1 = [(x,2)] *)
(* Env5 = [(y,4) >< Env4] *)

(* EnvT1 = [(x,4)] *)
(* EnvT1 = [(x,3)] *)

(* B. *)
(* Env1 = [] *)
(* Env2 = [(f,<a, let b = 2 in a * b, Env1>) >< Env1] *)
(* Env3 = [(b,3), (x,2) >< Env2] *)
(* EnvT1 = [(a,2)] *)
(* EnvT2 = [(b,2)] *)
(* EnvT1 = [(a,2)] *)
(* EnvT2 = [(b,3)] *)

(* C. *)
(* Env0 = [] *)
(* Env1 = [(a,2) >< Env0] *)
(* EnvT1 = [(b,12)] *)
(* Env2 = [(f,<x,2*x +a>)] *)
(* EnvT1 = [(x,2)] *)






(* On vois ici que l'évaluation des ET et des OU est "paresseuse" car il n'y a pas d'erreur
   le ET break dès que FAUX et le OU break dès que VRAI *)
let divisable = fun (x,y) ->
    d <> 0 & x mod d = 0;;  




