(* Compte rendu CC1 2019 *)

(* 1 - Année bissextile (expressions booléennes) *)

(* Retourne si une année est bissextile *)
(* bissextile : int -> bool = <fun> *)
let bissextile = fun 
    a -> (a mod 4 = 0 & a mod 100 <> 0) or (a mod 400 = 0);;

(* Tests *)
bissextile(2019);;
(* - : bool = false *)
bissextile(2020);;
(* - : bool = true *)
bissextile(3000);;
(* - : bool = false *)
bissextile(2000);;
(* - : bool = true *)

(* 2 - Volume d'un ballon de rugby *)

(* 1 *)

(* Retourne un reel troncaturé à sa première décimale *)
(* trunc1 : float -> float = <fun> *)
let trunc1 = fun n -> float_of_int(int_of_float(n *. 10.0) ) /. 10.;;

(* Tests *)
trunc1(21.35);;
(* - : float = 21.3 *)
trunc1(0.35);;
(* - : float = 0.3 *)
trunc1(22.35555);;
(* - : float = 22.3 *)

(* 2 *)

(* Retourne le volume en dm3 d'une elllipsoide de hauteur h et diametre d *)
(* volume : float * float -> float = <fun> *)
let volume = fun (h,d) -> trunc1((3.14 *. h *. d *. d) /. 6.0);;

(* Tests *)
volume(2.9,1.9);;
(* - : float = 5.4 *)

(* 3 - Demain est un autre jour *)

(* Retourne le jours suivant de la semaine*)
(*  *)
let suivant = fun
    "Lundi" -> "Mardi"
    | "Mardi" -> "Mercredi"
    | "Mercredi" -> "Jeudi"
    | "Jeudi" -> "Vendredi"
    | "Vendredi" -> "Samedi"
    | "Samedi" -> "Dimanche"
    | "Dimanche" -> "Lundi";;

