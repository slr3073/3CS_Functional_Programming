(* 2 - Chiffres romains *)

(* 1 *)

let chiffre = fun
    `I` -> 1
    | `X` -> 10
    | `V` -> 5
    | _ -> failwith("n'est pas un chiffre Romain");;

(* Tests *)
chiffre(`I`);;
(* - : int = 1 *)
chiffre(`V`);;
(* - : int = 5 *)
chiffre(`X`);;
(* - : int = 10 *)
chiffre(`R`);;
(* Exception non rattrapée: Failure "n'est pas un chiffre Romain" *)


(* Retourne la valeur d'un nombre romain *)
(* rom1 : string -> int = <fun> *)
let rec rom1 = fun
    "" -> 0
    | s -> chiffre(tetec(s)) + rom1(reste(s));;

(* Tests *)
rom1("XXVIII");;
(* - : int = 28 *)
rom1("XX");;
(* - : int = 20 *)
rom1("");;
(* - : int = 0 *)

(* 2 *)

(* Retourne la valuer Ic d'un chiffre c en chiffre Romain *)
(*  valeurI : char -> int *)
let valeurI = fun
    `X` -> 9
    | `V` -> 4
    | c -> chiffre(c);; 

(* Tests *)
valeurI(`I`);;
(* - : int = 1 *)
valeurI(`V`);;
(* - : int = 4 *)
valeurI(`X`);;
(* - : int = 9 *)
valeurI(`R`);;
(* Exception non rattrapée: Failure "n'est pas un chiffre Romain" *)

let rec romain = fun
    "" -> 0
    | s -> 
        if(tetec(s) = `I` & longChaine(s) >= 2 & niemeCar(2,s) <> `I`) then
            valeurI(niemeCar(2,s)) + romain(reste(reste(s)))
        else
            chiffre(tetec(s)) + romain(reste(s));;


(* Tests *)
romain("XXIVI");;
(* - : int = 25 *)
romain("XXI");;
(* - : int = 21 *)
romain("XXII");;
(* - : int = 22 *)