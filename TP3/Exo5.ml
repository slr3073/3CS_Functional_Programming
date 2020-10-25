(* 5. Un calendrier perpétuel *)

(* 1 *)
(* Retourne la valeur de la formule *)
(* formule : int * int * int * int -> int  *)
let formule = fun (j,m,p,s) -> j + (48*m - 1)/5 + p/4 + p + s/4 - 2*s;;

(* Tests *)
formule(1,2,8,6);;
(* - : int = 19 *)

(* 2 *)
(* Retourne le quotient et le reste d'une division par 100 *)
(* decoupe : int -> int * int *)
let decoupe = fun x -> (x / 100, x mod 100);;

(* Tests *)
decoupe(2014);;
(* - : int * int = 20, 14 *)

(* 3 *)
(* Retourne un couple mois-année antérieur à deux
mois au couple mois-année passé en argument*)
(* deuxMoisAvant : int * int -> int * int *)
let deuxMoisAvant = fun
    (0,a) -> (10,a-1)
    | (1,a) -> (11,a-1)
    | (2,a) -> (12,a-1)
    | (m,a) -> 
        if(m < 0 or m > 12) then
            failwith("Date inconnue")
        else
            (m-2,a);;
            
(* Tests *)
deuxMoisAvant(0,2012);;
(* - : int * int = 10, 2011 *)
deuxMoisAvant(1,2012);;
(* - : int * int = 11, 2011 *)
deuxMoisAvant(2,2012);;
(* - : int * int = 12, 2011 *)
deuxMoisAvant(8,2012);;
(* - : int * int = 6, 2012 *)
deuxMoisAvant(13,2012);;
(* Exception non rattrapée: Failure "Date inconnue" *)

(* 4 *)
(* Retourne le jour de la semaine *)
(* leJour : int -> string *)
let leJour = fun 
    0 -> "Dimanche"
    | 1 -> "Lundi"
    | 2 -> "Mardi"
    | 3 -> "Mercredi"
    | 4 -> "Jeudi"
    | 5 -> "Vendredi"
    | 6 -> "Samedi" ;;

(* Tests *)
leJour 0;;
(* - : string = "Dimanche" *)
leJour 6;;
(* - : string = "Samedi" *)

(* 5 *)
(* Retourne le reste d'une division par 7 *)
(* modulo7 : int -> int *)
let modulo7 = fun x -> 
    if(x > 0) then
        x mod 7
    else
       (x mod 7) + 7;;

(* Tests *)
modulo7 7;;
(* - : int = 0 *)
modulo7 55;;
(* - : int = 6 *)
modulo7 (-4);;
(* - : int = 3 *)

(* 6 *)
(* Retourne le jour d'une date *)
(* quelJour : int * int * int -> string = <fun> *)
let quelJour = fun (j,m,a) ->
    let (mPre,aPre) = deuxMoisAvant(m,a) in
        let (s, p) = decoupe(aPre) in
            let k = formule(j,mPre,p,s) in
                leJour(modulo7(k));; 

(* Tests *)
quelJour(29,9,2020);;
(* - : string = "Mardi" *)
quelJour(25,1,2000);;
(* - : string = "Mardi" *)
quelJour(26,1,2000);;
(* - : string = "Mercredi" *)