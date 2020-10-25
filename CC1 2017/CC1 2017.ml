(* 1. Seconde après seconde *)

(* Retourne la valeure horaire d'un nombre de seconde x *)
(* secEnHeure : int -> int * int * int = <fun> *)
let secEnHeure = fun 
    s -> 
        let nbH = s / 3600 in
            let nbM = (s - nbH * 3600) / 60 in
                (nbH, nbM, s - nbH * 3600 - nbM * 60);;

(* Tests *)
secEnHeure(3659);;
secEnHeure(1000);;
secEnHeure(100000);;

(* Année après année *)

(* Retourne si une année x est bissextile *)
(* bissextile : int -> bool = <fun> *)
let bissextile = fun x -> (x mod 4 = 0 & x mod 100 <> 0) or x mod 400 = 0

(* Tests *)
bissextile(2016);;
(* - : bool = true *)
bissextile(2017);;
(* - : bool = false *)

(* Retourne la prochaine année bissextile ou x si elle est bissextile *)
(* nextBissextile : int -> int = <fun> *)
let nextBissextile = fun
    x -> 
        if(bissextile(x)) then
            x
        else if(bissextile(x + 1)) then
            x + 1
        else if(bissextile(x + 2)) then
            x + 2
        else
            x + 3;;

(* Tests *)
nextBissextile(2016);;
(* - : int = 2016 *)
nextBissextile(2017);;
(* - : int = 2020 *)
nextBissextile(2018);;
(* - : int = 2020 *)
nextBissextile(2019);;
(* - : int = 2020 *)

(* Jour après jour *)

(* 1 *)
let nbJoursParMois = fun
    2 -> 28
    | 4 -> 30 
    | 6 -> 30
    | 9 -> 30
    | 10 -> 30
    | _ -> 31 ;;
    
(* Tests *)
nbJoursParMois(4);;
(* - : int = 30 *)
nbJoursParMois(7);;
(* - : int = 31 *)
nbJoursParMois(2);;
(* - : int = 28 *)

(* 2 *)
let dateValide = fun (j,m) -> m > 0 & m < 13 & j < nbJoursParMois(m) & j > 0;;

(* Tests *)
dateValide(15,6);;
(* - : bool = true *)
dateValide(3,14);;
(* - : bool = false *)
dateValide(30,2);;
(* - : bool = false *)
dateValide(31,-2);;
(* - : bool = false *)
dateValide(0,1);;
(* - : bool = false *)

(* 3 *)
let datePasse = fun 
    (j,10) -> 
        if(dateValide(j,10)) then
            j < 16
        else
            failwith("Date invalide")
    | (j,m) ->
        if dateValide(j,m) then
            m < 10
        else
            failwith("Date invalide");;

(* Tests *)
datePasse(16,10);;
(* - : bool = false *)
datePasse(15,9);;
(* - : bool = true *)
datePasse(20,10);;
(* - : bool = false *)
datePasse(30,2);;
(* Exception non rattrapée: Failure "Date invalide" *)

(* 4 *)

let age = fun
    (j, m, a) ->
        if(dateValide(j,m)) then
            if(datePasse(j,m)) then
                2017 - a
            else
                2016 - a
        else
            failwith("Date invalide");;

(* Tests *)
age(25,1,2000);;
(* - : int = 17 *)
age(30,2,2000);;
(* Exception non rattrapée: Failure "Date invalide" *)
age(20,7,2000);;
(* - : int = 17 *)
age(20,10,2000);;
(* - : int = 16 *)

(* 5 *)

let rec nbJaux = fun 
    (_,0) -> 0
    | (j,m) -> j + nbJaux(nbJoursParMois(m-1),m-1);;

(* Tests *)
nbJaux(16,10);;
nbJaux(31,12);;
nbJaux(2,1);;

let nbJours = fun 
    (j,m) -> 
        if(dateValide(j,m)) then
            nbJaux(j,m)
        else
            failwith("Date invalide");;

(* Tests *)
nbJours(16,10);;
(* - : int = 289 *)
nbJours(30,2);;
(* Exception non rattrapée: Failure "Date invalide" *)


