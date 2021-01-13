(* Compte rendu TP3 *)

(* 1. Première fonction définie par filtrage *)

(* Retourne la chaine zero si l'argument vaux zero, la chaine un si vaux un
 et pair ou impair dans tous les autres cas *)
(* entier : int -> string = <fun> *)

let entier = fun
    0 -> "zero"
    | 1 -> "un"
    | x ->
        if(x mod 2 = 1) then
            "impair"
        else
            "pair";;

(* Tests *)
entier 0;;
(* - : string = "zero" *)
entier 1;;
(* - : string = "un" *)
entier 3;;
(* - : string = "impair" *)
entier 4;;
(* - : string = "pair" *)

(* 2. Un peu de géométrie *)

(* Retourne une chaine en fonction de la position du point *)
(* point : float * float -> string = <fun> *)
let point = fun 
    (0., 0.) -> "Origine"
    | (0., y) -> "Axe des ordonnées"
    | (x, 0.) -> "Axe des abscises"
    | (x,y) ->
        if(x > 0.) then 
            "Point du demi plan x > 0"
        else
            "Point du demi plan x < 0";;

(* Tests *)
point(0.,0.);;
(* - : string = "Origine" *)
point(0.,3.);;
(* - : string = "Axe des ordonnées" *)
point(-4.,0.);;
(* - : string = "Axe des abscises" *)
point(-2.,-2.);;
(* - : string = "Point du demi plan x < 0" *)
point(2.,-2.);;
(* - : string = "Point du demi plan x > 0" *)

(* 3. Opérations *)

(* Réalise une opération *)
(* operation : int * int * char -> int = <fun> *)
let operation = fun
    (x,y,`+`) -> x+y
    | (x,y,`-`) -> x-y
    | (x,y,`*`) -> x*y
    | (x,0,`/`) -> failwith("division par zero")
    | (x,y,`/`) -> x / y
    | _ -> failwith("Arguments invalides");;

(* Tests *)
operation(3,3,`+`);;
(* - : int = 6 *)
operation(3,3,`-`);;
(* - : int = 0 *)
operation(3,3,`*`);;
(* - : int = 9 *)
operation(3,3,`/`);;
(* - : int = 1 *)
operation(3,0,`/`);;
(* Exception non rattrapée: Failure "division par zero" *)
operation(3,0,`m`);;
(* Exception non rattrapée: Failure "Arguments invalides" *)

(* Calcul de TVA *)

(* Retourne le prix TTC d'un produit après application de la TVA *)
(* prixTTC : float * float -> float *)
let prixTTC = fun (x,y) ->
    let TVA = (x /. 100.) *. y in
        x +. TVA;;

(* Tests *)
prixTTC(200.,12.);;
(* - : float = 224.0 *)

(* Retourne le prix unitaire d'un article et le montant de sa TVA *)
(* prix : string -> float * float = <fun> *)
let prix = fun 
    "pain" -> (1.05,5.5)
    | "conserve" -> (3.5,7.)
    | "disque" -> (12.3,18.6)
    | "bijou" -> (356.,33.)
    | x -> failwith ("Article " ^ x ^ " inconnu");;

(* Tests *)
prix "pain";;
(* - : float * float = 1.05, 5.5 *)
prix "conserve";;
(* - : float * float = 3.5, 7.0 *)
prix "esclave";;
(* Exception non rattrapée: Failure "Article inconnu" *)

(* Retourne le prix à payer pour y article(s) x  *)
(* prix : string -> float * float *)

let sommeAPayer  = fun (x,y) -> prixTTC(prix(x)) *. float_of_int(y);;

(* Tests *)
sommeAPayer("pain",3);;
(* - : float = 3.32325 *)
sommeAPayer("conserve",0);;
(* - : float = 0.0 *)
sommeAPayer("bijou",2);;
(* - : float = 946.96 *)

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



        