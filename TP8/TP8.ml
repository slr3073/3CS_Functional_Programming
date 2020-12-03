(* Compte rendu TP8 *)

(* I : Un peu de géométrie *)

type Point = {abs : float; ord : float};;

let p1 = {abs = 0.0 ; ord = 0.0} and
p2 = {abs = 2.0 ; ord = 0.0} and
p3 = {abs = 1.0 ; ord = 2.0} and
p4 = {abs = 0.0 ; ord = 1.0};;

type Forme = Cercle of Point*float | Polygone of Point list;;

let p = Polygone [p1; p2; p3; p4; p1];;

(* Calcule la distance entre deux points *)
(* dis : Point * Point -> float = <fun> *)
let dis = fun (p1,p2) -> sqrt((p2.abs -. p1.abs)**2.0 +. (p2.ord -. p1.ord)**2.0);;

(* Tests *)
dis(p3,p2);;
(* - : float = 2.2360679775 *)

(* Retourne la longueur d'un ligne brisée *)
(* longueur : Point list -> float = <fun> *)
let rec longueur = fun 
    (a::b::c) -> dis(a,b) +. longueur(b::c)
    | _ -> 0.0;;

(* Tests *)
longueur([p1;p2;p3]);;
(* - : float = 4.2360679775 *)

let rec dernier = fun
    (_::a::l) ->  dernier(a::l)
    | [a] -> a;;


(* Retourne si une liste de point est un polygone *)
(* bonPoly : 'a list -> bool = <fun> *)
let bonPoly = fun
    (a::b::c::d::e::r) -> dernier(a::b::c::d::e::r) = a
    | _ -> false;;


(* Tests *)
bonPoly([p1 ;p2 ;p3 ;p4 ;p1]);;
(* - : bool = true *)
bonPoly ([p1 ;p2 ;p3 ;p4 ;p3]);;
(* - : bool = false *)
bonPoly ([p1 ;p2 ;p1]);;
(* - : bool = false *)

let perimetreCercle = fun
    (Cercle(p,r)) -> 2. *. 3.14 *. r;;

(* Retourne le perimetre d'une figure *)
(*  *)
let perimetre = fun
    (Cercle(p,r)) -> 2. *. 3.14 *. r
    | (Polygone(l)) -> longueur(l);;

(* Tests *)
perimetre p;;
(* - : float = 6.65028153987 *)
perimetre (Polygone [p1 ;p2 ;p3]);;
(* - : float = 4.2360679775 *)
perimetre (Cercle (p1,1.));;
(* - : float = 6.28 *)

(* II : Couleurs *)

(* A : Définition des types *)

type CodeRVB = { R:int; V:int; B:int };;

type Couleur = 
    Rouge | Vert | Bleu
    | RVB of int*int*int
    | Melange of Couleur*Couleur;;

(* B : Fonctions de conversion *)

(* Convertit un CodeRVB en Couleur *)
(* rvbToCoul : CodeRVB -> Couleur = <fun> *)
let rvbToCoul = fun 
    code -> RVB (code.R,code.V,code.B);;

(* Tests *)
rvbToCoul({R = 0; V = 255; B = 100});;

(* Convertis une Couleur en codeRVB*)
(* coulToRVB : Couleur -> CodeRVB = <fun> *)
let rec coulToRVB = fun
    (Rouge) -> {R = 255;V = 0; B = 0}
    | (Vert) -> {R = 0;V = 255; B = 0}
    | (Bleu) -> {R = 0;V = 0; B = 255}
    | (RVB(r,v,b)) -> {R = r;V = v; B = b}
    | (Melange(c1, c2)) -> let rvb1 = coulToRVB(c1) and rvb2 = coulToRVB(c2) in
        {R = (rvb1.R + rvb2.R) / 2;V = (rvb1.V + rvb2.V) / 2; B = (rvb1.B + rvb2.B) / 2};;

(* Tests *)
coulToRVB(Rouge);;
(* - : CodeRVB = {R = 255; V = 0; B = 0} *)
coulToRVB(RVB(100,255,55));;
(* - : CodeRVB = {R = 100; V = 255; B = 55} *)
coulToRVB(Melange(Rouge,RVB(100,255,55)));;
(* - : CodeRVB = {R = 177; V = 127; B = 27} *)

(* Convertis un triplet d'entier en Couleur *)
(* tripletToCoul : int * int * int -> Couleur = <fun>*)
let tripletToCoul=fun (a,b,c) -> RVB(a,b,c);;

(* Tests *)
tripletToCoul(100,255,15);;
(* - : Couleur = RVB (100, 255, 15) *)

(* Convertis une Couleur en triplet d'entier *)
(* coulToTriplet : Couleur -> int * int * int = <fun> *)
let coulToTriplet = fun 
    c -> let code = coulToRVB(c) in
        (code.R,code.V,code.B);;

(* Tests *)
coulToTriplet(Melange(Rouge,Vert));;
(* - : int * int * int = 127, 127, 0 *)

(* C : Visualisation *)

include "couleurs";;

(* Ouvre une fenetre graphique d'une seule Couleur *)
(* peindre : Couleur -> unit = <fun> *)
let peindre = fun c -> bandes([coulToTriplet(c)]);;
        

(* Tests *)
peindre(Rouge);;
peindre(Melange(Rouge,Vert));;


(* Ouvre une fenetre graphique de deux couleurs *)
(*  peindre2 : Couleur * Couleur -> unit = <fun>*)
let peindre2 = fun (c1,c2) -> bandes([coulToTriplet(c1);coulToTriplet(c2)]);;

(* Tests *)
peindre2(Rouge,Vert);;
peindre2(Melange(Rouge,Vert),Vert);;

(* Ouvre une fenetre graphique de deux couleurs *)
(*  drapeau : Couleur * Couleur * Couleur -> unit = <fun>*)
let drapeau = fun (c1,c2,c3) -> bandes([coulToTriplet(c1);coulToTriplet(c2);coulToTriplet(c3)]);;

(* Tests *)
drapeau(Bleu,RVB(255,255,255),Rouge);;

(* D : Création de couleurs *)

let eclaircir = fun
    
