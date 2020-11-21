let main = [("trefle",8) ;("pique",7) ;("coeur",11) ;("trefle",11) ;("coeur",7) ;
("carreau",10);("pique",10) ;("pique",11) ];;

 let jeucomplet=[("pique",7) ;("pique",8) ;("pique",9) ;("pique",10) ;("pique",11) ;
("pique",12) ;("pique",13) ;("pique",14) ; ("coeur",7) ;("coeur",8) ;("coeur",9) ;
("coeur",10) ;("coeur",11) ;("coeur",12) ;("coeur",13) ;("coeur",14) ; ("carreau",7) ;
("carreau",8) ;("carreau",9) ;("carreau",10) ;("carreau",11) ;("carreau",12) ;("carreau",13)
;("carreau",14) ; ("trefle",7) ;("trefle",8) ;("trefle",9) ;("trefle",10) ;("trefle",11) ;
("trefle",12) ;("trefle",13) ;("trefle",14) ] ;; 

(* Retourne la longueur d'une liste *)
(* long : 'a list -> int = <fun> *)
let rec long = fun 
    (a::l) -> 1 + long(l)
    | _ -> 0;;

(* Tests *)
long(main);;
(* - : int = 8 *)

(* Retourne le nombre d'occurence d'une couleur *)
(* nbCoul : 'a * ('a * 'b) list -> int = <fun> *)
let rec nbCoul = fun
    (coul,(coulC,_)::l) ->
        if coul = coulC then
            1 + nbCoul(coul,l)
        else
            nbCoul(coul,l)
    | (_,_) -> 0;;

(* Tests *)
nbCoul("pique",main);;
(* - : int = 3 *)

(* Retourne la liste inverse *)
(* inverse : 'a list -> 'a list = <fun> *)
let rec retourne = fun
    (a::l) -> retourne(l)@[a]
    | (_) -> [];;

(* Tests *)
retourne([3;2;1;0]);;
(* - : int list = [0; 1; 2; 3] *)

(* Retourne un quadruplet de liste a partir d'une liste *)
(* distribue : 'a list -> 'a list * 'a list * 'a list * 'a list = <fun> *)
let rec distribue = fun
    (a::b::c::d::l) -> 
        let (l1,l2,l3,l4) = distribue(l) in
            (a::l1,b::l2,c::l3,d::l4)
    |(a::b::c::_) -> ([a],[b],[c],[])
    |(a::b::_) -> ([a],[b],[],[])
    |(a::_) -> ([a],[],[],[])
    | _ -> ([],[],[],[]);;

(* Tests *)
distribue(main);;
(*  (string * int) list =
["trefle", 8; "coeur", 7], ["pique", 7; "carreau", 10],
 ["coeur", 11; "pique", 10], ["trefle", 11; "pique", 11] *)

(* Range une carte dans le bon paquet *)
(* range :
 (string * 'a) * 'a list * 'a list * 'a list * 'a list ->
 'a list * 'a list * 'a list * 'a list = <fun> *)
let range = fun
    ((coul,val),piL,coL,caL,trL) -> 
        match coul with
            "pique" -> (val::piL,coL,caL,trL)
            |"coeur" -> (piL,val::coL,caL,trL)
            |"carreau" -> (piL,val::coL,caL,trL)
            |"trefle" -> (piL,coL,caL,val::trL);;

(* Tests *)
range(("pique",7),[8;12],[],[7;8;9],[14]);;
(* #- : int list * int list * int list * int list =
 [7; 8; 12], [], [7; 8; 9], [14] *)


(* Retourne la liste des valeurs des cartes appartenant aux 4 couleurs *)
(* triCoul : (string * 'a) list -> 'a list * 'a list * 'a list * 'a list = <fun> *)
let rec triCoul = fun
    (a::l) -> 
        let (piL,coL,caL,trL) = triCoul(l) in 
            range(a,piL,coL,caL,trL)
    |_ -> ([],[],[],[]);;

(* Tests *)
triCoul(main);;
(* - : int list * int list * int list * int list =
 [7; 10; 11], [11; 7], [10], [8; 11] *)


(* Insère une entier dans une liste en préservant l'ordre *)
(* insere : 'a * 'a list -> 'a list = <fun> *)
let rec insere = fun 
    (entier,(a::l)) ->
        if entier <= a then
            entier::a::l
        else
            a::insere(entier,l)
    | (entier,_) ->
        [entier];;

(* Tests *)
insere(7,[2;4;6;9;12;14]);;
(* - : int list = [2; 4; 6; 7; 9; 12; 14] *)


(* Retourne une liste d'entier trié par insertion *)
(* tri : 'a list -> 'a list = <fun> *)
let rec tri = fun
    (a::l) -> insere(a,tri(l))
    | _ -> [];;

(* Tests *)
tri([1;2;4;3;9;15;0]);;
(* - : int list = [0; 1; 2; 3; 4; 9; 15] *)


(* Donne pour chaques couleurs la liste des valeurs triées *)
(* triCartes : (string * 'a) list -> 'a list * 'a list * 'a list * 'a list = <fun> *)
let triCartes = fun
    l -> 
        let (l1,l2,l3,l4) = triCoul(l) in
            (tri(l1),tri(l2),tri(l3),tri(l4));;

(* Tests *)
triCartes(main);;
(* - : int list * int list * int list * int list = [7; 10; 11], [7; 11], [10], [8; 11] *)

(* Fait passer les p premier carte du premier jeu dans l'autre jeu *)
(* passe : int * 'a list * 'a list -> 'a list * 'a list = <fun> *)
let rec passe = fun
    (0,l1,l2) -> (l1,l2)
    | (p,a::l1,l2) -> passe(p-1,l1,a::l2);;

(* Tests *)
passe(2,[1;2;3;4],[5;6;7;8]);;
(* - : int list * int list = [3; 4], [2; 1; 5; 6; 7; 8] *)

let coupe = fun l -> passe(long(l)/2,l,[]);;

(* Tests *)
coupe([1;2;3]);;

let rec riffle = fun
    (a::l1,b::l2) -> a::b::riffle(l1,l2)
    | (a::l1,l2) -> a::l2
    | (l1,b::l2) -> b::l1
    | _ -> [];;

(* Tests *)

riffle([1;2;3;4],[5;6;7;8;9]);;

let rec shuffle = fun
    (0,l1) -> l1
    | (n,l1) -> shuffle(n-1,riffle(coupe(l1)));;

shuffle(5,jeucomplet);;
    



