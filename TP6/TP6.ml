(* Compte rendu TP6 *)

exception TROP_COURT;;
exception LISTE_VIDE ;;

(* 1 - Premières fonctions non récursives sur les listes  *)

(* Retourne le 2eme element d'une liste *)
(* deuxieme : 'a list -> 'a = <fun> *)
let deuxieme = fun (_::b::_) -> b;;

(* Tests *)
deuxieme([1;2;3]);;
(* - : int = 2 *)

(* ---------------------------------------------------------- *)

(* Retourne si une liste à au moins 3 éléments *)
(* auMoinsTrois : 'a list -> bool = <fun> *)
let auMoinsTrois = fun 
  (_::_::_::_) -> true
  | _ -> false;;

(* Tests *)
auMoinsTrois([1;2;3]);;
(* - : bool = true *)
auMoinsTrois([1;2]);;
(* - : bool = false *)

(* ---------------------------------------------------------- *)

(* Retourne la somme des 3 premiers éléments *)
(* sommeTrois : int list -> int = <fun> *)
let sommeTrois = fun
  (a::b::c::_) -> a + b + c;;

(* Tests *)
sommeTrois([1;2;3]);;
(* - : int = 6 *)
sommeTrois([4;2;3]);;
(* - : int = 9 *)

(* ---------------------------------------------------------- *)

(* Retourne si le 3eme élément est pair *)
(* troisEstPair : int list -> bool = <fun> *)
let troisEstPair = fun
  (a::b::c::_) -> c mod 2 = 0
  | _ -> raise(TROP_COURT);;

troisEstPair([1;2;3]);;
(* - : bool = false *)
troisEstPair([4;2;4]);;
(* - : bool = true *)

(* ---------------------------------------------------------- *)

(* Ajoute deux fois un élément à une liste et la retourne *)
(* ajoutDeuxFois : 'a * 'a list -> 'a list = <fun> *)
let ajoutDeuxFois = fun
  (a,l) -> a::a::l;;

(* Tests *)
ajoutDeuxFois("oui",["lol"]);;
(* - : string list = ["oui"; "oui"; "lol"] *)

(* ---------------------------------------------------------- *)

(* Permute les deux premiers éléments d'une liste *)
(* permute12 : 'a list -> 'a list = <fun> *)
let permute12 = fun
  (a::b::l) -> b::a::l;;

(* Tests *)
permute12([1;2;3]);;
(* - : int list = [2; 1; 3] *)

(* ---------------------------------------------------------- *)

(* 2 - Premières fonctions récursives sur les listes *)

(* Retourne la liste sous le format [n;n−1;...;1;0] *)
(* construitListe : int -> int list = <fun> *)
let rec construitListe = fun
  0 -> [0]
  | n -> n::construitListe(n-1);;

(* Tests *)
construitListe(6);;
(* - : int list = [6; 5; 4; 3; 2; 1; 0] *)

(* ---------------------------------------------------------- *)

(* Retourne la longueur d'une liste *)
(* longueur : 'a list -> int = <fun> *)
let rec longueur = fun
  (_::l) -> 1 + longueur(l)
  | [] -> 0;;

(* Tests *)
longueur([1;2;3]);;
(* - : int = 3 *)

(* ---------------------------------------------------------- *)

(* Retourne le dernier élément *)
(* dernier : 'a list -> 'a = <fun> *)
let rec dernier = fun
  (a::[]) -> a
  | (_::l) -> dernier(l);;      

(* Tests *)
dernier([1;2;3]);;

(* ---------------------------------------------------------- *)

(* Retourne la somme des éléments d'une liste *)
(* somme : int list -> int  *)
let rec somme = fun
  (a::l) -> a + somme(l)
  | _ -> 0;;

(* Tests *)
somme([1;2;3]);;
(* - : int = 6 *)

(* ---------------------------------------------------------- *)

(* Retourne si une liste a un nombre pair d'élément *)
(* taillePaire : 'a list -> bool *)
let rec taillePair = fun
  (_::_::l) -> taillePair(l)
  | (_::_) -> false
  | _ -> true;;

(* Tests *)
taillePair([1;2;3]);;
(* - : bool = false *)
taillePair([1;2;3;4]);;
(* - : bool = true *)

(* ---------------------------------------------------------- *)

(* Retourne une liste construite à partir des 
éléments impair d'une liste initiale *)
(* rangImpair : 'a list -> 'a list  *)
let rec rangImpair = fun
  (a::_::l) -> a::rangImpair(l)
  | (a::_) -> [a]
  | _ -> [];;

(* Tests *)
rangImpair([1;2;3;4]);;
(* - : int list = [1; 3] *)

(* ---------------------------------------------------------- *)

(* 3 - Encore des fonctions récursives sur les listes  *)

(* Retourne si un élément appartient à une liste *)
(* appartient : 'a * 'a list -> bool *)
let rec appartient = fun 
  (e,a::l) -> a = e or appartient(e,l)
  | (_,_) -> false;;

(* Tests *)
appartient(3,[1;2;3]);;
appartient(4,[1;2;3]);;

(* ---------------------------------------------------------- *)

(* Retourne si un élément appartient à une liste *)
(* appartient : 'a * 'a list -> bool *)
let rec maximum = fun 
  [a] -> a
  |(a::l) ->
    if(a >= maximum(l)) then
      a
    else
      maximum(l);;
  

(* Tests *)
maximum([1;2;3]);;
(* - : int = 3 *)
maximum([-3;-1;-3;-3]);;
(* - : int = -1 *)

(* ---------------------------------------------------------- *)

(* Retourne le nombre d'occurences d'un élément dans une liste *)
(* occurrences : 'a * 'a list -> int  *)
let rec occurences = fun
  (e,a::l) ->
    if(e = a) then
      1 + occurences(e,l)
    else
      occurences(e,l)
  | (e,[]) -> 0;;

(* Tests *)
occurences(1,[1;1;2;3]);;
(* - : int = 2 *)
occurences(3,[1;1;2;3]);;
(* - : int = 1 *)
occurences(9,[1;1;2;3]);;
(* - : int = 0 *)

(* ---------------------------------------------------------- *)

(* Construit une liste en multipliant par deux tous les entiers d'une liste *)
(* fois2 : int list -> int *)
let rec fois2 = fun
  (a::l) -> a*2::fois2(l)
  | (_) -> [];;

(* Tests *)
fois2([1;2;3;4]);;
(* - : int list = [2; 4; 6; 8] *)

(* ---------------------------------------------------------- *)

let rec insere = fun
  (n,(a::l)) ->
    if(n <= a)then
      n::a::l
    else
      a::insere(n,l)
  | (n,_) -> [n];;

(* Tests *)
insere(2,[1;3;4]);;
(* - : int list = [1; 2; 3; 4] *)
insere(4,[1;3;4]);;
(* - : int list = [1; 3; 4; 4] *)
insere(-4,[1;3;4]);;
(* - : int list = [-4; 1; 3; 4] *)

(* ---------------------------------------------------------- *)

(* 4 - Encore des fonctions récursives sur les listes  *)

let rec ieme = fun
  (1,a::_) -> a
  |(i,_::l) -> ieme(i-1,l)
  |(_,[]) -> raise TROP_COURT;;

(* Tests *)
ieme(5,[1;2]);;
(* Exception non rattrapée: TROP_COURT *)
ieme(2,[1;2]);;
(* - : int = 2 *)

(* ---------------------------------------------------------- *)

(* Retourne la sous-liste composé des p premiers éléments d'une liste l *)
(* prendre : int * 'a list -> 'a list = <fun> *)
let rec prendre = fun
  (1,a::_) -> [a]
  |(p,a::l) -> a::prendre(p-1,l)
  |(_,[]) -> raise TROP_COURT;;

(* Tests *)
prendre(2,[1;2;3]);;
(* - : int list = [1; 2] *)
prendre(5,[1;2;3]);;
(* Exception non rattrapée: TROP_COURT *)

(* ---------------------------------------------------------- *)

(* Retourne la sous-liste privé des p premiers élément d'une liste l *)
(* enleve : int * 'a list -> 'a list  *)
let rec enleve = fun
  (1,(_::l)) -> l
  |(p,(a::l)) -> enleve(p-1,l)
  |(_,[]) -> raise TROP_COURT;;

(* Tests *)
enleve(2,[1;2;3]);;
(* - : int list = [3] *)
enleve(5,[1;2;3]);;
(* Exception non rattrapée: TROP_COURT *)

(* ---------------------------------------------------------- *)

(* Retourne la liste des paires de chaque éléments des listes a et b *)
(* melange : 'a list * 'b list -> ('a * 'b) list = <fun> *)
let rec melange = fun
  (a::la,b::lb) -> (a,b)::melange(la,lb)
  |(_,_) -> [];;

(* Tests *)
melange([1;2;3],["oui";"non";"lol"]);;
(* - : (int * string) list = [1, "oui"; 2, "non"; 3, "lol"] *)
melange([1;2;3],["oui";"non"]);;
(* - : (int * string) list = [1, "oui"; 2, "non"] *)


(* 5 - Suite de Fibonacci  *)

(* 6 - Fusion de listes croissantes  *)

(* 7 - Nombres premiers par le crible d’Ératosthène *)

(* 8 - Représentation des ensembles par des listes *)