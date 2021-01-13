(*Exceptions*)
exception tropCourte;;
exception listeVide ;;

(*-----------------------*)

let deux = fun l -> hd(tl(l));;
(*Exception non rattrapée quand la liste est trop courte.*)

(*-----------------------*)
(*Exception rattrapée quand la liste est trop courte.*)
let deux = fun
(_::b::_) -> b
| _ -> raise tropCourte;;

(*-----------------------*)
(*Liste à 3 éléments*)
let auMoinsTrois = fun
(_::_::_::_) -> true
| _ -> false ;;

(*-----------------------*)
let somme23 = fun
(_::b::c::_) -> b+c
| _ -> 0 ;;

(*-----------------------*)
let somme123 = fun
(a::b::c::_) -> a+b+c
| _ -> 0 ;;

(*-----------------------*)
let troisEstPair = fun
(a::b::c::_) -> c mod 2 = 0
| _ -> false ;;

(*-----------------------*)
let rec compteARebours = fun
0 -> [0]
|n-> n::f(n-1);;

(*-----------------------*)
let rec tailleListe = fun
(_::l) -> 1 + tailleListe(l) 
|[] -> 0 ;;

(*-----------------------*)
let rec longPair = fun
(_::_::l) -> longPair(l)
|[] -> true 
| _ -> false ;;

(*-----------------------*)
let rec rangImpair = fun
(a::_::l) -> a::rangImpair(l)
| l -> l ;;

(*-----------------------*)
let rec valeurImpaire = fun 
(a::l) -> if a mod 2 = 0 then valeurImpaire(l)
          else a::valeurImpaire(l)
|l -> l ;;

(*-----------------------*)
let rec dernier = fun
(_::l) -> dernier(l)
|[a] -> a 
|_ raise -> tropCourte ;;

(*-----------------------*)
let rec somme = fun
(a::l) -> a + somme(l)
|[] -> 0 ;;

(*-----------------------*)
let rec appartient = fun
(n, (i::l)) -> n = i or appartient(n,l)
|(n, []) -> false ;;

(*-----------------------*)
let max = fun(a,b) -> if a > b then a else b ;;
let rec maximum = fun 
[a] -> a 
|(a::l) -> max(a, maximum(l))
|_ -> raise listeVide ;;