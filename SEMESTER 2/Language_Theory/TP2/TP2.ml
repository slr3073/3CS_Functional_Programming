(* Compte rendu TP2 - Lecture d'un mot par un automate *)
(* 1. Rappel : Le type afd *)

(* 1.1. Le type etat *)
type etat = {accept : bool ; t : char -> int} ;;

(* 1.2. Le type afd *)
type afd = {sigma : char list ; nQ : int ; init : int ; e : int -> etat} ;;

let a1 = {sigma= ['a';'b'] ; nQ = 3; init = 1 ; 
	e = function	
	    1 -> { accept = false ;
		    t = function 
			    'a'->2
			    |'b'-> 1 }
		|2 -> { accept = false ;
		    t = function 
			   'a'->2
			   |'b'-> 3 }		   
		|3 -> { accept = true ;
		    t = function 
			    'a'->3
				|'b'->3 }
    };;

let a2 = {sigma= ['a';'b'] ; nQ = 3; init = 1 ; 
	e = function	
	    1 -> { accept = false ;
		      t = function 
			       'a'-> 2 }
		|2 -> { accept = false ;
		      t = function 
			       'a'-> 2
				   |'b'-> 3  }		   
		|3 -> { accept = true ;
		      t = function 
			    'a'-> 3
				|'b'-> 3 }		   				
	};;

(* Retourne le nombre d'etat d'un automate *)
(* val nombreEtats : afd -> int = <fun> *)
let nombreEtats (aut: afd) = aut.nQ;;

(* Tests *)
(nombreEtats a2);;
(* - : int = 3 *)

(* Retourne l'etat initial d'un automate *)
(* val etatInitial : afd -> int = <fun> *)
let etatInitial (aut: afd) = aut.init;;

(* Tests *)
(etatInitial a1);;
(* - : int = 1 *)

(* Retourne la description de l'etat num dans l'automate aut *)
(* val descriptionE : afd -> int -> etat = <fun> *)
let descriptionE (aut: afd) (num: int) = (aut.e num);;

(* Tests *)
(descriptionE a1 1);;
(* - : etat = {accept = false; t = <fun>} *)

let rec nombreEtatsAccAux (aut: afd) (cont: int) = match cont with
    0 -> 0 
    | x -> 
        if (aut.e x).accept then
            1 + (nombreEtatsAccAux aut (cont - 1))
        else
            (nombreEtatsAccAux aut (cont - 1));;

(* Retourne le nombre d'état acceptant *)
(* val nombreEtatsAcc : afd -> int = <fun> *)
let nombreEtatsAcc (aut: afd) = (nombreEtatsAccAux aut aut.nQ);;

(* Tests *)
nombreEtatsAcc a1;;
(* - : int = 1 *)

(* 2. Lecture d’un mot par un automate *)
(* 2.1. Cas d’un automate complet *)

(* Retourne la lecture d'un mot par un automate *)
(* val lireComplet : afd -> int -> string -> int = <fun> *)
let rec lireComplet (aut: afd) (x: int) (mot: string) = match mot with
    "" -> x
    | w -> lireComplet aut ((aut.e x).t (mot.[0])) (String.sub w 1 ((String.length w) - 1) );;

(* Tests *)
lireComplet a1 1 "ab" ;;
(* - : int = 3 *)

(* 2.2. Cas d’un automate incomplet *)

exception PasTransition;;

let transit ((aut: afd) * (q: int) * (char: c)) = match (aut,q,c) with
    
