(* Compte rendu TP5 *)

(* 1 - Fonctions usuelles opérant sur les chaîne de caractères *)

(* 1 *)

(* Retourne si un character x est une lettre*)
let lettre = fun x -> not (x <= `@` or (x >= `[` & x <= `\``) or x >= `{`);;

(* Tests *)
lettre(`=`);;
(* - : bool = false *)
lettre(`]`);;
(* - : bool = false *)
lettre(`|`);;
(* - : bool = false *)
lettre(`a`);;
(* - : bool = true *)
lettre(`Z`);;
(* - : bool = true *)

(* Retourne si un char est une majuscule *)
(* majuscule : char -> bool = <fun> *)
let majuscule = fun
    x -> 
        if( not lettre(x) ) then
            failwith("x n'est pas une lettre")
        else
            x < `a`;;
            
(* Tests *)
majuscule(`c`);;
(* - : bool = false *)
majuscule(`A`);;
(* - : bool = true *)
majuscule(`&`);;
(* Exception non rattrapée: Failure "x n'est pas une lettre" *)
majuscule(`{`);;
(* Exception non rattrapée: Failure "x n'est pas une lettre" *)
majuscule(`\``);;
(* Exception non rattrapée: Failure "x n'est pas une lettre" *)


(* Retourne si un char est une minuscule *)
(* minuscule : char -> bool = <fun> *)
let minuscule = fun x -> not majuscule(x);;

 (* Tests *)
majuscule(`c`);;
(* - : bool = true *)
majuscule(`A`);;
(* - : bool = false *)
majuscule(`&`);;
(* Exception non rattrapée: Failure "x n'est pas une lettre" *)
            
(* 2 *)

(* Retourne si un char c appartient à une chaine s *)
(* appartientRec : char * string -> bool = <fun> *)
let rec appartient = fun 
    (_,"") -> false 
    | (c, s) -> tetec(s) = c or appartient(c,reste(s));;         

(* Tests *)
appartient(`c`,"abcd");;
(* - : bool = true *)
appartient(`e`,"abcd");;
(*  *)
appartient(`#`,"abcd");;

(* 3 *)

(* Retourne si une chaine est le début d'une autre chaine *)
(* debut : string * string -> bool = <fun> *)
let rec debut = fun 
    ("",_) -> true
    | (s1,s2) -> tetec(s1) = tetec(s2) & debut(reste(s1),reste(s2));;

(* Tests *)
debut("abc","abcdef");;
(* - : bool = true *)
debut("aabc","abcdef");;
(* - : bool = false *)
debut("abc","aabcdef");;
(* - : bool = false *)
debut("abc","a");;
(* Exception non rattrapée: Failure "La chaine est vide" *)

(* 4 *)

(* Retourne si une chaine est incluse dans une autre chaine *)
(* incluse : string * string -> bool = <fun> *)
let rec incluse = fun
    (_,"") -> false
    | (s1, s2) -> debut(s1,s2) or incluse(s1,reste(s2));;

(* Tests *)
incluse("abc","lolabc");;
(* - : bool = true *)
incluse("abc","ab");;
(* Exception non rattrapée: Failure "La chaine est vide" *)
incluse("","abcdef");;
(* - : bool = true *)
incluse("abc", "bcd");;
(* - : bool = false *) 

(* 5 *)

(* Retourne la fréquence d'apparition d'un charactère dans une chaine *)
(* frequence : char * string -> int *)
let rec frequence = fun
    (_,"") -> 0
    | (c,s) ->
        if(tetec(s) = c) then
            1 + frequence(c,reste(s))
        else
            frequence(c,reste(s));;

(* Tests *)
frequence(`a`,"abaac");;
(* - : int = 3 *)
frequence(`d`,"abaac");;
(* - : int = 0 *)
frequence(`b`,"abaac");;
(* - : int = 1 *)

(* 6 *)

(* Retourne une chaine en enlevant un caractère *)
(*  elimine : char * string -> string  *)
let rec elimine = fun
    (_,"") -> ""
    |(c, s) ->
        if(tetec(s) = c)then
            elimine(c,reste(s))
        else
            tetes(s) ^ elimine(c,reste(s));;

(* Tests *)
elimine(`a`,"abcabcaaa");;
(* - : string = "bcbc" *)
elimine(`a`,"lollololo");;
(* - : string = "lollololo" *)

(* 7 *)

(* Retourne un chaine inverssé *)
(* renverse : string -> string *)
let rec renverse = fun
    "" -> ""
    | s -> renverse(reste(s)) ^ tetes(s);;

(* Tests *)
renverse("abcef");;
renverse("kayak");;

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

