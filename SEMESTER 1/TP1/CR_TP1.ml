(* 1. Expressions élémentaires et types de base *)

(* Entiers et réels *)
1.2 + 1 ;; (* Erreur de type *)
1.2 + 2.3 ;; (* Erreur de type , manque +.*)
-2E-1 +. 2. ;; (* Marche : float = 1.8*)
(sqrt(4.)+. 2.)/. 3.5 ;; (* Marche : float = 1.14285714286*)
-2 * 3 ;; (* Marche : int = -6 *)
2.1 +. 4.9 ;; (* Marche : float = 7.0 *)
sqrt 9. ;; (* Marche : float = 3.0 , racine de 9 *)
10/3 ;; (* Marche : int = 3 *)
10 mod 3 ;; (* Marche : int = 1 *)
2+ 3*5 ;; (* Marche : int = 17, multiplication en 1er *)
-(5+1)*(-2+5)+2*3 ;; (* Marche : int = -12 *)

(* Booléens *)
1 = 2 ;; (* Marche : bool = false *)
4 < 5 ; ; (* Marche : bool = true *)
4.2 < 4.7 ;; (* Marche : bool = true *)
(1=1)=(2<1) ;; (* Marche : bool = false, parenthèses en 1er *)
true = 1 ;; (* Erreur de type, on ne peux pas comparer un bool à un int *)
true or false ;; (* Marche : bool = true *)
true or false = (1=1) & (4<5) ;; (* Marche : bool = true *)
(1+2 = 2+1) & 4>5 ;; (* Marche : bool = false *)
1+2 = 2+1 & 4>5 ;; (* Marche : bool = false, comparaison avant opérateur logique & *)
1+2 = 2+1 or 4>5 ;; (* Marche : bool = true, comparaison avant opérateur logique or *)
1+2 = 2+1 > 4 > 5 ;; (* Erreur de type , comparaison true > 4 *)
1+2 = 2+1 > (4>5) ; ; (* Marche : bool = true, true > false *)
false < true ;; (* Marche : bool = true *)
4 + 1 < 6 & (`a`<`h` or "debut" = "fin") ;; (* Marche : bool = true *)

(* Exercice *)
not 1 < 2 ;; (* comparaisons prioritaire sur le not sinon erreur *)
(* ça ma soulé *)

(* Chaines de caractères *)

"salut" ;; (* Marche : string = "salut" *)
"salut" ˆ "à tous" ;; (* Marche : string = "salutà tous" *)
"salut" ˆ " à tous" ;; (* Marche : string = "salut à tous" *)
"salut" < "bonjour" ;; (* Marche : bool = false, s > b *)
"salut" < "Salut" ;; (* Marche : bool = false, s = 115 et S = 83 *)
"A" < "a";; (* Marche : bool = true *)
’A’ < ’a’;; (* Marche : bool = true *)
’a’;; (* Marche : char = a *)
int_of_char(’a’);; (* Marche : int = 97 *)
’a’<’b’;; (* Marche : bool = true *)
’a’<"bonjour";; (* Erreur de type comparaison impossible entre char et string *)
"a"<"bonjour";; (* Marche : bool = true *)
’a’^"près";; (* Erreur de type concaténation impossible entre char et string *)
"a"^"près";; (* Marche : string = après *)
"12">"2";; (* Marche : bool = true *)


(* 2. Conversion de types *)

int_of_float ;; (* - : float -> int = <fun>, ??? *)
int_of_float(4.0) ;; (* Marche : int = 4 *)
int_of_float(4.25) ;; (* Marche : int = 4, troncature *)
int_of_float(-4.25) ;; (* Marche : int = -4, troncature *)
int_of_float(4.25e-33) ;; (* Marche : int = 0, ne fonctionne correctement que sur les exposants positifs *)

string_of_int(-235) ;; (* Marche : string = "-235" *)
string_of_int(55e2) ;; (* Erreur de type 55e2 est un float et non un int *)
string_of_float(55e2) ;; (* Marche : string = "5500.0" *)
string_of_float(-55e-2) ;; (* Marche : string = "-0.55" *)

int_of_string "345" ;; (* Marche : int = 345, pas besoin des parenthèses *)
int_of_string "34.5" ;; (* Exeption 34.5 ne peux pas être convertis en int *)
float_of_string "34.5" ;; (* Marche : float = 34.5 *)
float_of_string "999999999999999999999999.9" ;; (* Marche : float = 1e+024, arondis au supérieur *)


(* 3. Définitions globales et locales *)

let x = 2 ;; (* Marche : x : int = 2 *)
let y = x + 3 (* Marche : y : int = 5 *)
let x = y + 5 (* Marche : x : int = 10 *)
let z = y * 2 in x + z + y * y ;; (* Marche : int = 45, z est une variable locale *)
let x = 3 in x*x+2*x*y + 4*y ;; (* Marche : int = 59, z est une variable locale *)
let x = 1 in x = 2*x ;; (* Marche : bool = false *)
let x = 0 in x = 2*x ;; (* Marche : bool = true *)

(* Définitions locales emboîtées *)

let x = 5 in (* x = 5 *)
	let prod = x*x in (* prod =  25 *)
		prod + prod * prod ;; (* Marche : int = 650 *)

let resultat = let x = 5 in
    let prod = x*x in
        prod + prod * prod ;; (* Marche : int = 650 *)

let val = let x = 3 and y = 4 in (* x = 3 et y = 4 *)
    let x = x+y and y = x-y in (* x = 7 et y = -1 *)
        x*x + y*y ;; (* Marche : int = 50 *)

let y = 2 in
    val * val + 2 * val * y ;; (* Marche : int = 2700 *)

(* Expressions conditionnelles *)

if (1=1) then 
    "salut"
else
    "au revoir";; (* Marche : string = "salut" *)

let x = 3 in
    if (x<0) then
        x
    else
        x*x ;; (* Marche : int = 9 *)

if(5 > 0) then
    1
else
    "erreur" ;; (* Erreur l'expression est de type string, ne peux 
                pas renvoyé potentiellement deux types différents *)


let x=3 and y=3 in (* x = 3 et y = 3 *)
    let y = y*x in (* y = 9 *)
        if y mod 2 = 0 then (* 9 mod 2 != 0 donc impair *)
            "pair"
        else 
            "impair" ;; (* Marche : string = impair *)

