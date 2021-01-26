(* Compte rendu TP1 - Multi-ensembles finis *)

(* 1. Multi-ensembles finis *)
(* 1.1. Par des listes *)
let me = [0;0;2;2;2;3];;

(* Retourne la multiplicite d'un element dans un multi-ensemble *)
(* val multiplicite : 'a list -> 'a -> int = <fun> *)
let rec multiplicite (li: 'a) (x: 'a) = match li with
    a::l -> if a = x then 1 + (multiplicite l x) else (multiplicite l x)
    | _ -> 0;;

(* Tests *)
(multiplicite me 2);;
(* - : int = 3 *)

(* Enleve le premier element d'une liste *)
(* val enleve : 'a list -> 'a -> 'a list = <fun> *)
let rec enleve (li: 'a) (x: 'a) = match li with
    a::l -> 
        if a = x then
            l
        else
            a::(enleve l x)
    | _ -> [];;

(* Tests *)
(enleve [1;2;3] 2);;
(* - : int list = [1; 3] *)

let me1 = [0;0;1;2];;
let me2 = [0;2;2;2];;

(* Retourne la somme de deux multi-ensembles *)
(* val l_somme : 'a list -> 'a list -> 'a list = <fun> *)
let rec l_somme (li1: 'a) (li2: 'a) = li1@li2;;

(* Tests *)
(l_somme me1 me2);;
(* - : int list = [0; 0; 1; 2; 0; 2; 2; 2] *)

(* Retourne la somme de deux multi-ensembles *)
(* val union_l : 'a list -> 'a list -> 'a list = <fun> *)
let rec union_l (li1: 'a) (li2: 'a) = match li1 with
    a::l -> [a]@(union_l l (enleve li2 a))
    | _ -> li2;;

(* Tests *)
(union_l me1 me2);;
(* - : int list = [0; 0; 1; 2; 2; 2] *)
(union_l me1 [3;5;7;2;2]);;
(* - : int list = [0; 0; 1; 2; 3; 5; 7; 2] *)

(* Retourne la somme de deux multi-ensembles *)
(* val intersection_l : 'a list -> 'a list -> 'a list = <fun> *)
let rec intersection_l (li1: 'a) (li2: 'a) = match li1 with
    a::l -> 
        if (multiplicite li2 a) > 0 then
            a::(intersection_l l (enleve li2 a))
        else
            (intersection_l l li2)
    | _ -> [];;

(* Tests *)
(intersection_l me1 me2);;
(* - : int list = [0; 2] *)

(* Retourne la somme de deux multi-ensembles *)
(* val difference_l : 'a list -> 'a list -> 'a list = <fun> *)
let rec difference_l (li1: 'a) (li2: 'a) = match li1 with
    a::l -> 
        if (multiplicite li2 a) > 0 then
            (difference_l l (enleve li2 a))
        else
            a::(difference_l l (enleve li2 a))
    | _ -> [];;

(* Tests *)
(difference_l me1 me2);;
(* - : int list = [0; 1] *)

(* Insere une element a la bonne position dans une liste *)
(* val insert : 'a list -> 'a -> 'a list = <fun> *)
let rec insert (li: 'a) (x: 'a) = match li with
    a::l -> 
        if a > x then
            x::a::l
        else
            a::(insert l x)
    | _ -> [x];;

(* Tests *)
(insert [1;3] 2);;
(* - : int list = [1; 2; 3] *)

(* Tri une liste *)
(* val sort : 'a list -> 'a list = <fun> *)
let rec sort (li: 'a) = match li with
    a::l -> (insert (sort l) a)
    | _ -> [];;

(* Tests *)
(sort [1;6;5]);;

(* Verifie l'egalité entre deux multi-ensembles *)
(* val equals : 'a list -> 'a list -> bool = <fun> *)
let rec equals (li1: 'a) (li2: 'a) = (sort li1) = (sort li2);;

(* Tests *)
(equals [1;3;2] [1;2;3]);;
(* - : bool = true *)
(equals [1;2;2] [1;2;3]);;
(* - : bool = false *)

(* 1.2. Par une application *)

type 'a multiset = 'a -> int ;;

let (m1 : int multiset) = function
    0 -> 2
    | 1 -> 0
    | 2 -> 3
    | 3 -> 1
    | _ -> 0;;

let (empty_m : int multiset) = function
    _ -> 0;;

(* Ajoute un element dans un multiset *)
(* val ajoute_m : 'a multiset -> 'a -> 'a -> int = <fun> *)
let ajoute_m (ms: 'a multiset) (element: 'a) = function
    x -> 
        if x = element then
            1 + (ms x)
        else
            (ms x);;

(* Tests *)
let m0 = (ajoute_m m1 0);;
(* val m0 : int -> int = <fun> *)
(m0 0);;
(* - : int = 3 *)
(m0 2);;
(* - : int = 3 *)
(m0 9);;
(* - : int = 0 *)

(* Convertis une liste en multiset *)
(* val multi_of_list : 'a list -> 'a multiset = <fun> *)
let rec multi_of_list (li: 'a list) = match li with
    a::l -> (ajoute_m (multi_of_list l) a)
    | _ -> function _ -> 0;;

(* Tests *)
let li1 = [0;0;2;2;2;3];;
let m0 = (multi_of_list li1);;
(* val m0 : int multiset = <fun> *)
(m0 2);;
(* - : int = 3 *)

(* Supprime un element dans un multiset *)
(* val enleve_un_m : 'a multiset -> 'a -> 'a -> int = <fun> *)
let enleve_un_m (ms: 'a multiset) (element: 'a) = function
    x ->
        if x = element then
            if (ms x) - 1 < 0 then
                0
            else
               (ms x) - 1
        else
             (ms x);;

(* Tests *)
let m0 = (enleve_un_m m1 0);;
(* val m0 : int -> int = <fun> *)
(m0 0);;
(* - : int = 1 *)

(* Supprime toutes les occurences d'un element dans un multiset *)
(* val enleve_tous_m : 'a multiset -> 'a -> 'a -> int = <fun> *)
let enleve_tous_m (ms: 'a multiset) (element: 'a) = function
    x -> 
        if x = element then
            0
        else
            (ms x);;

(* Tests *)
let m0 = (enleve_tous_m m1 0);;
(* val m0 : int -> int = <fun> *)
(m0 0);;
(* - : int = 0 *)
(m0 2);;
(* - : int = 3 *)

let (m1 : int multiset) = function
    0 -> 2
    | 1 -> 1
    | 2 -> 1
    | _ -> 0;;

let (m2 : int multiset) = function
    0 -> 1
    | 2 -> 3
    | _ -> 0;;

(* Retourne la somme de deux multisets *)
(* val somme_m : 'a multiset -> 'a multiset -> 'a -> int = <fun> *)
let somme_m (ms1: 'a multiset) (ms2: 'a multiset) = function
    x -> (ms1 x) + (ms2 x);;

(* Tests *)
let m0 = (somme_m m1 m2);;
(m0 0);;

(* Retourne l'intersection de deux multisets *)
(* val intersection_m : 'a multiset -> 'a multiset -> 'a -> int = <fun> *)
let intersection_m (ms1: 'a multiset) (ms2: 'a multiset) = function
    x -> 
        if (ms1 x) < (ms2 x) then
            (ms1 x)
        else 
            (ms2 x);;

(* Tests *)
let m0 = (intersection_m m1 m2);;
(m0 2);;
(* - : int = 1 *)

(* Retourne l'union de deux multisets *)
(* val union_m : 'a multiset -> 'a multiset -> 'a -> int = <fun> *)
let union_m (ms1: 'a multiset) (ms2: 'a multiset) = function
    x -> 
        if (ms1 x) > (ms2 x) then
            (ms1 x)
        else 
            (ms2 x);;

(* Tests *)
let m0 = (union_m m1 m2);;
(m0 2);;
(* - : int = 3 *)

(* Retourne la difference de deux multisets *)
(* val difference_m : 'a multiset -> 'a multiset -> 'a -> int = <fun> *)
let difference_m (ms1: 'a multiset) (ms2: 'a multiset) = function
    x -> 
        if (ms2 x) > 0 then
            if (ms1 x) - (ms2 x) < 0 then
                0
            else
                (ms1 x) - (ms2 x)
        else
            (ms1 x);;

(* Tests *)
let m0 = (difference_m m1 m2);;
(m0 2);;
(* - : int = 0 *)

(* 1.3. Par un couple *)

(* 2. Ordre sur les multi-ensembles. *)

let rec listmap (f: 'a -> 'b) (li: 'a list) = match li with
    a::l -> (f a)::(listmap f l)
    | _ -> [];;

listmap (function x->x+2) [1;2;3];;
listmap (function x-> [x]) [1;2;3];;

let comp (f1:'b -> 'c) (f2:'a -> 'b) (x:'a) = f1(f2 x);;
