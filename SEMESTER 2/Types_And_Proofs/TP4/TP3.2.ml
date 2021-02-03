(* TP 03 - Vérification de types *)
(* 1. Listes d’associations *)
type 'a option =
    None 
    | Some of 'a;;

(* 1.1. Par une liste de couples *)

let liste1 = [("Max",22) ;("Nicolas",24) ;("Lucie",29)];;

(* Cherche la valeur associé dans une liste de couple à l'aide d'une clé *)
(* val cherche1 : 'a -> ('a * 'b) list -> 'b option = <fun> *)
let rec cherchel (key: 'a) (me: ('a * 'b) list) = match me with
    ((a,b)::l) ->  if key = a then Some b else (cherche1 key l)
    | _ -> None;;

(* Tests *)
cherchel "Nicolas" liste1;;
(* - : int option = Some 24 *)
cherchel "Margaux" liste1;;
(* - : int option = None *)

(* Ajoute un couple à la liste *)
(* val ajoutel : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list = <fun> *)
let ajoutel (key: 'a) (numb: 'b) (me: ('a * 'b) list) = (key,numb)::me;;

(* Tests *)
ajoutel "Margaux" 27 liste1;;
(* - : (string * int) list =
 [("Margaux", 27); ("Max", 22); ("Nicolas", 24); ("Lucie", 29)] *)

(* Retire une clé d'un multi ensemble *)
(* val retirel : 'a -> ('a * 'b) list -> ('a * 'b) list = <fun> *)
let rec retirel (key: 'a) (me: ('a * 'b) list) = match me with
    ((a,b)::l) ->  if key = a then l else (a,b)::(retirel key l)
    | _ -> [];;

(* Tests *)
retirel "Nicolas" liste1;;
(* - : (string * int) list = [("Max", 22); ("Lucie", 29)] *)

(* 1.2. Par une fonction *)
type ('a,'b) map = 'a -> 'b option;;

(* val map1 : (string, int) map = <fun> *)
let (map1 : (string, int) map) = function
    "Max" -> Some 22
    | "Nicolas" -> Some 24
    | "Pierre" -> Some 29
    | _ -> None;;

(* (* val me1 : string -> int option = <fun> *) *)
let map1 (key: string) = match key with
    "Max" -> Some 22
    | "Nicolas" -> Some 24
    | "Pierre" -> Some 29
    | _ -> None;;

(* Multi ensemble vide *)
(* val emptyf : ('a, 'b) map = <fun> *)
let (emptyf : ('a, 'b) map) = function _ -> None;;

(* Recherche un element dans un multi-ensemble *)
(* val cherchef : 'a -> ('a, 'b) map -> 'b option = <fun> *)
let cherchef (key: 'a) (me: ('a, 'b) map) = me key;;

(* Tests *)
cherchef "Max" map1;;
(* - : int option = Some 22 *)

(* Ajoute un element dans un multi-ensemble *)
(* val ajoutef : 'a -> 'b -> ('a, 'b) map -> ('a, 'b) map = <fun> *)
let ajoutef (key: 'a) (numb: 'b) (me: ('a, 'b) map) = 
    let (ms : ('a, 'b) map ) = function
        x -> 
            if x = key then
                Some numb
            else
                (me x) in ms;;

(* Tests *)
let map2 = (ajoutef "Margaux" 27 map1);;
map2 "Margaux";;
(* - : int option = Some 27 *)
map2 "Max";;
(* - : int option = Some 22 *)

(* Retire une element dans un multi-ensemble *)
(* val retiref : 'a -> ('a, 'b) map -> ('a, 'b) map = <fun>*)
let retiref (key: 'a) (me: ('a, 'b) map) =
    let (ms: ('a, 'b) map) = function
        x ->
            if x = key && ((me x) != None) then
                None
            else
                (me key) in ms;;

(* Tests *)
let map3 = (retiref "Nicolas" map1);;
map3 "Nicolas";;
(* - : int option = None *)
map3 "Max";;
(* - : int option = Some 24 *)

(*  Convertis une multi-ensemble list en multi-ensemble fonction *)
(* val list_to_map : ('a * 'b) list -> ('a, 'b) map = <fun> *)
let rec list_to_map (liste : ('a * 'b) list) = match liste with
    (a,b)::l -> (ajoutef a b (list_to_map l))
    | _ -> emptyf;;

(* Tests *)
let map4 = list_to_map liste1 ;;
map4 "Lucie" ;;
(* - : int option = Some 29 *)
    
(* 2. Vérification de types *)
(* 2.1. Les expressions en Caml O’Caramel *)

type expr =
    ConstInt of int
    | ConstBool of bool
    | Var of string
    | Add of expr * expr
    | Equal of expr * expr
    | And of expr * expr ;;

let expr1 = Add (ConstInt 2, ConstBool true);;
let expr2 = Add (ConstInt 2, Var "x");;
let expr3 =(And(Var "b", Equal(Var "y", Add(Var "x", ConstInt 2))));;


let rec affiche_expr (e: expr) = match e with
    ConstInt k -> (string_of_int k)
    | ConstBool b -> (string_of_bool b)
    | Var x -> x
    | Add (e1, e2) -> String.concat "( " [(affiche_expr e1);(affiche_expr e2); " )"];;

(* 2.2. Types et environnement de typage *)

(* 2.3. La vérification de type (enfin !) *)