(* Compte rendu : TP 5 - UNIFICATION *)

(* Introduction *)
type const = 
    BConst of bool
    | IConst of int
    | FConst of string ;;

type term = 
    Const of const
    | Var of string
    | Appl of term * term ;;

(* Fonctions préalables *)
let unifex1 = Appl(Appl(Const(FConst "p"), (Appl(Appl(Const(FConst "m"), Const(IConst 3)), (Var "x")))), Const(IConst 2));;
let unifex2 = Appl(Appl(Const(FConst "p"), (Appl(Appl(Const(FConst "m"), Const(IConst 3)), (Var "x")))), (Var "y"));;
let unifex3 = Appl(Appl(Const(FConst "p"), (Var "x")), (Appl(Appl(Const (FConst "m"), Const (IConst 3)), (Var "x"))));;
let unifex4 = Appl(Appl(Const(FConst "p"), (Var "y")), (Appl(Appl(Const (FConst "m"), Const (IConst 3)), (Var "z"))));;

(* Affiche une expression *)
(* val afficher : term -> string = <fun> *)
let rec afficher (t: term) = match t with
    Const c -> 
        (match c with
            BConst b -> string_of_bool(b)
            | IConst i -> string_of_int(i)
            | FConst f -> f)
    | Var v -> v
    | Appl (t1,t2) -> "(" ^ afficher(t1) ^ " " ^ afficher(t2) ^ ")";;

(* Tests *)
afficher unifex1;;
(* - : string = "((p ((m 3) x)) 2)" *)

(* Retourne la liste des variables d'un terme *)
(* val fv : term -> string list = <fun> *)
let rec fv (t: term) = match t with
    Const c -> []
    | Var v -> [v]
    | Appl (t1,t2) -> fv(t1)@fv(t2);;

(* Tests *)
fv unifex1;;
(* - : string list = ["x"] *)
fv unifex2;;
(* - : string list = ["x"; "y"] *)

(* L’Unification *)

type 'a option =
    | None
    | Some of 'a ;;

(* Recherche une valeur a partir d'une clé *)
(* val rechercher : 'a -> ('a * 'b) list -> 'b option = <fun> *)
let rec rechercher (key: 'a) (li: ('a * 'b) list) = match li with
    (a1,a2)::l -> if key = a1 then Some a2 else (rechercher key l)
    | _ -> None;;

(* Tests *)
rechercher 3 [(2,"aa");(3,"bb")];;
(* rechercher 3 [(2,"aa");(3,"bb")];; *)

let rec

(* Substitutions - Le retour des listes d’associations *)
(* L’unification *)
let rec union = fun s1 -> fun s2 -> match s1 with
    [] -> s2
    | e::s1' -> 
        if List.mem e s2 then
            (union s1' s2)
        else 
            e::(union s1' s2) ;;


  

	   
	   
	   
	   