(*tri insere, algo du cours de tri par insertion, on prend un élément que 
l'on ajoute dans une liste vide au départ et qui sera construite de manière 
triée*)

let  rec insere = fun
    (x,a::l) -> if x <= a then x::a::l
                else a::insere(x,l)
    |(x,[]) -> [x] ;;

let rec triInsert = fun 
    [a] -> [a]
    |(a::l) -> insere(a,triInsert(l)) ;;

(*tri fusion, sépare une liste en deux et fais le tri de chacune des parties
et ensuite fusion*)

let rec divise = fun 
    (a::b::l) -> let (l1,l2) = divise (l) in (a::l1,b::l2)
    |l -> (l,[]);;

(* fusion des deux listes, on prend le plus petit élément de la tête des 
deux listes et l'ajoute a la liste fusion et on continue ainsi
jusqu'a avoir une liste vide, on concatene alors le reste de la liste*)
let rec fusion = fun 
    (a::l1,b::l2) -> if a < b 
                    then a::fusion(l1,b::l2) 
                    else b::fusion(a::l1,l2)
    |(l1,l2) -> l1@l2 ;;

let rec triFus = fun 
    [a] -> [a]
    |l -> let (l1,l2) = divise(l) in fusion(triFus(l1),triFus(l2)) ;;

(*Tri par bulle, compare les éléments 1à1 et les inverses quand ils ne sont pas
dans l'ordre croissant*)

let rec bulle = fun 
    [a] -> ([a],false)
    |(a::b::l) -> if a<=b then let (l2,inv)=bulle(b::l) in (a::l2,inv)
                else let (l2,inv)=bulle(a::l) in (b::l2, true) ;;

let rec triBulle = fun 
    l -> let (l2,inv) = bulle (l) in
            if inv then triBulle(l2)
            else l2 ;;


let partition = fun
    (p, a::l) -> 
        let (l1,l2) = partition(a,l) in
            if (p <= b) then
                (l1,a::l2)
            else
                (a::l1,l2)
    |(_,[]) -> ([],[])
       
        

            

          

