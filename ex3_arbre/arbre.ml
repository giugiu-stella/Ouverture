module Arbre : sig
    type 'a sufftree = Node of ('a * ('a sufftree) list * int)
    val arbreSuffixes  : string -> char sufftree list
    val sousChaine : string -> string -> bool
    val sousChainesCommunes : string -> string -> string
    val creersuffixes : string -> string list
    val motArbre : char sufftree list -> string
    val ajoutChaine : string -> char sufftree list -> char sufftree list
    val chaine_de : char sufftree list -> string
end = struct

(* On n'a pas besoin de représentation d'une feuille vide, vu que les feuilles ont forcément un charactère *)
type 'a sufftree = Node of ('a * ('a sufftree) list * int)

let rec motArbre (l:char sufftree list) : string =
    let findHighest = List.fold_left (
        fun acc x -> 
            let Node(_, _, p) = acc 
            and Node(_, _, xp) = x in
            if p>xp then acc else x
    ) in

    match l with
    | [] -> ""
    | x::xs ->
        let Node(c,ls,_) = findHighest x xs
        in (String.make 1 c)^(motArbre ls)
   
let chaine_de (l:char sufftree list) : string =
    let rec aux = function
        | [] -> ""
        | Node(c, l, p)::xs -> "("^(String.make 1 c)^(aux l)^")"^(aux xs)
    in "("^(aux l)^")"


let ajoutChaine s l = 
    let length = (String.length s) in
    let rec aux i l =
        if i>= length then l
        else let c = String.get s i
        in match l with
            | [] -> [Node(c, aux (i+1) [], length-i)]
            | Node(xc, xl, xp)::xs -> if xc = c 
                then Node(xc, aux (i+1) xl, max xp (length-i))::xs
                else if xc < c
                    then Node(xc, xl, xp)::(aux i xs)
                    else Node(c, aux (i+1) [], length-i)::Node(xc, xl, xp)::xs
    in aux 0 l


let creersuffixes chaine = 
    let rec aux i s acc =
        if i>=0 then 
            let new_s = (String.make 1 (String.get chaine i))^s in
            aux (i-1) new_s (new_s::acc)
        else 
            acc
    in aux ((String.length chaine)-1) "" []


let arbreSuffixes s =
    List.fold_right ajoutChaine (creersuffixes s) []




let sousChaine (s1:string) (s2:string) : bool=

    let rec aux1 l k valeur c =
        if k=(String.length s2) then true
        else if (String.get c l)= (String.get s2 k) then aux1 (l+1) (k+1) (valeur+1) c
            else false

    in

    let rec aux2 j k valeur c=
        let taille= (String.length c) in
        if taille<(String.length s2) then false 
        else if (String.get c j)<>(String.get s2 k) then false 
        else let mot = aux1 (j+1) (k+1) (valeur+1) c in 
            if mot= true then true
            else  false

    in 

    let rec aux3 i k valeur =
        let x = aux2 0 k valeur (String.sub s1 i ((String.length s1)-i) ) in 
        if x= true then true
        else if (i= (String.length s1)-1) then false
            else aux3 (i+1) k valeur

    in aux3 0 0 0;;

let creerprefixes (mot:string) : string list = 
    let len = (String.length mot)-1 in 

    let rec recupMot k i res =
        if k>i then res
        else 
            recupMot (k+1) i (res^(String.make 1 (String.get mot k)))
    
    in 

    let rec aux i tab =
        if i<0 then tab
        else 
            let result= recupMot 0 i ("") in 
            aux (i-1) (result::tab)
    
    in aux (len-1) []

let rec filtre l1 l2 = match l2 with
    |[] -> l2
    |h2::t2 ->  if (List.hd l1 = h2) then filtre l1 t2
                else h2::filtre l1 t2

let sousChaineMot (mot:string) : string list = 
    let suf= creersuffixes mot in 
    let pref = creerprefixes mot in 
    let len= (List.length pref)-1 in 

    let rec aux i resultat = 
        if i> len then resultat
        else
            let res= creersuffixes (List.nth pref i) in
            let res2 = filtre resultat res
            in 
            let ko = List.concat [res2; resultat]
            in aux (i+1) ko
    
    in aux 0 (List.concat [[]; suf])


let sousChainesCommunes (s1:string) (s2:string) : string=
    let scm = sousChaineMot s2 in 

    let rec aux l res = match l with
        | [] -> res
        | x::xs -> let sc = sousChaine s1 x in 
            if sc = false then aux xs res
            else 
                aux xs (ajoutChaine x res)

    in motArbre (aux scm [])


end;;