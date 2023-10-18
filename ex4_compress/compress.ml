open Arbre;;
module Compress : sig
    type racine = string * (int*int) Arbre.sufftree list
    val compression_str : char Arbre.sufftree list -> string Arbre.sufftree list
    val compression_int : char Arbre.sufftree list -> racine
    val chaine_de_str : string Arbre.sufftree list -> string 
    val chaine_de_int : racine -> string 
    val motArbre_str : string Arbre.sufftree list -> string
    val motArbre_int : racine -> string
    val plusLongueChaine : racine -> string
    val indiceFinSousChaine_int : string -> (int*int) -> racine -> int
    val sousChainesCommunes : string -> string -> string
    val creerIndexSuffixes : string -> (int*int) list
end = struct
    type racine = string * (int*int) Arbre.sufftree list

    let compression_str (l: char Arbre.sufftree list) : string Arbre.sufftree list = 
        let str c = String.make 1 c in
        let rec get_name (l:char Arbre.sufftree list) : string = match l with
            | [] -> ""
            | Arbre.Node(c, l, p)::[] -> (str c)^(get_name l)
            | x1::x2::xs -> ""
        in
        let rec liste_suiv (l:char Arbre.sufftree list) : char Arbre.sufftree list = match l with
            | [] -> []
            | Arbre.Node(c, l, p)::[] -> liste_suiv l
            | x1::x2::xs -> l
        in
        let rec aux = function
            | [] -> []
            | Arbre.Node(c, l, p)::xs -> Arbre.Node((str c)^(get_name l), aux (liste_suiv l), p)::(aux xs)
        in aux l

    let compression_int (l: char Arbre.sufftree list) : racine = 
        let mot = Arbre.motArbre l in
        let length = (String.length mot) in
        let end_mot = length - 1 in
        let rec taille (l:char Arbre.sufftree list) : int = match l with
            | [] -> 0
            | Arbre.Node(_, l, _)::[] -> 1+(taille l)
            | x1::x2::xs -> 0
        in
        let rec indice_bas (n:char Arbre.sufftree) : int = 
            let Arbre.Node(c,l,_) = n in
            List.fold_left (fun acc x -> min acc ((indice_bas x)-1)) end_mot l
        in
        let rec liste_suiv (l:char Arbre.sufftree list) : char Arbre.sufftree list = match l with
            | [] -> []
            | Arbre.Node(_, l, _)::[] -> liste_suiv l
            | x1::x2::xs -> l
        in
        let rec aux = function
            | [] -> []
            | Arbre.Node(c, l, p)::xs -> 
                let low = (indice_bas (Arbre.Node(c, l, p))) in 
                let high = low+(taille l) in
                Arbre.Node((low, high), aux (liste_suiv l), p)::(aux xs)
        in (mot,aux l)

    let chaine_de_str (l:string Arbre.sufftree list) : string =
        let rec aux = function
            | [] -> ""
            | Arbre.Node(s, l, p)::xs -> "("^s^(aux l)^")"^(aux xs)
        in "("^(aux l)^")"


    let chaine_de_int (r:racine) : string =
        let (s,l0) = r in 
        let rec aux = function
            | [] -> ""
            | Arbre.Node((low, high), l, p)::xs -> "("^(String.sub s low (high - low + 1))^(aux l)^")"^(aux xs)
        in "("^(aux l0)^")"


    let rec motArbre_int (r:racine) : string = let (s,_) = r in s

    let rec motArbre_str (l:string Arbre.sufftree list) : string =
        let m_w = ref "" in (
            List.iter (fun x-> 
                let Arbre.Node(s,l,_)=x in 
                let w = s^(motArbre_str l) in
                if (String.length w) > (String.length !m_w)
                then m_w:=w
                else ()
            ) l;
            !m_w
        )

    let findHighest = List.fold_left (
        fun acc x -> 
            let Arbre.Node((low,high), l, p) = acc 
            and Arbre.Node((xlow,xhigh), xl, xp) = x in
            if p>xp then acc else x
        )

    let plusLongueChaine r =
        let (mot, liste)=r in
        let rec aux = function
            | [] -> ""
            | x::xs -> let Arbre.Node((low,high), l, p) = findHighest x xs in
                (String.sub mot low (high-low+1))^(aux l)
        in aux liste

    let creerIndexSuffixes chaine = 
        let end_s = (String.length chaine)-1 in
        let rec aux i acc =
            if i>=0
            then aux (i-1) ((i, end_s)::acc)
            else acc
        in aux (end_s) []

    let indiceFinSousChaine_int (mot:string) (indices:int*int) (racine:racine) : int =
        let (low, high) = indices 
        and (mot_x, arbre) = racine in
        if high<low then -1 else
        let rec aux_list i l = 
            if low+i > high then 
                low+i-1 else
            match l with
                | [] -> low-i-1
                | Arbre.Node((xlow,xhigh), xl, _)::xs -> 
                    let c_x = String.get mot_x xlow
                    and c_s = String.get mot   (low+i) in
                    if not (c_s = c_x) then 
                        if c_s > c_x then 
                            aux_list i xs else 
                        low+i-1 else 
                    let rec aux_noeud j =
                        let i_c_mot = low+i+j and i_c_x = xlow+j in
                        if  i_c_mot > high then 
                            high else
                        if  i_c_x > xhigh then 
                            aux_list (i+j) xl else
                        let c_x = String.get mot_x i_c_x
                        and c_s = String.get mot   i_c_mot in
                        if  c_x = c_s then 
                            aux_noeud (j+1) else 
                        i_c_mot-1
                    in aux_noeud 1

        in aux_list 0 arbre


    let sousChainesCommunes (s1:string) (s2:string) : string =
        let (s1,s2) = if String.length s1 > String.length s2
            then (s1^"#",s2)
            else (s2^"#",s1)
        in let (s1,arbreS1) = (compression_int (Arbre.arbreSuffixes s1)) in
        let newArbre = List.fold_left (
            fun oldArbre lh -> 
                let i_fin = indiceFinSousChaine_int s2 lh (s1,arbreS1) 
                and (low,high)=lh in
                if i_fin >= low
                    then Arbre.ajoutChaine (String.sub s2 low (i_fin-low+1)) oldArbre
                    else oldArbre
        ) [] (creerIndexSuffixes s2) in
        Arbre.motArbre newArbre



end;;