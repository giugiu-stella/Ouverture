open Arbre;;
open Compress;;
module Compress_tree : sig
    val arbreSuffixes  : string -> Compress.racine 
    val sousChainesCommunes : string -> string -> string
    val plusLongueChaine : Compress.racine -> string
end = struct

let rec creerFusionNoeudDansListe indices profondeur_lettres mot liste =
    
    (* Précondition : mot[low] = mot[xlow] lors du premier appel de l'extérieur. 
    On commence donc à j=1 pour éviter de refaire la comparaison de la première lettre *)
    let rec creerFusionNoeud indices profondeur_lettres j mot x =
        let (low,high) = indices and 
            Arbre.Node((xlow,xhigh), xl, xp) = x in
        let diff_n = (high-low) and
            diff_x = (xhigh-xlow) in
        if j > min diff_n diff_x
        (* On a atteint la fin de n ou de x *)
        then let end_mot = (String.length mot)-1 in 
            if diff_n = diff_x
                (* n et x sont égaux, on renvoie x (avec la suite de la branche) *)
                then x
                else if diff_n < diff_x
                    (* Si x commence avec n, on crée le noeud de n avec comme enfants la fin ('#') et le reste de x *)
                    then Arbre.Node((low, high), [Arbre.Node((end_mot, end_mot), [], 0); Arbre.Node((xlow+diff_n+1, xhigh), xl, xp-(j+1))], xp)
                    (* Si n commence avec x, on ajoute le reste de n aux enfants de x *)
                    else Arbre.Node((xlow, xhigh), creerFusionNoeudDansListe (low+diff_x+1, high) profondeur_lettres mot xl, max xp (diff_x+1+diff_n+1 -j))
        else let c_n = (String.get mot (low+j)) and c_x = (String.get mot (xlow+j)) in
            if c_n = c_x
            (* les j-èmes lettres sont les mêmes, on continue *)
            then creerFusionNoeud indices (profondeur_lettres+1) (j+1) mot x
            else let 
                indice_return = (low, low+j-1) (* ou (xlow, xlow+j-1), pareil *) and
                node_n = Arbre.Node((low+j, high), [], diff_n-j+1) and
                node_x = Arbre.Node((xlow+j, xhigh), xl, xp-j) in
                if c_n < c_x
                    (* n est strictement inférieur à x, on renvoit le plus petit noeud de l'intersection des deux mots avec n avant x*)
                    then Arbre.Node(indice_return, [node_n; node_x], max (xp) (diff_n+1))
                    (* n est strictement supérieur à x, on renvoit le plus petit noeud de l'intersection des deux mots avec n après x*)
                    else Arbre.Node(indice_return, [node_x; node_n], max (xp) (diff_n+1))
    

    in
    let length = String.length mot and (low,high)=indices in
    if profondeur_lettres >= length || (high-low)<0 
    then liste 
    else let noeud_n = Arbre.Node((low,high), [], high-low+1) in
        match liste with
            | [] -> [noeud_n]
            | x::xs -> let Arbre.Node((xlow,xhigh), xl, _)=x in
                if low=xlow && high=xhigh then x::xs else
                let c_n = String.get mot low and c_x = String.get mot xlow in
                if c_n = c_x
                then (creerFusionNoeud indices (profondeur_lettres+1) 1 mot x )::xs
                else if c_n < c_x
                    then noeud_n::x::xs
                    else x::(creerFusionNoeudDansListe indices (profondeur_lettres+1) mot xs)


let chercheMax = List.fold_left (
    fun acc x -> 
        let Arbre.Node((low,high), l, p) = acc 
        and Arbre.Node((xlow,xhigh), xl, xp) = x in
        if p>xp then acc else x
)

let plusLongueChaine r =
    let (mot, liste)=r in
    let rec aux = function
        | [] -> ""
        | x::xs -> let Arbre.Node((low,high), l, p) = chercheMax x xs in
            (String.sub mot low (high-low+1))^(aux l)
    in aux liste

let ajoutChaine mot liste indices = creerFusionNoeudDansListe indices 0 mot liste



let arbreSuffixes (s:string) : Compress.racine = 
    let end_s = (String.length s)-1 in
    (s, (List.fold_left (ajoutChaine s) [Arbre.Node((end_s, end_s), [], 0)] (Compress.creerIndexSuffixes s)))


let sousChainesCommunes (s1:string) (s2:string) : string =
    let (s1,s2) = if String.length s1 > String.length s2
        then (s2^"#",s1)
        else (s1^"#",s2)
    in let (s1,arbreS1) = arbreSuffixes s1 in
    let newArbre = List.fold_left (
        fun oldArbre lh -> 
            let i_fin = Compress.indiceFinSousChaine_int s2 lh (s1,arbreS1) 
            and (low,high)=lh in
            if i_fin >= low
                then ajoutChaine s2 oldArbre (low,i_fin)
                else oldArbre
    ) [] (Compress.creerIndexSuffixes s2) in(
        Printf.printf "Taille espace arbre : %d\n" ((Obj.reachable_words (Obj.repr arbreS1))+(Obj.reachable_words (Obj.repr newArbre)));
        Printf.printf "Taille espace arbre 1 : %d\n" ((Obj.reachable_words (Obj.repr arbreS1)));
        Printf.printf "Taille espace arbre 2 : %d\n" ((Obj.reachable_words (Obj.repr newArbre)));
        plusLongueChaine (s2,newArbre);
    )
end
;;
