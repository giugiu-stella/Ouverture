open Arbre;;
open Compress_tree;;


let dot racine =
    let (m,l)=racine in
    let rec dot_fils liste pere = match liste with
        | [] -> ""
        | Arbre.Node((l,h), fils, p)::ns -> 
            let n_str = (String.sub m l (h-l+1)) in
            let str = pere^" -- "^n_str^";\n"
            in str^(dot_fils fils n_str)^(dot_fils ns pere)


    in let rec dot_racine liste = dot_fils liste m     

    in "graph {\n"^(dot_racine l)^"}";;

let __ =
    print_string ("// Graphe de ANANAS_ :\n"^((dot (Compress_tree.arbreSuffixes "ANANAS_")))^"\n\n");
    print_string ("// Graphe de BANANE_ :\n"^((dot (Compress_tree.arbreSuffixes "BANANE_")))^"\n\n");
    print_string ("// Graphe de CARAMBAR_ :\n"^((dot (Compress_tree.arbreSuffixes "CARAMBAR_"))))