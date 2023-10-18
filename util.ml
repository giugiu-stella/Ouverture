(* Fonction js popur créer X caractères aléatoires dont # (alphabet)
function generateRandom(x){s="";for(q=x-1;q>0;q--){s+=String.fromCharCode((e=(Math.floor(Math.random()*26)+97)))};return s+"#"}
*)

(* Fonctions utiles partout *)
open Unix;;
module Util : sig
    val time : ('a -> 'b) -> 'a -> 'b
    val max_list : int list -> int
    val creerPreffixes : string -> string list
    val function_test : ('a -> bool) -> ('a -> unit) -> 'a list -> unit
end = struct

let time (f:'a->'b) (x:'a) : 'b = (
    let t = Unix.gettimeofday() in
    let fx = f x in (
        Printf.printf "Execution time: %fs\n" (Unix.gettimeofday () -. t);
        fx
    )
)
let time_no (f:'a->'b) (x:'a) : unit = (
    let t = Unix.gettimeofday() in (
        f x;
        Printf.printf "Execution time: %fs\n" (Unix.gettimeofday () -. t);
    )
)


let rec function_test test_function print_function l =
    List.iter (fun x -> (
        assert (test_function x);
        print_function x
    )) l;
    print_string ("Tous les tests ont passé\n")

let creerPreffixes chaine = 
    let len = (String.length chaine) in 
    let rec aux i acc =
        if i<len then
            aux (i+1) ((String.sub chaine 0 (len-i))::acc)
        else 
            acc
    in aux 0 []

let max_list = List.fold_left max 0
end;;