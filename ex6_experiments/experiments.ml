open Dynamic;;
open Compress_tree;;
open Compress;;
open Unix;;

let donnee0 = "D'abord confinée dans les monastères et limitée essentiellement au domaine religieux, la traduction s'étend au domaine profane dès le Xe  siècle. Bientôt apparaissent en langue romane des fabliaux, comédies ou romans imités d'oeuvres de l'Antiquité, et les premiers grands poètes - Chrétien de Troyes, Marie de France, Rutebeuf, Jean de Meung - sont avant tout traducteurs (à une époque où traduction, imitation et création ne sont guère différenciées). Ce n'est toutefois qu'au XIVe siècle, marqué par la création des premières universités, que la traduction quitte les monastères et connaît un bref essor grâce à la protection de la cour."
;;

let donnee1 = "D'après Nama et al., la traduction a joué un rôle important dans l'évolution de la langue française. D'abord confinée dans les monastères et limitée essentiellement au domaine religieux, la traduction s'étend au domaine profane dès le Xe  siècle. Bientôt apparaissent en langue romane des fabliaux, comédies ou romans imités d'oeuvres de l'Antiquité, et les premiers grands poètes - Chrétien de Troyes, Marie de France, Rutebeuf, Jean de Meung - sont avant tout traducteurs (à une époque où traduction, imitation et création ne sont guère différenciées). Ce n'est toutefois qu'au XIVe siècle, marqué par la création des premières universités, que la traduction quitte les monastères et connaît un bref essor grâce à la protection de la cour. Pendant ces quelques siècles, la réflexion sur la traduction comme pratique évolue elle aussi."
;;

let donnee2 = "D'après Nama et al., la traduction a joué un rôle important dans l'évolution de la langue française. Elle était d'abord confinée dans les monastères et limitée dans la plupart des cas au domaine religieux. À partir du Xe siècle, elle s'étend au domaine profane. Des textes littéraires écrits en langue romane et imités d'oeuvres de l'Antiquité commencent alors à apparaître. Les premiers grands poètes - Chrétien de Troyes, Marie de France, Rutebeuf, Jean de Meung - sont surtout traducteurs (à une époque où traduction, imitation et création sont peu différenciées). Ce n'est cependant qu'au XIVe siècle, marqué par l'établissement des premières universités, que la traduction quitte les monastères. Elle connaît un bref essor grâce à la protection de la cour. Pendant ces quelques siècles, la réflexion sur la traduction comme pratique évolue elle aussi."
;;

let donnee3 = "La traduction a joué un rôle important dans l'évolution de la langue française. Avant le Xe siècle, elle se met surtout au service de la religion; par la suite, elle se pratiquera de plus en plus en dehors des monastères, entre autres par des littérateurs. L'établissement des premières universités au XIVe siècle et l'appui de mécènes royaux marqueront des moments importants dans cette migration. Pendant ces quelques siècles, la réflexion sur la traduction comme pratique évoluera elle aussi."
;;

let donnee4 = "La traduction a joué un rôle important dans l'évolution de la langue française. D'après Nama et al., avant le Xe siècle, elle se met surtout au service de la religion; par la suite, elle se pratiquera de plus en plus en dehors des monastères, entre autres par des littérateurs. L'établissement des premières universités au XIVe siècle et l'appui de mécènes royaux marqueront des moments importants dans cette migration. Pendant ces quelques siècles, la réflexion sur la traduction comme pratique évoluera elle aussi."
;;

let donnee5 = "Le Canada s'étend de l'océan Pacifique à l'océan Atlantique. Il a une superficie de 9 984 670 kilomètres carrés (3 854 085 milles carrés) et une population de 33 930 830 habitants (estimation datant de 2008)."
;;

let donnee6 = "La ville de Montréal se situe sur le fleuve Saint-Laurent dans la province de Québec. Elle est reconnue pour sa population multiculturelle, sa vie nocturne animée, ses longs hivers ainsi que ses festivals d'été."
;;


let get_exec_time (f:string->string->string) (x1:string) (x2:string) : (float*string) =
    let t = Unix.gettimeofday () in 
    let r = f x1 x2 in
    (Unix.gettimeofday () -. t,r)
;;
let length_d0 = (String.length donnee0);;

let get_temps donnee : (string*string*int*float*float) list =
	let stop1 = ref false and stop2 = ref false in
	let rec aux = function
		| [] -> []
		| x::xs -> 
			let d0 = if length_d0 <= x
				then donnee0
				else String.sub donnee0 0 x
			and dn = if (String.length donnee) <= x
				then donnee
				else String.sub donnee 0 x
			in
			let (t1,r1) = if !stop1 then (0.-.1.,"") else get_exec_time Dynamic.mot_plps d0 dn 
			and (t2,r2) = if !stop2 then (0.-.1.,"") else get_exec_time Compress_tree.sousChainesCommunes d0 dn in (
				if t1 > 60. then (stop1:=true) else ();	
				if t2 > 60. then (stop2:=true) else ();	
				assert ((String.length r1)=(String.length r2));
				(r1,r2,max (String.length d0) (String.length dn),t1,t2)::(aux xs)
			)
	in aux [10; 20; 50; 75; 100; 125; 150; 200; 350; 400; 500; 1000]
;;

let rec print_list = function
	| [] -> ()
	| x::xs -> let (r1,r2,l,t1,t2)=x in(
		Printf.printf "\tPour %d characteres :
		\t\tTemps dynamique : %f
		\t\t\tResultat : \"%s\"
		\t\tTemps arbre : %f
		\t\t\tResultat : \"%s\"\n\n" l t1 r1 t2 r2;
		print_list xs
	)


let __ = 
	print_string "Pour donnee0 et donnee1 :\n";
	print_list (get_temps donnee1);
	print_string "\n\nPour donnee0 et donnee2 :\n";
	print_list (get_temps donnee2);
	print_string "\n\nPour donnee0 et donnee3 :\n";
	print_list (get_temps donnee3);
	print_string "\n\nPour donnee0 et donnee4 :\n";
	print_list (get_temps donnee4);
	print_string "\n\nPour donnee0 et donnee5 :\n";
	print_list (get_temps donnee5);
	print_string "\n\nPour donnee0 et donnee6 :\n";
	print_list (get_temps donnee6);
;;