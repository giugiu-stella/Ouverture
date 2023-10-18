open Util;;
open Arbre;;


let __ = (
	print_string "Tests de création\n";
	let tests = [
		("ANANAS#","((#)(A(N(A(N(A(S(#))))(S(#))))(S(#)))(N(A(N(A(S(#))))(S(#))))(S(#)))");
		("BANANE#","((#)(A(N(A(N(E(#))))(E(#))))(B(A(N(A(N(E(#)))))))(E(#))(N(A(N(E(#))))(E(#))))");
	] and print_function x = let (s,mot)=x in 
		print_string ("Mot de l'arbre de \""^s^"\" est \""^mot^"\"\n") 
	and test_function x = let (s,mot)=x in 
		(String.equal (Arbre.chaine_de (Arbre.arbreSuffixes s)) mot)
	in Util.function_test test_function print_function tests;

	print_string "\nTests de sousChaine\n";
	let tests = [
		("ANANAS#","ANAN",true);
		("ANANAS#","ANA",true);
		("ANANAS#","ANAN#",false);
		("ANANAS#","NAS",true);
		("BANANE#","BAN",true);
		("BANANE#","BANE",false);
		("BANANE#","NANE",true);
		("BANANE#","NANA",false);
	] and print_function x = let (mot,s,b)=x in 
		if b
		then print_string ("\""^s^"\" est une sous chaîne de \""^mot^"\"\n")
		else print_string ("\""^s^"\" n'est pas une sous chaîne de \""^mot^"\"\n") 
	and test_function x = let (mot,s,b)=x in 
		(Arbre.sousChaine mot s)=b
	in Util.function_test test_function print_function tests;



	print_string "\nTests de sousChainesCommunes\n";

	let tests = [
		("BANANE","ANANAS",4);
		("ABCDE","FGH",0);
		("AACBAA","BAABAA",3);
		("AACAABAA","BAABAA",5);
		("ABAABAAA","AAABAABA",6);
		("CARAMBAR","BARBAPAPA",3);
		("CARZER","ZERCAR",3);
		("ZERCAR","CARZER",3);
		("babacbbccacccabaccc","bcbacaacbacabacacccba",5)
	] 
	and test_function x  = let (s1,s2,l)=x in 
		(String.length (Arbre.sousChainesCommunes s1 s2))=l 
	
	and print_function x = let (s1,s2,l)=x in 
		print_string ("Plus long préfixe des suffixes de \""^s1^"\" et \""^s2^"\" est de taille "^(string_of_int l)^"\n")
	
	in Util.function_test test_function print_function tests;
);;