open Util;;
open Dynamic;;

let __ = (
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
	and test_function x  = let (s1,s2,sol)=x in 
		(Dynamic.longueur_plps s1 s2)=sol 
	
	and print_function x = let (s1,s2,sol)=x in 
		print_string ("Plus long préfixe des suffixes de \""^s1^"\" et \""^s2^"\" est de taille "^(string_of_int sol)^"\n")
	
	in Util.function_test test_function print_function tests
);;