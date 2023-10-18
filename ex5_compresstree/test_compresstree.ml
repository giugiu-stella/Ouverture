open Util;;
open Compress;;
open Compress_tree;;

let __ = (
	print_string "Tests de arbreSuffixes\n";
	let tests = [
		("ANANAS#","((#)(A(NA(NAS#)(S#))(S#))(NA(NAS#)(S#))(S#))");
		("BANANE#","((#)(AN(ANE#)(E#))(BANANE#)(E#)(N(ANE#)(E#)))");
		("AZEAZE#","((#)(AZE(#)(AZE#))(E(#)(AZE#))(ZE(#)(AZE#)))");
		("tzvdsygcyxghuwaoddf#","((#)(aoddf#)(cyxghuwaoddf#)(d(df#)(f#)(sygcyxghuwaoddf#))(f#)(g(cyxghuwaoddf#)(huwaoddf#))(huwaoddf#)(oddf#)(sygcyxghuwaoddf#)(tzvdsygcyxghuwaoddf#)(uwaoddf#)(vdsygcyxghuwaoddf#)(waoddf#)(xghuwaoddf#)(y(gcyxghuwaoddf#)(xghuwaoddf#))(zvdsygcyxghuwaoddf#))");
		("ecbdeddca#","((#)(a#)(bdeddca#)(c(a#)(bdeddca#))(d(ca#)(dca#)(eddca#))(e(cbdeddca#)(ddca#)))");
		("fbebefba#","((#)(a#)(b(a#)(e(befba#)(fba#)))(e(befba#)(fba#))(fb(a#)(ebefba#)))")
	] and print_function x = let (s,mot)=x in 
		print_string ("Mot de l'arbre de \""^s^"\" est \""^mot^"\"\n") 
	and test_function x = let (s,mot)=x in 
		let r = Compress_tree.arbreSuffixes s in
		(String.equal (Compress.chaine_de_int (r)) mot) && (String.equal (Compress_tree.plusLongueChaine r) s)
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
		(String.length (Compress_tree.sousChainesCommunes s1 s2))=l 
	
	and print_function x = let (s1,s2,l)=x in 
		print_string ("Plus long préfixe des suffixes de \""^s1^"\" et \""^s2^"\" est de taille "^(string_of_int l)^"\n")
	
	in Util.function_test test_function print_function tests;
);;