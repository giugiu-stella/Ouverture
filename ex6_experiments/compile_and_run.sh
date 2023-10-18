ocamlc -I '../ex2_dynamic' -I '../ex3_arbre' -I '../ex4_compress' -I '../ex5_compresstree' '../ex3_arbre/arbre.ml' '../ex2_dynamic/dynamic.ml' '../ex4_compress/compress.ml' '../ex5_compresstree/compress_tree.ml' unix.cma experiments.ml -o test.out;
ocamlc -I '../ex3_arbre' -I '../ex4_compress' -I '../ex5_compresstree' '../ex3_arbre/arbre.ml' '../ex4_compress/compress.ml' '../ex5_compresstree/compress_tree.ml' dot.ml -o dot.out;
echo "Test en cours, ne pas fermer"
currentDate=`date`
echo -e "Résultats obtenus le $currentDate\n\n" > results.txt
./test.out >> results.txt
echo -e "\n\n" >> results.txt
echo "Test finis, résultats dans results.txt"
echo "Tests de DOT"
./dot.out > dot.txt
echo "Test de DOT finis, résultats dans dot.txt"
#read -p "Appuyez sur [Enter] pour quitter"
