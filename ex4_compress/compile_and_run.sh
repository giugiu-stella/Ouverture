ocamlc -I '../' -I '../ex3_arbre' '../ex3_arbre/arbre.ml' compress.ml unix.cma '../util.ml' test_compress.ml -o test.out;
./test.out
