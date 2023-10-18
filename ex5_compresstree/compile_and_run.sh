ocamlc -I '../' -I '../ex3_arbre' -I '../ex4_compress' '../ex3_arbre/arbre.ml' '../ex4_compress/compress.ml' compress_tree.ml unix.cma '../util.ml' test_compresstree.ml -o test.out;
./test.out
