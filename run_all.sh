echo "=== Tests Exercice 2 ==="
cd ex2_dynamic
./compile_and_run.sh
cd ..
echo "=== Fin Tests Exercice 2 ==="
echo -e "\n"

echo "=== Tests Exercice 3 ==="
cd ex3_arbre
./compile_and_run.sh
cd ..
echo "=== Fin Tests Exercice 3 ==="
echo -e "\n"

echo "=== Tests Exercice 4 ==="
cd ex4_compress
./compile_and_run.sh
cd ..
echo "=== Fin Tests Exercice 4 ==="
echo -e "\n"

echo "=== Tests Exercice 5 ==="
cd ex5_compresstree
./compile_and_run.sh
cd ..
echo "=== Fin Tests Exercice 5 ==="
echo -e "\n"

echo "=== Execution Exercice 6 ==="
cd ex6_experiments
./compile_and_run.sh
cd ..
echo "=== Fin Execution Exercice 6 ==="
echo -e "\n"

rm *.cm* */*.cm* */*.out
