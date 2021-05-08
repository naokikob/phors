#!/bin/sh

time ../ub -step example2.3-1.txt
time ../ub -step example2.3-0.txt
time ../ub -step example2.3-v1.txt
time ../ub -step example2.3-v2.txt
time ../ub -step example2.3-v3.txt
time ../ub -step example2.4.txt
time ../ub -step double.txt
time ../ub -step -i 15 listgen.txt
time ../ub -step -i 15 -s 64 4096 treegen.txt
time ../ub -step  treegenp.txt
time ../ub -step -s 32 1024 listeven.txt 
time ../ub listeven2.txt 
time ../ub -step -i 25 determinize.txt
time ../ub -step -s 64 4096 treegen_even0.5.txt
time ../ub -step -s 64 4096 treegen_even0.49.txt
time ../ub -step -s 64 4096 treegen_even0.51.txt
time ../ub -step example5.4_0_0.txt
time ../ub -step example5.4_0.3_0.3.txt
time ../ub -step -i 10000 example5.4_0.5_0.5.txt
time ../ub -step discont_0_1.txt
time ../ub -step -i 1000 discont_0.01_0.99.txt
time ../ub -step -i 10000 incomp.txt
time ../ub -step -i 10000 -s 10 100 incomp.txt
time ../ub -step incomp2.txt
time ../ub -step -s 16 65536 incomp2.txt
