#!/bin/sh

time ../ub example2.3-1.txt
time ../ub example2.3-0.txt
time ../ub example2.3-v1.txt
time ../ub example2.3-v2.txt
time ../ub example2.3-v3.txt
time ../ub example2.4.txt
time ../ub double.txt
time ../ub -i 100 listgen.txt
time ../ub -i 15 -s 64 4096 treegen.txt
time ../ub  treegenp.txt
time ../ub -s 32 1024 listeven.txt 
time ../ub listeven2.txt 
time ../ub determinize.txt 
time ../ub -s 64 4096 treegen_even0.5.txt
time ../ub -s 64 4096 treegen_even0.49.txt
time ../ub -s 64 4096 treegen_even0.51.txt
time ../ub example5.4_0_0.txt
time ../ub example5.4_0.3_0.3.txt
time ../ub -i 10000 example5.4_0.5_0.5.txt
time ../ub discont_0_1.txt
time ../ub -i 10000 discont_0.01_0.99.txt
time ../ub -i 10000 incomp.txt
time ../ub -i 10000 -s 10 100 incomp.txt
time ../ub incomp2.txt
time ../ub -s 256 65536 incomp2.txt
