Installation
========
  Run "make ub".
  You need Ocaml compiler
   (ocamlopt, ocamlyacc, and ocamllex).

  To run the experiments reported in the paper,
  run
      cd examples; ./runexp

Usage
====

   ub <options> <filename>

    Available options are:
       -i <num> 
          set the number of Kleene iterations for computing a lower bound
           the default value is 12.
       -s <num1> <num2>
          set the number of discrete points in the domain and codomation to num1 and num2 respectively.
	  (I.e., [0,1] of the domain is decomposed to intervals
	    [0,0], (i/num1, (i+1)/num1], for i=0,...,num1-1)
	    The default values are num1=16, num2=512.
       -step
          use step functions instead of piecewise linear functions to compute an upper-bound

Format of input file:
   equations of the form
   F(~x1;...;~xk) = e.
   where ~xi denotes a sequence of variables separated by commas.
   The function defined by the first equation must be nullary.
   The syntax of e is given by:

     e ::= x  (variable)
           |  r (floating point number)
           |  F(e1,...,ek) (function application)
	   |  e1+e2
	   |  e1*e2

     A nullary application F() may be abbreviated to F.

     See examples in the directory "examples".
     
   
