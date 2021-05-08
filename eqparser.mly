%{
open Syntax
%}

%token <string> NAME
%token <string> FNAME
%token <float> FLOAT
%token FGROUP
%token COMMA
%token SEMICOLON
%token EQ
%token PERIOD
%token LPAR
%token RPAR
%token PLUS
%token MULT
%token EOF

%left PLUS
%left MULT

%start main
%type <Syntax.eqlist * Syntax.fgroups option> main
%type <Syntax.fgroups> fgroups
%type <Syntax.fgroup> fgroup
%type <Syntax.fgroup> fnames
%type <Syntax.eqlist> eqs
%type <Syntax.eq> eq
%type <Syntax.sexp> term
%type <string list> args
%type <string list list> fargs
%%

main:
  eqs 
  { ($1, None)  }
 | eqs FGROUP fgroups
  { ($1, Some($3))}

fgroups:
    fgroup {[$1]}
 |  fgroups fgroup {$2::$1}

fgroup:
    FNAME {[$1]}
 |  LPAR fnames RPAR {$2}

fnames:
    FNAME {[$1]}
  | fnames FNAME {$2::$1}
   
eqs: 
  eq 
  {[$1]}
| eq eqs
  { $1::$2  }
;
eq:
  FNAME LPAR fargs RPAR EQ term PERIOD
  {($1,$3,$6)}
| FNAME EQ term PERIOD
  {($1,[],$3)}
;

fargs: 
   args
    {[$1]}
|   args SEMICOLON fargs
    {$1::$3}
;

args:
   NAME
   {[$1]}
| NAME COMMA args
  {$1::$3}
;

term:
   NAME
   {SVar($1)}
 | FLOAT
   {SFloat($1)}
 | FNAME
   {SApp($1, [])}
 | FNAME LPAR RPAR
   {SApp($1, [])}
 | FNAME LPAR terms RPAR
   {SApp($1, $3)}
 | term PLUS term
   {SPlus($1,$3)}
 | term MULT term
   {SMult($1,$3)}
 | LPAR term RPAR
   {$2}
;

terms:
   term
   {[$1]}
 | term COMMA terms
   {$1::$3}
;
