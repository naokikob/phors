open Syntax;;

let num_of_iter = ref 12
let num_of_iter2 = ref 100

let parseFile filename =
  let in_strm = 
    try
      open_in filename 
    with
	Sys_error _ -> (print_string ("Cannot open file: "^filename^"\n");exit(-1)) in
  let _ = print_string ("analyzing "^filename^"...\n") in
  let _ = flush stdout in
  let lexbuf = Lexing.from_channel in_strm in
  let result =
    try
      Eqparser.main Eqlexer.token lexbuf
    with 
	Failure _ -> exit(-1) (*** exception raised by the lexical analyer ***)
      | Parsing.Parse_error -> (print_string "Parse error\n";exit(-1)) in
  let _ = 
    try
      close_in in_strm
    with
	Sys_error _ -> (print_string ("Cannot close file: "^filename^"\n");exit(-1)) 
  in
    result

let hsize = ref 16
let vsize = ref 512
let rec read_options index =
  match Sys.argv.(index) with
   "-i" ->  (num_of_iter := int_of_string(Sys.argv.(index+1));
             read_options(index+2))
  | "-i2" ->  (num_of_iter2:= int_of_string(Sys.argv.(index+1));
             read_options(index+2))
  | "-s" ->  (hsize := int_of_string(Sys.argv.(index+1));
		   vsize := int_of_string(Sys.argv.(index+2));
             read_options(index+3))
  | "-d" ->  (debug := true;
             read_options(index+1))
  | "-step" ->  (Approx.step := true;
             read_options(index+1))
  | _ -> index

let report_usage () =
    (print_string "Usage: \n";
     print_string "ub <filename> <num> <num>\n\n";
    )

let main() =
   let (index,flag) = 
      try
        (read_options 1, true)
      with
        Invalid_argument _ -> (0,false)
      | _ -> 
         (print_string "Invalid options\n\n";
          report_usage();
          exit (-1))
  in
  let parseresult =
    try
         parseFile(Sys.argv.(index))
    with
	Eqlexer.LexError s -> (print_string ("lex error: "^s^"\n"); exit (-1))
  in
  let eqsys = Syntax.convert parseresult in
  let l = Lb.lb eqsys !num_of_iter in
  let l2 =  Lbapprox.lb !num_of_iter2 eqsys !hsize !vsize in
  let _ = (print_string ("lower-bound: "^(string_of_float (max l l2))^"\n"); flush stdout) in
(*
  let _ =  (print_string ("approx lower-bound: "^(string_of_float al)^"\n"); flush stdout) in
*)
  let u =  Approx.ub eqsys !hsize !vsize in
  let _ =  (print_string ("upper-bound: "^(string_of_float u)^"\n"); flush stdout) in
	  ();;


if !Sys.interactive then
  ()
else
  main();;

