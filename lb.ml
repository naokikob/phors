open Syntax

let equations: eqsys ref = ref [||]

let rec mk_env_aux env vs i =
   match vs with
      [] -> ()
    | v::vs' -> (env.(i) <- v;mk_env_aux env vs' (i+1))
    
let mk_env vs =
   let n = List.length vs in
   let env = Array.make n 0.0 in
      (mk_env_aux env vs 0; env)
      
let rec eval env i e =
   match e with
       Var x -> env.(x)
     | Float(r) -> r
     | App(fid, es) ->
         if i=0 then 0.0
	 else
           let vs = List.map (eval env i) es in
 	   let env' = mk_env vs in
	   let e' = body_of_eq ((!equations).(fid)) in
	      eval env' (i-1) e'
    | Plus(e1,e2) ->
         (eval env i e1) +. (eval env i e2)
    | Mult(e1,e2) ->
         (eval env i e1) *. (eval env i e2)
	 
let lb eqsys i =
   equations := eqsys;
   let e = body_of_eq (eqsys.(0)) in
   let env = [| |] in
      eval env i e 
      
