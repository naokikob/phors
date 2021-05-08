open Syntax

type fval = int array
type fenv = fval array
type aenv = float array

let step = ref false
let size = ref 32
let factor = ref 32.0
let hsize = ref 256
let hfactor = ref 256.0
let redundancy = 0

let rec index avals =
  match avals with
   [] -> 0
  | i::avals' -> (min !size i) + (!size+1)*(index avals')

let rec index_of_aenv aenv = index(Array.to_list aenv)

let alpha r =
  if r=0.0 then 0
  else int_of_float(floor(r *. !factor))

let gamma n =
  float_of_int(n) /. !factor
let halpha r =
  if r=0.0 then 0
  else int_of_float(ceil(r *. !hfactor))

let out_of_hbound v = v>4.0
let hinfty = ref 5.0

let equations = ref [||]

let out_of_bound fid avals =
  let vars = (fun (_,_,x,_)-> x)((!equations).(fid)) in
    List.exists (fun vs -> 
       match vs with
         _::_::_ -> 
           List.fold_left (fun sum i -> sum+(List.nth avals i)) 0 vs > !size+redundancy
       | _ -> false
    ) vars

let rec abs_apply_aux fid fval avals cvals =
  match cvals with
   [] -> fval.(index avals) 
 | r::cvals' ->
     let a = alpha r in
         abs_apply_aux fid fval (avals@[a]) cvals'

(* Compute an overapproximation of the function application f(v1,...,vk),
   where the values of f at discrete points are given in fval, and
    cvals = [v1;...;vk].
   an overapproximation of f(v1,...,vk) is computed from the values at surrounding points
   by a kind of linear interpolation. Assuming that the actual function f is convex,
   the linear interpolation does give an overapproximation.

   The argument "inside" represents whether we are computing the value of a function
   in a valid domain; in that case, any intermediate value should be no greater than 1.0
*)
let rec abs_apply inside fid fval cvals =
(*  (fval.(index (List.map alpha cvals ))) *)
    abs_apply_aux fid fval [] cvals 

let updated = ref false
let rec array_size n =
  if n=0 then 1 else (!size+1) * (array_size (n-1))

let rec eval exp inside aenv fenv =
  match exp with
    Var(i) -> gamma(aenv.(i))
  | Float(r) -> r
  | Plus(e1,e2) -> let r1 = eval e1 inside aenv fenv in
                   let r2 = eval e2 inside aenv fenv in
                      r1 +. r2
  | Mult(e1,e2) -> let r1 = eval e1 inside aenv fenv in
                   let r2 = eval e2 inside aenv fenv in
                      r1 *. r2
  | App(fid, es) ->
      let cvals = List.map (fun e->eval e inside aenv fenv) es in
        abs_apply inside fid fenv.(fid) cvals

let rec iter_fun_aux fid vars sum e inside aenv fenv newfenv =
  match vars with
    [] ->
      let r = eval e inside aenv fenv in
      let i = index_of_aenv aenv in
            newfenv.(fid).(i) <- r
  | vs::vars' ->
      match vs with
       [] -> iter_fun_aux fid vars' 0 e inside aenv fenv newfenv 
      | v::vs' ->
          let dsize = min !size (!size+redundancy-sum) in
          for aval=0 to dsize do 
            (aenv.(v) <- aval;
             let inside = inside && (aval+sum<= !size) in
             iter_fun_aux fid (vs'::vars') (aval+sum) e inside aenv fenv newfenv)
    done
    

let rec iter_fun fid arity vars e fenv newfenv =
  let aenv = Array.make arity 0 in
    iter_fun_aux fid vars 0 e true aenv fenv newfenv


(* repeat until a fixpoint is reached *)
let rec iter_sub i fenv eqsys =
 if i=0 then fenv
 else 
  let _ = updated := false in
  let newfenv = Array.copy fenv in
  let _ =
    Array.iter (fun (fid, arity,vars,e) -> iter_fun fid arity vars e fenv newfenv) eqsys in
       iter_sub (i-1) newfenv eqsys

let rec iterate i eqsys =
  let fenv = Array.map (fun (fid,arity,_,_) -> Array.make (array_size arity) 0.0 ) eqsys in
  iter_sub i fenv eqsys

let lowerboundarray = ref [| [|0.0|] |]
let upperboundarray = ref [| [|0.0|] |]

let print_env array =
   for i=0 to Array.length array -1 do
       print_string "[";
       for j=0 to Array.length (array.(i))  -1 do
           print_string ((string_of_float array.(i).(j))^";")
       done;
       print_string "]\n"
   done

let mk_copy_fenv fenv =
  let a = Array.make (Array.length fenv) [| |] in
  for i=0 to Array.length fenv-1 do
     a.(i) <- Array.make (Array.length fenv.(i)) 1.0
  done;
  a

let lb i eqsys s hs =
  let _ = equations := eqsys in
  let _ = size := s in 
  let _ = factor := float_of_int(s) in 
  let fenv = iterate i eqsys in
    (if !debug then (print_string "pre-lower-bound:\n"; print_env (fenv));
     lowerboundarray := fenv;
     upperboundarray := mk_copy_fenv fenv;
      for i=0 to (Array.length fenv)-1 do
        let env = (!upperboundarray).(i) in
        let fgroup = (!Syntax.fgrouparray).(i) in
        for j=0 to (Array.length (fenv.(i)))-1 do
            env.(j) <- 1.0 -. (List.fold_left (fun sum f -> sum+. (fenv.(f).(j))) 0.0 fgroup)
        done
      done;
      if !debug then (print_string "pre-upper-bound:\n"; print_env (!upperboundarray));
      fenv.(0).(0)
    )


