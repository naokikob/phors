open Syntax

type fval = int array
type fenv = fval array
type aenv = float array

let step = ref false
let size = ref 32
let factor = ref 32.0
let hsize = ref 256
let hfactor = ref 256.0
let redundancy = 2

let rec index avals =
  match avals with
   [] -> 0
  | i::avals' -> (min !size i) + (!size+1)*(index avals')

let rec index_of_aenv aenv = index(Array.to_list aenv)

let alpha r =
  if r=0.0 then 0
  else int_of_float(ceil(r *. !factor))

let gamma n =
  float_of_int(n) /. !factor
let halpha r =
  if r=0.0 then 0
  else int_of_float(ceil(r *. !hfactor))

let hgamma n =
  float_of_int(n) /. !hfactor

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
   [] -> if out_of_bound fid avals 
         then !hinfty
         else hgamma(fval.(index avals)) 
 | r::cvals' ->
     let a = alpha r in
     if gamma(a)=r || !step then 
         abs_apply_aux fid fval (avals@[a]) cvals'
    else
       let v1 = abs_apply_aux fid fval (avals@[a-1]) cvals' in 
       let v2 = abs_apply_aux fid fval (avals@[a]) cvals' in
       let r' = gamma(a-1) in
           if out_of_hbound(v2) 
           then 
              v2  (* an out-of-bound value represents infinity *)
           else v1 +. (v2 -. v1) *. (r -. r') *. !factor

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
(*  hgamma(fval.(index (List.map alpha cvals ))) *)
  let v = abs_apply_aux fid fval [] cvals in
     if inside then min 1.0 v else v

let updated = ref false
let rec array_size n =
  if n=0 then 1 else (!size+1) * (array_size (n-1))

let rec eval exp inside aenv fenv =
  match exp with
    Var(i) -> gamma(aenv.(i))
  | Float(r) -> r
  | Plus(e1,e2) -> let r1 = eval e1 inside aenv fenv in
                   let r2 = eval e2 inside aenv fenv in
                     if out_of_hbound(r1) || out_of_hbound(r2) then !hinfty
                     else r1 +. r2
  | Mult(e1,e2) -> let r1 = eval e1 inside aenv fenv in
                   let r2 = eval e2 inside aenv fenv in
                     if out_of_hbound(r1) || out_of_hbound(r2) then !hinfty
                     else r1 *. r2
  | App(fid, es) ->
      let cvals = List.map (fun e->eval e inside aenv fenv) es in
        abs_apply inside fid fenv.(fid) cvals

let rec iter_fun_aux fid vars sum e inside aenv fenv newfenv =
  match vars with
    [] ->
      let r0 = eval e inside aenv fenv in
      let i = index_of_aenv aenv in
      let r = if inside then min (!Lbapprox.upperboundarray).(fid).(i) r0 
                 else min r0 !hinfty  in
      let r' = fenv.(fid).(i) in
      let x = halpha(r) in
        if x > r' then 
           (updated := true;
            newfenv.(fid).(i) <- x)
        else ()
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

let print_fenv fenv =
  Array.iter (fun a -> 
   (print_string "["; Array.iter (fun n -> (print_float(hgamma n);print_string ";")) a; print_string "]\n")) fenv

(* repeat until a fixpoint is reached *)
let rec iter fenv eqsys =
  let _ = updated := false in
  let newfenv = Array.copy fenv in
  let _ =
    Array.iter (fun (fid, arity,vars,e) -> iter_fun fid arity vars e fenv newfenv) eqsys in
  if !updated then (if !debug then print_fenv fenv; flush stdout; iter newfenv eqsys) else fenv

let rec abs_lfp eqsys =
  let fenv = Array.map (fun (fid,arity,_,_) -> Array.make (array_size arity) 0 ) eqsys in
  iter fenv eqsys

let ub eqsys s hs =
  let _ = equations := eqsys in
  let _ = size := s in 
  let _ = factor := float_of_int(s) in 
  let _ = hsize := hs in 
  let _ = hfactor := float_of_int(hs) in 
  let fenv = abs_lfp eqsys in
    hgamma(fenv.(0).(0))

