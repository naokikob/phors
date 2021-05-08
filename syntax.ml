let debug = ref false

type var = string
type sexp = SVar of string | SFloat of float | SApp of string * sexp list | SPlus of sexp * sexp | SMult of sexp * sexp 
type eq = var * var list list * sexp


type eqlist = eq list
type fgroup = string list
type fgroups = string list list

type fid = int
type exp = Var of int | Float of float | App of fid * exp list | Plus of exp * exp | Mult of exp * exp 

type vgroup = int list list
 (* information about groups of variables that have constraints of the form
    x1+...+xk<=1 
    For example, if a function has three arguments x0, x1, and x2 that range over
       x0+x1<=1 and x2<=1, then the list should be [[0;1]; [2]]
       *)
type eqsys = (int * int * vgroup * exp) array

let body_of_eq (_,_,_,e)=e

let fnametab: string array ref = ref [| "dummy" |]
let init_fnametab i =
   fnametab := Array.make i "dummy"
   
let tab_fname_id = Hashtbl.create 100
let lookup_fname fname = Hashtbl.find tab_fname_id fname

let rec register_funs fnames i =
   match fnames with
      [] -> ()
    | fname::fnames' ->
        ( Hashtbl.add tab_fname_id fname i;
	  (!fnametab).(i) <- fname;
	  register_funs fnames' (i+1))

let rec mk_env_sub vars i =
  match vars with
     [] -> []
   | v::vars' -> (mk_env_sub vars' (i+1))@[(v,i)]
                   (* this is inefficient but would not matter
		       if the number of arguments is small *)

let rec mk_env varss i =
  match varss with
      [] -> []
    | vars::varss' ->
         (mk_env varss' (i+List.length vars))@(mk_env_sub vars i)

let rec convert_sterm venv sterm =
  match sterm with
     SVar(s) -> Var(List.assoc s venv)
   | SFloat(r) -> Float(r)
   | SApp(fname,sterms) ->
       App(lookup_fname fname, List.map (convert_sterm venv) sterms)
   | SPlus(sterm1,sterm2) ->
       Plus(convert_sterm venv sterm1, convert_sterm venv sterm2)
   | SMult(sterm1,sterm2) ->
       Mult(convert_sterm venv sterm1, convert_sterm venv sterm2)

let rec convert_eqs eqlist eqsys i =
  match eqlist with
     [] -> ()
   | (fname, varss, sterm)::eqlist' ->
        let num_of_vars =
	     List.fold_left (fun sum vars -> sum+List.length vars) 0 varss in
	let venv = mk_env varss 0 in
	let vgroup = List.map (fun vars -> List.map (fun v -> List.assoc v venv) vars) varss in
	let term = convert_sterm venv sterm in
          ( eqsys.(i) <- (i, num_of_vars, vgroup, term);
	     convert_eqs eqlist' eqsys (i+1))

let fgrouparray = ref [| [] |]

let rec register_fgroup (fgroup: string list) =
  let fids = List.map lookup_fname fgroup in
    List.iter (fun fid -> 
                    let fids' = List.filter (fun fid'->not(fid=fid')) fids in
		        (!fgrouparray).(fid) <- fids') fids
			
let rec register_fgroups_aux fgroups =
  match fgroups with
      [] -> ()
    | fgroup::fgroups' ->
        (register_fgroup fgroup;
	 register_fgroups_aux fgroups')
    
let register_fgroups fgroupsopt fnames num_of_funs =
   fgrouparray := Array.make num_of_funs [];
   match fgroupsopt with
      None -> ()
    | Some(fgroups) ->
        register_fgroups_aux fgroups
   
  

let convert (eqlist,fgroupsopt) =
   let fnames = List.map (fun (f,_,_)->f) eqlist in
   let num_of_eqs = List.length fnames in
   let _ = init_fnametab num_of_eqs in
   let _ = register_funs fnames 0 in
   let _ = register_fgroups fgroupsopt fnames num_of_eqs in
   let eqsys = Array.make num_of_eqs (0,0, [], Float(0.0)) in
     (convert_eqs eqlist eqsys 0;
       eqsys)
     
