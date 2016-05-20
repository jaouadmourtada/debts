(* GESTION DE DETTES ENTRE PLUSIEURS PERSONNES *)

type agent = string * int ;;
type debt = string * int list ;;
type transf = ((string*string) * int) list ;;

  
(* FONCTIONS GÉNÉRALES *)
  
(* open List ;; *)
(* j'utilise les fonctions mem et map du module List ; 
alternative à open : utiliser List.mem etc. *)

let mem = List.mem ;;
let map = List.map ;;
    
let rec iteract f l el = match l with
  |[]-> el
  |t::q-> f t (iteract f q el) ;;

let sum l = iteract (fun x y -> x+y) l 0 ;;

  
(* VÉRIFICATIONS *)
  
let rec no_double = function
  |[]-> true
  |t::q -> (no_double q) && (mem t q = false) ;;

let is_debt l = (sum (map (fun (x,y)->y) l) = 0)
		&& (no_double (map (fun (x,y)->x) l)) ;;


(* OPÉRATIONS ÉLÉMENTAIRES *)
  
let rec elim_nulls = function
  |[]->[]
  |(et,t)::q-> if t=0 then elim_nulls q
		else (et,t)::(elim_nulls q) ;;
  
let rec etiqs = function
  |[]->[]
  |(et,t)::q-> let l = etiqs q in
	       if mem et l then l else et::l ;;
  
let rec sum_match et = function
  |[]->0
  |(et',t)::q-> (if et'=et then t else 0)+(sum_match et q) ;;
  
let sum_et l = map (fun et -> (et,sum_match et l) ) (etiqs l) ;;
  
let apply ((et,et'),t) =
  map (fun (et'',s)-> (et'',
		       if et'' = et then  s-t
		       else if et'' = et' then s+t
		       else s)) ;;

let execute transf = iteract apply transf ;;

let solves_debt transf debt = elim_nulls (execute transf debt) = [] ;;


(* ALGORITHMES DE TRANSFERTS *)

let rec insert (et,t) l = match t,l with			
  |0,_->l
  |_,(et',t')::q when abs t < abs t' -> (et',t')::(insert (et,t) q)
  |_,_ -> (et,t)::l ;;

let rec deb_cred = function
  |[]-> [],[]
  |(et,t)::q-> let (d,c)=deb_cred q in
	       if t>0 then (d,insert (et,t) c)
	       else (insert (et,t) d,c) ;;

exception Invalid_debt_list ;;
  
let algo debt =
  let rec update tsf ld lc = match ld,lc with
    |[],_-> tsf
    |_,[]-> tsf
    |(et,t)::q,(et',t')::q'-> let t'' = min (-t) t' in
			      update (((et,et'),t'')::tsf)
				     (insert (et,t+t'') q)
				     (insert (et',t'-t'') q')
  in
  if is_debt debt = false then raise Invalid_debt_list
  else let (ld,lc)= deb_cred debt in
       List.rev (update [] ld lc) ;;


  (* VERSION AVEC FLOTTANTS *)

let absf t = if t<0. then -.t else t ;;

let sumf l = iteract (fun x y -> x+.y) l 0. ;;

let is_debtf l = (sumf (map (fun (x,y)->y) l) < 0.01)
		&& (no_double (map (fun (x,y)->x) l)) ;;
    
let rec insertf (et,t) l = match t,l with			
  |t,_ when absf t < 0.01 -> l
  |_,(et',t')::q when absf t < absf t' -> (et',t')::(insertf (et,t) q)
  |_,_ -> (et,t)::l ;;

let rec deb_credf = function
  |[]-> [],[]
  |(et,t)::q-> let (d,c)=deb_credf q in
	       if t>0. then (d,insertf (et,t) c)
	       else (insertf (et,t) d,c) ;;

let algof debt =
  let rec update tsf ld lc = match ld,lc with
    |[],_-> tsf
    |_,[]-> tsf
    |(et,t)::q,(et',t')::q'-> let t'' = min (-.t) t' in
			      update (((et,et'),t'')::tsf)
				     (insertf (et,t+.t'') q)
				     (insertf (et',t'-.t'') q')
  in
  if is_debtf debt = false then raise Invalid_debt_list
  else let (ld,lc)= deb_credf debt in
       List.rev (update [] ld lc) ;;

  
  (* CRÉER LISTE DE DETTES À PARTIR D'AVANCES *)

  
