let hd l = match l with
| h::_ -> h
| [] -> raise (Failure "hd");;


let tl l = match l with
| _::t-> t
| [] -> raise (Failure "tl");;


let length l =
let rec aux a = function
	[] -> a
	| h::t -> aux (a+1) t
  in aux 0 l;;


let rec nth l n = 
if(n == 0) then hd l 
else if(n > 0) then nth (tl l)(n-1)
	else if(n < 0) then raise (Invalid_argument "List.nth")
		else raise (Failure "nth");;



let rec append l1 l2 = 
if l1=[] then l2
else (hd l1)::(append(tl l1) l2);;



let rev l =
	let rec aux l a = match l with
	  [] -> a
	| h::t -> aux t (h::a)
  in aux l [];; 
	



let rev_append l1 l2 = match (l1,l2) with
  [],[] -> []
| [],h::t -> h::t
| h::t,[] -> rev (h::t)
| h1::t1,h2::t2 -> let rec aux acc l1 l2 = match (l1,l2) with
  		     [],[] -> rev acc
  		   | [],h2::t2 -> aux (h2::acc) [] (t2)
  		   | [h1],h2::t2 -> aux (rev (h1::acc)) [] (h2::t2)   
  		   | h1::t1,h2::t2 -> aux (h1::acc) (t1) (h2::t2)
  		   | _,_ -> raise (Failure "rev_append")
	in aux [] l1 l2;;



let rec concat = function
  [] -> []
| h::t -> append h (concat t);;


let flatten = concat;;



let rec map f = function
  [] -> []
| h::t -> (f h) :: (map f t);;



let rec map2 f l1 l2 =
  match (l1,l2) with
    [], [] -> []
  | h1::t1, h2::t2 -> (f h1 h2) :: (map2 f t1 t2)
  | _-> raise (Invalid_argument "map2");;


let rec fold_left f a = function
  [] -> a
| h::t -> fold_left f (f a h) t;;


let rec fold_right f l a =
  match l with
   [] -> a
 | h::t -> f h (fold_right f t a);;


let rec find p = function
  [] -> raise Not_found
| h::t -> if p h then h else find p t;;	


let rec for_all p = function
  [] -> true
| h::t -> (p h) && (for_all p t);;


let rec exists p = function
  [] -> false
| h::t -> (p h) || (exists p t);;

	

let rec mem p = function
  [] -> false
| h::t -> if (p = h) then true
			else (mem p t);;


let filter p l =
	let rec aux si no = function
		[] -> rev si
		| h::t -> if (p h) then aux (h::si) no t
				else aux si no t
	in aux [] [] l;;


let find_all = filter;;


let partition p l =
	let rec aux si no = function
		[] -> (rev si, rev no)
		| h::t -> if (p h) then aux (h::si) no t
				else aux si (h::no) t
	in aux [] [] l;;


let rec split = function
  [] -> ([],[])
| (h1,h2)::t -> let t1,t2 = split t in
		h1::t1,h2::t2;;



let rec combine l1 l2 =
 match (l1,l2) with
  [], [] -> []
  | h1::t1, h2::t2 -> (h1,h2) :: (combine (t1) (t2))
  | _ -> raise (Invalid_argument "combine");;




let rec remove a l = match l with 
  [] -> l
| h::t -> if (a==h) then t
	  else h::(remove a t);;


let rec remove_all a l = match l with
	[] -> []
| h::t -> if (a==h) then (remove_all a t)
	  else h::(remove_all a t);;



let rec ldif l1 l2 = match (l1,l2) with
  h1::t1, h2::t2 -> ldif (remove_all h2 l1) t2
| _ -> l1;;


let lprod l1 l2 =	
	let rec aux acc = function
		  [],_ -> rev acc
		| _::t1,[] -> aux acc (t1,l2)
		| h1::t1,h2::t2 -> aux ((h1,h2)::acc) (h1::t1,t2)
	in aux [] (l1,l2);;


let divide l =
  let rec aux acc acc2 = function
					  [h] -> (rev (h::acc), rev acc2)
					| [] -> (rev acc, rev acc2)
					| h::t -> aux (h::acc) (hd t::acc2) (tl t)
				in aux [] [] l;;
