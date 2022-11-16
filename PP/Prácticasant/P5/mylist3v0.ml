let lista = [1;2;4;5;6];;
let rec make_list n l =
if n=0 then l
else make_list (n-1) (n :: l);;
let listagrande = make_list 1000000 [];;
let listagrande2 = make_list 2000000 [];;


let suml l =
	let rec aux s = function
		  [] -> 0
		| [h] -> s+h
		| h::t -> aux (s+h) t
	in aux 0 l;;
	
	
	
let rec sumlm = function
	[] -> 0
	| h::t -> h + sumlm t;;

let maxl l = match l with 
  [] -> raise (Failure "maxl")
| h::[] -> h
| h::t -> let rec aux m = function
			[] -> m
			| h::t -> if (m>h); then aux m t
						else aux h t
		in aux 0 l;;
		
let rec maxlm = function
	[] -> raise (Failure "maxl")
	| h::[] -> h
	| h::t -> max h (maxlm t);;

		
		
let to0from n = 
	let rec aux l i =
		if i<0 then List.rev l
		else aux (i::l) (i-1)
	in aux [] n;;
		
let fromto m n = 
	let rec aux l i =
		if i<m then l
		else aux (i::l) (i-1)
	in aux [] n;;
	
let from1to n = 
	let rec aux l i =
		if i<1 then l
		else aux (i::l) (i-1)
	in aux [] n;;

	
let append l1 l2 = match (l1,l2) with
	  [],[] -> []
	| [],_ -> l2
	| _,[] -> l1
	| _,_ -> List.rev_append (List.rev l1) l2;;


let concat l =
	let rec aux l acc = match l with
		[] -> acc
		| h::t -> aux t (List.hd h::acc)
	in aux l [];;

(*
let map = List.map;;
*)

let power x y =
	let rec innerpower x y acc =
		if y=0 then acc
		else innerpower x (y-1) (x*acc)
	in
	if y>= 0 then innerpower x y 1
	else invalid_arg "power";;


let fib n = 
	let rec innerfib i f a =
		if i=n then f
		else innerfib (i+1) (f+a) f
	in if n>=0 then innerfib 0 0 1
	else invalid_arg "fib";;

	
let fact n =
	let rec innerfact n acc =
		if n=0 then acc
		else innerfact (n-1) (acc *. float n)
	in
	if n>=0 then innerfact n 1.
	else invalid_arg "fact";;


(*let incseg l = List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;*)


let incseg l = 
	let rec aux l acc l2 = match l with
		[] -> []
		| [h] -> List.rev ((h+acc)::l2)
		| h::t -> aux t (h+acc) ((h+acc)::l2)
	in aux l 0 [];;



let rec multicomp1 l x = match l with
  [] -> x
| f::t -> f (multicomp1 t x);;



let multicomp l x = 
	let rec aux l x acc = match l with
		  [] -> acc
		| f::t -> aux t x (f acc)
	in
	if l=[] then x
	else aux l x x;;
		


let insert x l =
	let rec aux x l l2 flag = match l,flag with
		_,0 -> List.rev_append l2 l
		| [],_ -> [x]
		| h::t, 1 -> if x<=h then aux x t (h::x::l2) 0
					else aux x t (h::l2) 1
	in aux x l [] 1;;
	

let rec insert_geno f x l = match l with
[] -> [x]
|h::t -> if f x h then x::l
	else h::insert_geno f x t;;

	

let rec insert_gen f x l =
	let rec aux f x l l2 flag = match l,flag with
		  [],_ -> List.rev_append l2 [x]
		| _,0 -> List.rev_append l2 l
		| h::t, 1 -> if f x h then aux f x t (h::x::l2) 0
					else aux f x t (h::l2) 1
	in aux f x l [] 1;;

	
let lprod l1 l2 =	
	let rec aux acc = function
		  [],_ -> List.rev acc
		| _::t1,[] -> aux acc (t1,l2)
		| h1::t1,h2::t2 -> aux ((h1,h2)::acc) (h1::t1,t2)
	in aux [] (l1,l2);;


suml lista;;
suml [];;
suml listagrande;;

sumlm lista;;
sumlm [];;
sumlm listagrande;;
maxl lista;;
maxl listagrande;;
maxl [];;
maxlm lista;;
maxlm listagrande;;
maxlm [];;
(*
to0from 1000000;;
fromto 23 1000000;;
from1to 1000000;;
append lista [55;66;77;88];;
append listagrande listagrande2;;
*)


concat [[1;2];[55;78]];;
concat [listagrande;listagrande2];;
(*
map sqrt listagrande;;
*)


power 2 20;;
(*
fib 90;;
fact 90;;
incseg lista;;
incseg listagrande;;


multicomp [sqrt;sqrt] 3.;;
multicomp [] 3.;;
multicomp1 [sqrt;sqrt] 3.;;
multicomp1 [] 3.;;


insert 3 lista;;
insert 3 listagrande;;
insert 10 lista;;
insert_gen (fun x -> fun y -> y=5) 3 lista;;
insert_gen (fun x -> fun y -> y=10) 4 lista;;
insert_gen (fun x -> fun y -> y=10) 4 listagrande;;
insert_gen (fun x -> fun y -> y=10) 4 [];;
insert_geno (fun x -> fun y -> y=5) 3 listagrande;;
insert_geno (fun x -> fun y -> y=10) 4 listagrande;;
insert_geno (fun x -> fun y -> y=10) 4 [];;



lprod [1;2;3] ['a';'b';'c';'d'];;
lprod listagrande ['a';'b';'c';'d'];;
lprod ['a';'b';'c';'d'] listagrande;;
*)
