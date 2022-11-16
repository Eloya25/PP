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
	
	
(*
let rec sumlm = function
	[] -> 0
	| h::t -> h + sumlm t;;
*)


let maxl l = match l with 
  [] -> raise (Failure "maxl")
| h::[] -> h
| h::t -> let rec aux m = function
			[] -> m
			| h::t -> if (m>h); then aux m t
						else aux h t
		in aux 0 l;;

(*		
let rec maxlm = function
	[] -> raise (Failure "maxl")
	| h::[] -> h
	| h::t -> max h (maxlm t);;
*)

(*		
let rec to0fromm n =
	if n<0 then []
	else n::to0fromm (n-1);;
*)

let to0from n = 
	let rec aux l i =
		if i<0 then List.rev l
		else aux (i::l) (i-1)
	in aux [] n;;
	
	
(*	
let rec fromtom m n =
	if m>n then []
	else m::fromtom (m+1) n;; 
*)	
	
		
let fromto m n = 
	let rec aux l i =
		if i<m then l
		else aux (i::l) (i-1)
	in aux [] n;;

	
(*	
let rec from1tom n =
		if n<1 then []
		else from1tom (n-1) @ [n];;
*)	
	
	
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
		[] -> List.rev acc
		| h::t -> aux t (List.rev_append h acc)
	in aux l [];;

let map f l =
	let rec aux f l acc = match l with
		[] -> List.rev acc
		| h::t -> aux f t ((f h)::acc)		
	in aux f l [];;

let power x y =
	let rec innerpower x y acc =
		if y=0 then acc
		else innerpower x (y-1) (x*acc)
	in
	if y >= 0 then innerpower x y 1
	else invalid_arg "power";;
	
	
(*	
let powerm x y =
	let rec innerpower x y =
		if y=0 then 1
		else x * innerpower x (y-1)
	in
	if y>= 0 then innerpower x y
	else invalid_arg "powerm";;
*)

let fib n = 
	let rec innerfib i f a =
		if i=n then f
		else innerfib (i+1) (f+a) f
	in if n >= 0 then innerfib 0 0 1
	else invalid_arg "fib";;
	
	
(*	
let fibm n =
	let rec innerfib n =
		if n<2 then n
		else innerfib (n-1) + innerfib (n-2)
	in
	if n>=0 then innerfib n
	else invalid_arg "fibm";;
*)

	
let fact n =
	let rec innerfact n acc =
		if n=0 then acc
		else innerfact (n-1) (acc *. float n)
	in
	if n>=0 then innerfact n 1.
	else invalid_arg "fact";;

(*	
let factm n =
	let rec innerfact n =
		if n=0then 1.
		else float n*.innerfact (n-1)
	in
	if n>=0 then innerfact n
	else invalid_arg "factm";;
*)

(*let incseg l = List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;*)


let incseg l = 
	let rec aux l acc l2 = match l with
		[] -> []
		| [h] -> List.rev ((h+acc)::l2)
		| h::t -> aux t (h+acc) ((h+acc)::l2)
	in aux l 0 [];;


(*
let rec multicomp1 l x = match l with
  [] -> x
| f::t -> f (multicomp1 t x);;
*)


let multicomp l x = 
	let rec aux l x acc = match l with
		  [] -> acc
		| f::t -> aux t x (f acc)
	in
	if l=[] then x
	else aux l x x;;
		

(*
let rec insertm  x = function
	[] -> [x]
	|h::t -> if x<= h then x::h::t
			else h::insertm x t;;
*)
		

let insert x l =
	let rec aux x l l2 flag = match l,flag with
		_,false -> List.rev_append l2 l
		| [],_ -> List.rev_append  l2 [x]
		| h::t, true -> if x<=h then aux x t (h::x::l2) false
					else aux x t (h::l2) true
	in aux x l [] true;;
	
(*
let rec insert_geno f x l = match l with
[] -> [x]
|h::t -> if f x h then x::l
	else h::insert_geno f x t;;
*)
	

let rec insert_gen f x l =
	let rec aux f x l l2 flag = match l,flag with
		  [],_ -> List.rev_append l2 [x]
		| _,false -> List.rev_append l2 l
		| h::t, true -> if f x h then aux f x t (h::x::l2) false
					else aux f x t (h::l2) true
	in aux f x l [] true;;

	
let lprod l1 l2 =	
	let rec aux acc = function
		  [],_ -> List.rev acc
		| _::t1,[] -> aux acc (t1,l2)
		| h1::t1,h2::t2 -> aux ((h1,h2)::acc) (h1::t1,t2)
	in aux [] (l1,l2);;


suml lista;;
suml [];;
suml listagrande;;
(*
sumlm lista;;
sumlm [];;
sumlm listagrande;;
*)
maxl lista;;
maxl listagrande;;
maxl [];;
(*
maxlm lista;;
maxlm listagrande;;
maxlm [];;
*)
to0from 0;;
to0from -3;;
to0from 1000000;;
(*
to0fromm 0;;
to0fromm -3;;
*)
fromto 9 3;;
fromto 23 1000000;;
fromto 1000000 0;;
(*
fromtom 9 3;;
fromtom 23 1000000;;
fromtom 1000000 0;;
*)
from1to 0;;
from1to 1;;
from1to 1000000;;
(*
from1tom 0;;
from1tom 1;;
from1tom 1000000;;
*)
append lista [55;66;77;88];;
append listagrande listagrande2;;
append lista [];;
append [] lista;;
(*
List.append lista [55;66;77;88];;
List.append listagrande listagrande2;;
List.append lista [];;
List.append [] lista;;
*)
concat [[1;3;4];[3;5;6];[5;6;99]];;
concat [[1;2];[55;78]];;
concat [listagrande;listagrande2];;
concat [[];[55;78]];;
concat [[55;78];[]];;
concat [[44;55;78]];;
(*
List.concat [[1;3;4];[3;5;6];[5;6;99]];;
List.concat [listagrande;listagrande2];;
List.concat [[1;2];[55;78]];;
List.concat [[];[55;78]];;
List.concat [[55;78];[]];;
List.concat [[44;55;78]];;
*)

map sqrt [1.0;3.0;5.0];;
(*map sqrt listagrande;;*)
map sqrt [];;
(*
List.map sqrt [1.0;3.0;5.0];;
List.map sqrt [];;
*)
power 2 20;;
power 0 20;;
power 2 -20;;
power 2 0;;
(*
powerm 2 20;;
powerm 0 20;;
powerm 2 -20;;
powerm 2 0;;
*)
fib 90;;
fib 0;;
fib -9;;
(*
fibm 0;;
fibm -9;;
*)
fact 91;;
fact 0;;
fact -9;;
(*
factm 91;;
factm 0;;
factm -9;;
*)
incseg lista;;
incseg listagrande;;
multicomp [sqrt;sqrt] 3.;;
multicomp [] 3.;;
(*
multicomp1 [sqrt;sqrt] 3.;;
multicomp1 [] 3.;;
*)
insert 3 lista;;
insert 3 listagrande;;
insert 10 lista;;
(*
insertm 3 lista;;
insertm 3 listagrande;;
insertm 10 lista;;
*)
insert_gen (fun x -> fun y -> y=5) 3 lista;;
insert_gen (fun x -> fun y -> y=10) 4 lista;;
insert_gen (fun x -> fun y -> y=10) 4 listagrande;;
insert_gen (fun x -> fun y -> y=10) 4 [];;
(*
insert_geno (fun x -> fun y -> y=5) 3 listagrande;;
insert_geno (fun x -> fun y -> y=10) 4 listagrande;;
insert_geno (fun x -> fun y -> y=10) 4 [];;
*)
lprod [1;2;3] ['a';'b';'c';'d'];;
lprod listagrande ['a';'b';'c';'d'];;
lprod ['a';'b';'c';'d'] listagrande;;



concat [[1;3;4];[3;5;6];[5;6;99]];;
concat [[1;2];[55;78]];;
concat [listagrande;listagrande2];;
concat [[];[55;78]];;
concat [[55;78];[]];;
concat [[44;55;78]];;
