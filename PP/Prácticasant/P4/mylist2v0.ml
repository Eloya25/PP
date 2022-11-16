let lista = [1;2;3;4;5];;
let lista2 = [9;12;51];;
let listavacia = [];;
let rec make_list n l =
if n=0 then l
else make_list (n-1) (n :: l);;
let listagrande = make_list 1000000 [];;

let hd l = match l with
| h::_ -> h
| [] -> raise (Failure "hd");;


let tl l = match l with
| _::t-> t
| [] -> raise (Failure "tl");;


(*
let rec length = function
[] -> 0
| _::t -> 1 + length t;;
*)

(*
let rec length = function
if l=[] then 0
else 1 + length(tl l);;
*)

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


(*
let rec nth l n = match n with
 n==0 -> hd l
| n>0 -> nth (tl l)(n-1)
| n<0 -> raise (Invalid_argument "List.nth")
| _ -> raise (Failure "nth")
*)

(*
let rec nth (h::t) = function
h::t -> function 0 -> h
			| n-> nth t(n-1);;
*)


let rec append l1 l2 = 
if l1=[] then l2
else (hd l1)::(append(tl l1) l2);;


(*
let rec append l1 l2 = match l1 with
[] -> l2
| h::t -> h::(append t l2);;
*)



(*
let rec append = function
[] -> (function l -> l)
| h::t -> (function l -> h::append t l);;
*)


(*
let rec rev = function
  [] -> []
| h::t -> append (rev t) [h];;
*)


let rev l =
	let rec aux l a = match l with
	  [] -> a
	| h::t -> aux t (h::a)
  in aux l [];; 
	

(*
let rev_append l1 l2 =
if l1=[] then []
else append (rev l1) l2;;
*)


(*
let rev_append l1 = function
 [] -> []
| h::t -> append (rev l1) (h::t);;
*)


let rev_append l1 l2 = match (l1,l2) with
  [],[] -> []
| h1::t1,h2::t2 -> let rec aux acc l1 l2 = match (l1,l2) with
  		     [],[] -> rev acc
  		   | [],h2::t2 -> aux (h2::acc) (t2) (h2::t2)  
  		   | h1::t1,h2::t2 -> aux (h1::acc) (t1) (h2::t2)
  		   | _,_ -> raise (Failure "rev_append")
	in aux [] (rev l1) l2;;



let rec concat = function
  [] -> []
| h::t -> append h (concat t);;


let flatten = concat;;



let rec map f = function
  [] -> []
| h::t -> (f h) :: (map f t);;


(*
let rec map f l = 
if l=[] then []
else append ((f (hd l)) (map f tl l));;


let rec map f l = 
if l=[] then []
else (f (hd l) :: (map f (tl l)));;
*)

(*
let rec map2 f l1 = function
  [] -> [] 
| h::t -> (f (hd l1) h) :: map2 f (tl l1) t
| _-> raise (Invalid_argument "map2");;
*)


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
 | h::t -> f (fold_right f t a) h;;



let rec find p = function
  [] -> raise Not_found
| h::t -> if p h then h else find p t;;	


let rec for_all p = function
  [] -> true
| h::t -> (p h) && (for_all p t);;


let rec exists p = function
  [] -> false
| h::t -> (p h) || (exists p t);;


(*
let rec mem a l =
if l=[] then false
else	if (a==(hd l)) then true
	else (mem a (tl l));;
*)
	

let rec mem p = function
  [] -> false
| h::t -> if (p = h) then true
			else (mem p t);;


let rec filter p = function
  [] -> []
| h::t -> if (p h) then h::(filter p t)
			else (filter p t);;


let find_all = filter;;


let rec partition p = function
  [] -> ([],[])
| h::t -> let (si,no) = partition p t
			in if (p h) then (h::si,no)
				else (si,h::no);;


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
  [] -> raise (Failure "No existe elemento a borrar")
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



hd lista;;
hd listavacia;;
tl lista;;
tl listavacia;;
length lista;;
length listagrande;;
nth lista 2;;
nth lista (-2);;
nth lista 22;;
nth listagrande 22;;
append lista lista2;;
append lista listagrande;;
append listagrande listagrande;;
rev lista;;
rev listagrande;;
rev_append lista lista2;;
rev_append lista listagrande;;
rev_append listagrande listagrande;;  (*FALTA ESTA TAIL RECURSIVE*)
concat [[1;2];[55;78]];;
flatten [lista;lista2];;
map sqrt [4.0;16.0];;
map2 (-) [1;2] [5;6];;
map2 (-) listagrande listagrande;;
map2 (-) [1;2;3] [5;6];;
fold_left (+) 3 [4;2;5];;
fold_left (+) 3 listagrande;;
fold_right (+) [4;2;5] 3;;
fold_right (+) listagrande 3;;
find (fun x -> x = 2) [1;2;3];;
find (fun x -> x = 2) listagrande;;
for_all (fun x -> x=1) [1;1;1];;
for_all (fun x -> x=1) listagrande;;
exists (fun x -> x=4) [1;2;3];;
exists (fun x -> x=4) listagrande;;
mem 4 [1;2;3];;
mem 4 listagrande;;
filter (fun x -> x=1) [1;1;2];;
filter (fun x -> x=1) listagrande;;
find_all (fun x -> x=1) [1;1;2];;
find_all (fun x -> x=1) listagrande;;
partition (fun x -> x=1) [1;1;2];;
partition (fun x -> x=1) listagrande;;
split [(1,2);(3,4);(6,9)];;
combine [1;2;3] [3;4;5];;
combine [1;2;3;6] [3;4;5];;
combine listagrande listagrande;;
remove 0 [1;0;2;3;4];;
remove_all 1 [1;0;1;2;1;3;41;1;];;
ldif [1;2;3;2;4;5] [3;4;2;2;8];;
lprod [1;2;3] ['a';'b';'c';'d'];;
divide ['a';'e';'i';'o';'u'];;

