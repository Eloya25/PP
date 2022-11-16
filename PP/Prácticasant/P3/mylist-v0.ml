let lista = [1;2;3;4;5];;
let lista2 = [9;12;51];;
(*let listavacia = [];*)

let hd l = match l with
| h::_ -> h
| [] -> raise (Failure "hd");;


let tl l = match l with
| _::t-> t
| [] -> raise (Failure "tl");;



let rec length = function
[] -> 0
| _::t -> 1 + length t;;

(*
let rec length = function
if l=[] then 0
else 1 + length(tl l);;
*)


let rec nth l n =  
if n=0 then hd l
else nth (tl l)(n-1);;

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

let rec rev = function
  [] -> []
| h::t -> append (rev t) [h];;


let rev_append l1 l2 =
if l1=[] then []
else append (rev l1) l2;;

(*
let rev_append l1 = function
 [] -> []
| h::t -> append (rev l1) (h::t);;
*)


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

let rec map2 f l1 = function
  [] -> [] 
| h::t -> (f (hd l1) h) :: map2 f (tl l1) t;;


(*
let rec map2 f l1 l2 =
  match (l1,l2) with
    [], [] -> []
  | h1::t1, h2::t2 -> (f h1 h2) :: (map f t1 t2)
  | _ -> raise (Invalid_argument "map2");;
*)


let rec fold_left f a = function
  [] -> a
| h::t -> f h (fold_left f a t);;


let rec fold_right f l a =
  match l with
   [] -> a
 | h::t -> f h (fold_right f t a);;


(*
let rec find a l =
if l=[] then failwith "ERROR"
else	if(a (hd l)) then a 
		else (find a (tl l));; 
*)		
		
let rec find p = function
  [] -> raise Not_found
| h::t -> if p h then h else find p t;;		


(*
let rec for_all a l = match l with
 [] -> true
| h::t -> if (a h) then (for_all a t) 
			else false;;
*)


let rec for_all p = function
  [] -> true
| h::t -> (p h) && (for_all p t);;


let rec exists p = function
  [] -> false
| h::t -> (p h) || (exists p t);;

(*
let rec mem a l =
if l=[] then false
else	if (a = (hd l)) then true
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


hd lista;;
tl lista;;
length lista;;
nth lista 2;;
append lista lista2;;
rev lista;;
rev_append lista lista2;;
concat [[1;2];[55;78]];;
flatten [lista;lista2];;	
map sqrt [4.0;16.0];;
map2 (-) [1;2] [5;6];;
fold_left (+) 3 [4;2;5];;
fold_right (+) [4;2;5] 3;;
find (fun x -> x = 2) [1;2;3];;
for_all (fun x -> x=1) [1;1;1;2];;
exists (fun x -> x=4) [1;2;3];;
mem 4 [1;2;3];;
filter (fun x -> x=1) [1;1;2];;
find_all (fun x -> x=1) [1;1;2];;
partition (fun x -> x=1) [1;1;2];;
split [(1,2);(3,4);(6,9)];;
combine [1;2;3] [3;4;5];;