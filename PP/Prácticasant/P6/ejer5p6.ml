let fromto m n = 
	let rec aux l i =
		if i<m then l
		else aux (i::l) (i-1)
	in aux [] n;;
	
	
let crono f x =
	let t= Sys.time() in
	let _ = f x in
	Sys.time () -. t;;
	
	
let rlist_t r n =
	let rec aux r n acc = 
		if n<=0 then acc
		else aux r (n-1) (Random.int r ::acc)
	in aux r n [];;
	
	
let rec insert x = function
	[] -> [x]
	| h::t -> if x<= h then x::h::t
				else h::insert x t;;
				
let insert_t x l =
	let rec aux x l l2 flag = match l,flag with
		_,false -> List.rev_append l2 l
		| [],_ -> List.rev_append  l2 [x]
		| h::t, true -> if x<=h then aux x t (h::x::l2) false
					else aux x t (h::l2) true
	in aux x l [] true;;

(*----------------------------------------------------------*)

		
		(*ISORT*)
let rec isort = function
	[] -> []
	| h::t -> insert h (isort t);;


let isort_t l = 
	let rec aux l acc = match l with
		[] -> acc
		| h::t -> aux t (insert_t h acc)
	in aux l [];;


(*----------------------------------------------------------*)

let rec divide = function
	h1::h2::t -> let t1,t2 = 
					divide t
				in h1::t1, h2::t2
	| l -> l, [];;

let divide_t l =
  let rec aux acc acc2 = function
					  [h] -> (List.rev (h::acc), List.rev acc2)
					| [] -> (List.rev acc, List.rev acc2)
					| h::t -> aux (h::acc) (List.hd t::acc2) (List.tl t)
				in aux [] [] l;;		


let rec merge l1 l2 = match l1,l2 with
	[], l | l,[] -> l
	| h1::t1, h2::t2 -> if h1<=h2 then h1::merge t1 l2
						else h2::merge l1 t2;;
						
						
let merge_t l1 l2 = match l1,l2 with
	[], l | l,[] -> l
	| [], [] -> []
	| h1::t1, h2::t2 -> let rec aux l1 l2 acc = match l1,l2 with
							[h], [] | [], [h] -> List.rev (h::acc)
							| [], h::t -> aux [] t (h::acc)
							| h::t,[] -> aux t [] (h::acc)
							| h1::t1, h2::t2 -> if h1<=h2 then aux t1 l2 (h1::acc)
												else aux l1 t2 (h2::acc)
						in aux l1 l2 [];;
(*----------------------------------------------------------*)						
	
	
	(*MSORT*)					
let rec msort l = match l with
	[] | [_] -> l
	| _ -> let l1,l2 = 
				divide l 
			in merge (msort l1) (msort l2);;


let rec msort_qt l = match l with
	[] -> []
	| [h] -> [h]
	| _ -> let drch, izq =
				divide_t l
			in merge_t (msort_qt drch) (msort_qt izq);;


(*----------------------------------------------------------*)

	(*QSORT*)
let rec qsort l = match l with
	[] | [_] -> l
	| h::t-> let l1,l2 = 
				List.partition ((<=) h) t 
			in qsort l2 @ (h::qsort l1);;
		

let rec qsort_qt l = match l with
	[] | [_] -> l
	| h::t-> let l1,l2 = 
				List.partition ((<=) h) t 
			in let append l1 l2 =
				List.rev_append (List.rev l1) l2
			in append (qsort_qt l2) (h::qsort_qt l1);;

(*----------------------------------------------------------*)	

	
	
	(*Ejercicio 5*)
let listaordenada2 = fromto 1 2000;;
let listaordenada4 = fromto 1 4000;;  
let listaordenada8 = fromto 1 8000;;
let listaordenada16 = fromto 1 16000;; 
let listaordenada32 = fromto 1 32000;;
let listaordenada64 = fromto 1 64000;;
let listaordenada128 = fromto 1 128000;; 
let listaordenada256 = fromto 1 256000;;
let listaordenada512 = fromto 1 512000;;

let listadesordenada2 = rlist_t 2000 2000;;
let listadesordenada4 = rlist_t 4000 4000;;  
let listadesordenada8 = rlist_t 8000 8000;;
let listadesordenada16 = rlist_t 16000 16000;; 
let listadesordenada32 = rlist_t 32000 32000;;
let listadesordenada64 = rlist_t 64000 64000;;
let listadesordenada128 = rlist_t 128000 128000;; 
let listadesordenada256 = rlist_t 256000 256000;;
let listadesordenada512 = rlist_t 512000 512000;; 


	(*ISORT*)

print_endline "ISORT";;
print_endline "isort ordenado";;

crono isort listaordenada2;;
crono isort listaordenada4;;
crono isort listaordenada8;;
crono isort listaordenada16;;
crono isort listaordenada32;;
crono isort listaordenada64;;
crono isort listaordenada128;;
crono isort listaordenada256;;
crono isort listaordenada512;;

print_endline "isort desordenado";;

crono isort listadesordenada2;;
crono isort listadesordenada4;;
crono isort listadesordenada8;;
crono isort listadesordenada16;;
crono isort listadesordenada32;;
crono isort listadesordenada64;;
(*
crono isort listadesordenada128;;
crono isort listadesordenada256;;
crono isort listadesordenada512;;
*)

(***********************************************************)

print_endline "isort_t ordenado";;

crono isort_t listaordenada2;;
crono isort_t listaordenada4;;
crono isort_t listaordenada8;;
crono isort_t listaordenada16;;
crono isort_t listaordenada32;;
(*crono isort_t listaordenada64;;
crono isort_t listaordenada128;;
crono isort_t listaordenada256;;
crono isort_t listaordenada512;;
*)

print_endline "isort_t desordenado";;

crono isort_t listadesordenada2;;
crono isort_t listadesordenada4;;
crono isort_t listadesordenada8;;
crono isort_t listadesordenada16;;
crono isort_t listadesordenada32;;
crono isort_t listadesordenada64;;
(*
crono isort_t listadesordenada128;;
crono isort_t listadesordenada256;;
crono isort_t listadesordenada512;;
*)

(***********************************************************)
(***********************************************************)

	(*MSORT*)
	
print_endline "MSORT";;
print_endline "msort ordenado";;

crono msort listaordenada2;;
crono msort listaordenada4;;
crono msort listaordenada8;;
crono msort listaordenada16;;
crono msort listaordenada32;;
crono msort listaordenada64;;
crono msort listaordenada128;;
crono msort listaordenada256;;
crono msort listaordenada512;;


print_endline "msort desordenado";;

crono msort listadesordenada2;;
crono msort listadesordenada4;;
crono msort listadesordenada8;;
crono msort listadesordenada16;;
crono msort listadesordenada32;;
crono msort listadesordenada64;;
crono msort listadesordenada128;;
crono msort listadesordenada256;;
crono msort listadesordenada512;;

(***********************************************************)

print_endline "msort_qt ordenado";;

crono msort_qt listaordenada2;;
crono msort_qt listaordenada4;;
crono msort_qt listaordenada8;;
crono msort_qt listaordenada16;;
crono msort_qt listaordenada32;;
crono msort_qt listaordenada64;;
crono msort_qt listaordenada128;;
crono msort_qt listaordenada256;;
crono msort_qt listaordenada512;;

print_endline "msort_qt desordenado";;

crono msort_qt listadesordenada2;;
crono msort_qt listadesordenada4;;
crono msort_qt listadesordenada8;;
crono msort_qt listadesordenada16;;
crono msort_qt listadesordenada32;;
crono msort_qt listadesordenada64;;
crono msort_qt listadesordenada128;;
crono msort_qt listadesordenada256;;
crono msort_qt listadesordenada512;;



(***********************************************************)
(***********************************************************)

	(*QSORT*)
	
print_endline "QSORT";;
print_endline "qsort ordenado";;

crono qsort listaordenada2;;
crono qsort listaordenada4;;
crono qsort listaordenada8;;
crono qsort listaordenada16;;
(*
crono qsort listaordenada32;;  #Killed
crono qsort listaordenada64;;
crono qsort listaordenada128;;
crono qsort listaordenada256;;
crono qsort listaordenada512;;
*)

print_endline "qsort desordenado";;

crono qsort listadesordenada2;;
crono qsort listadesordenada4;;
crono qsort listadesordenada8;;
crono qsort listadesordenada16;;
crono qsort listadesordenada32;;
crono qsort listadesordenada64;;
crono qsort listadesordenada128;;
crono qsort listadesordenada256;;
crono qsort listadesordenada512;;

(***********************************************************)

print_endline "qsort_qt ordenado";;

crono qsort_qt listaordenada2;;
crono qsort_qt listaordenada4;;
crono qsort_qt listaordenada8;;
crono qsort_qt listaordenada16;;
(*
crono qsort_qt listaordenada32;;  #Killed
crono qsort_qt listaordenada64;;
crono qsort_qt listaordenada128;;
crono qsort_qt listaordenada256;;
crono qsort_qt listaordenada512;;
*)

print_endline "qsort_qt desordenado";;

crono qsort_qt listadesordenada2;;
crono qsort_qt listadesordenada4;;
crono qsort_qt listadesordenada8;;
crono qsort_qt listadesordenada16;;
crono qsort_qt listadesordenada32;;
crono qsort_qt listadesordenada64;;
crono qsort_qt listadesordenada128;;
crono qsort_qt listadesordenada256;;
crono qsort_qt listadesordenada512;;
