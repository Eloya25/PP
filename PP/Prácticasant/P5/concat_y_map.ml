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


concat [[1;2];[55;78];[4;6;8]];;
concat [listagrande;listagrande2];;
concat [[1;2];[];[4;6;8]];;
map sqrt [1.0;3.0;5.0];;
(*map sqrt listagrande;;*)
map sqrt [];;
List.map sqrt [1.0;3.0;5.0];;
List.map sqrt [];;
