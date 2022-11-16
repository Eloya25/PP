type 'a st_tree =
	S of 'a
	| C of 'a * 'a st_tree * 'a st_tree;;
exception Ramas;;

let single e = S e;;
let comp r (i,d) = C (r,i,d);;
let raiz = function S r | C(r,_,_) -> r;;
let ramas = function C(_,i,d) -> (i,d) | _ -> raise Ramas;;
