type 'a bin_tree =
	Empty
	| Node of 'a * 'a bin_tree * 'a bin_tree;;

exception Ramas;;


let empty = Empty;;


let comp r (i,d) = Node(r,i,d);;


let raiz = function
	Empty -> raise Ramas
	| Node(r,_,_) -> r;;
	
	
	
let ramas = function
	Empty -> raise Ramas
	| Node (_,i,d) -> (i,d);;
	
	
	
let is_empty = function
	Empty -> true
	| _ -> false;;
	
	
let izq = function
	Empty -> raise Ramas
	| Node (_,i,_) -> i;;

let dch = function
	Empty -> raise Ramas
	| Node (_,_,d) -> d;;


let rec size = function
	Empty -> 0
	| Node (r,i,d) -> (size i) + (size d) + 1;;
	
	
let rec height = function
	Empty -> 0
	| Node (r,i,d) -> (max (height i) (height d)) +1;;
	
	
let rec preorder = function
	Empty -> []
	| Node (r,i,d) -> r::(preorder i) @ (preorder d);;
	

let rec postorder = function
	Empty -> []
	| Node (r,i,d) -> (postorder i) @ (postorder d) @ [r];;
	
let rec inorder = function
	Empty -> []
	| Node (r,i,d) -> ((inorder i) @ [r]) @ (inorder d);;
	
let rec leafs = function
	Empty -> []
	| Node (r,Empty,Empty) -> [r]
	| Node (r,i,d) -> leafs i @ leafs d;;
	
let rec mirror = function
	Empty -> Empty
	| Node (r,i,d) -> Node (r,d,i);;
	
let rec treemap f = function
	Empty -> Empty
	| Node (r,i,d) -> Node(f r, treemap f i, treemap f d);;
	

	
let arbolV = empty;;
let arbolConstr x = Node (x, Empty, Empty);; 
let arbolC = Node ("saludos", arbolConstr "hola", arbolConstr "adios");;

let arbolC2 = comp "SALUDOS" (arbolC, arbolC);;
let arbolC3 = comp "otra cosa" (arbolConstr "son", arbolConstr "otras");;
let arbolC4 = comp "COMPLETO" (arbolC2, arbolC3);;
let arbolC5 = comp 16. (arbolConstr 77., arbolConstr 1.);;
let arbolC6 = comp 4. (arbolConstr 1.,arbolConstr 6.);;
let arbolC7 = comp 32. (arbolC5, arbolC6);;

comp 5 (Node (2, Empty, Empty) , Node (4, Empty,Empty));;
raiz arbolV;;
raiz arbolC;;
raiz arbolC2;;
is_empty arbolV;;
is_empty arbolC;;
ramas arbolV;;
ramas arbolC;;
izq arbolV;;
izq arbolC4;;
dch arbolV;;
dch arbolC4;;
size arbolV;;
size arbolC4;;
height arbolV;;
height arbolC4;;
preorder arbolV;;
preorder arbolC4;;
postorder arbolV;;
postorder arbolC4;;
inorder arbolV;;
inorder arbolC4;;
leafs arbolV;;
leafs arbolC4;;
mirror arbolV;;
mirror arbolC4;;
treemap (sqrt) arbolV;;
treemap (sqrt) arbolC7;;
