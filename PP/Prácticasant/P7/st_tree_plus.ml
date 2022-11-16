open St_tree;;

let is_single t =
	try ramas t ; false
	with Ramas -> true;;

let izq t =
	if (is_single t) then t
	else fst (ramas t);;
	
let dch t =
	if (is_single t) then t
	else snd (ramas t);;
	
let rec size t = 
	if (is_single t) then 1
	else size (dch t) + size (izq t) + 1;;
	
let rec height t =
	if (is_single t) then 1
	else max (height (izq t)) (height (dch t)) +1;;

let rec preorder t =
	if (is_single t) then [raiz t]
	else List.append ((raiz t)::(preorder (izq t))) (preorder (dch t));;

	
let rec postorder t =
	if (is_single t) then [raiz t]
	else (postorder (izq t) @ postorder (dch t)) @ [raiz t];;
	

let rec inorder t =
	if (is_single t) then [raiz t]
	else (inorder (izq t) @ [raiz t]) @ (inorder (dch t));;
	
	
let rec leafs t =
	if (is_single t) then [raiz t]
	else leafs (izq t) @ leafs (dch t);;
	

let rec mirror t =
	if (is_single t) then t
	else comp (raiz t) (mirror (dch t), mirror (izq t));;


let rec treemap f t =
	if (is_single t) then single (f (raiz t))
	else comp (f (raiz t)) (treemap f (izq t), treemap f (dch t));;
	
		
let arbolS = single "hola";;
let arbolS2 = single "adios";;
let arbolC = comp "saludos" (arbolS,arbolS2);;	
let arbolC2 = comp "saludos" (single "hola",single "adios");;
let arbolC2 = comp "SALUDOS" (arbolC, arbolC);;
let arbolC3 = comp "otra cosa" (single "son", single "otras");;
let arbolC4 = comp "COMPLETO" (arbolC2, arbolC3);;
let arbolC6 = comp 4. (single 1.,single 6.);;



is_single arbolS;;		
is_single arbolC;;
izq arbolS;;
izq arbolC;;
size arbolS;;
size arbolC;;
height arbolS;;
height arbolC;;
preorder arbolS;;
preorder arbolC4;;
postorder arbolS;;
postorder arbolC4;;
inorder arbolS;;
inorder arbolC4;;
leafs arbolS;;
leafs arbolC;;
mirror arbolS;;
mirror arbolC;;
treemap (sqrt) arbolC6;;
