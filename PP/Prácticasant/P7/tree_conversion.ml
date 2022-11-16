
(*CAMBIAR ESTO PARA ENTREGAR*)
#load "st_tree.cmo";;
#load "bin_tree.cmo";;



let stV = St_tree.single "unico";;

let stC1 = St_tree.comp "SEMICOMPLETO1" (St_tree.single "nodo1", St_tree.single "nodo2");;
let stC2 = St_tree.comp "SEMICOMPLETO2" (St_tree.single "nodo3", St_tree.single "nodo4");;
let stC3 = St_tree.comp "SEMICOMPLETO3" (St_tree.single "nodo5", St_tree.single "nodo6");;
let stC4 = St_tree.comp "SEMICOMPLETO4" (stC1, stC2);;

let stC = St_tree.comp "COMPLETO" (stC4, stC3);;


let binV = Bin_tree.Empty;;
let binC = Bin_tree.Node ("COMPLETO", 
									(Bin_tree.Node ("Rama1", 
															Bin_tree.Node ("SubRama1.1", 
																					Bin_tree.Node ("Hoja1.1.1", Bin_tree.Empty, Bin_tree.Empty),
																					Bin_tree.Node ("Hoja1.1.2", Bin_tree.Empty, Bin_tree.Empty)),
															Bin_tree.Node ("SubRama1.2", Bin_tree.Empty, Bin_tree.Empty))),
									(Bin_tree.Node ("Rama2", Bin_tree.Empty, Bin_tree.Empty)));;


open Bin_tree;;
open St_tree;;
	
let rec st_tree_of_bin_tree = function
    Empty -> raise (Invalid_argument "st_tree_of_bin_tree")
  | Node (r, Empty, Empty) -> single r
  | Node (r,i,d) -> comp r (st_tree_of_bin_tree i, st_tree_of_bin_tree d);;

let is_single t = 
  try ramas t ; false 
  with Ramas -> true
;;

let rec bin_tree_of_st_tree t =
  if is_single t
  	then Node (raiz t, Empty, Empty)
  else let i,d = ramas t 
	in Node (raiz t, bin_tree_of_st_tree i, bin_tree_of_st_tree d);;


	



bin_tree_of_st_tree stV;;
bin_tree_of_st_tree stC;;
st_tree_of_bin_tree binV;;
st_tree_of_bin_tree binC;;

