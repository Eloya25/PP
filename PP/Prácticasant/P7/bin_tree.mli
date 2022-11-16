type 'a bin_tree =
	Empty
	| Node of 'a * 'a bin_tree * 'a bin_tree

exception Ramas

val empty : 'a bin_tree
val comp : 'a -> 'a bin_tree * 'a bin_tree -> 'a bin_tree
val raiz : 'a bin_tree -> 'a
val ramas : 'a bin_tree -> 'a bin_tree * 'a bin_tree
val is_empty : 'a bin_tree -> bool
val izq : 'a bin_tree -> 'a bin_tree
val dch : 'a bin_tree -> 'a bin_tree
val size : 'a bin_tree -> int
val height : 'a bin_tree -> int
val preorder : 'a bin_tree -> 'a list
val postorder : 'a bin_tree -> 'a list
val inorder : 'a bin_tree -> 'a list
val leafs : 'a bin_tree -> 'a list
val mirror : 'a bin_tree -> 'a bin_tree
val treemap : ('a -> 'b) -> 'a bin_tree -> 'b bin_tree
