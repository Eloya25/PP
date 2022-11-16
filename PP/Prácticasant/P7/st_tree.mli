type 'a st_tree
exception Ramas
val single : 'a -> 'a st_tree
val comp : 'a -> 'a st_tree * 'a st_tree -> 'a st_tree
val raiz : 'a st_tree -> 'a
val ramas : 'a st_tree -> 'a st_tree *  'a st_tree
