
type diccionario;;

exception Variable_no_asignada of string;;

val diccionario_vacio : diccionario;;
val consultar_valor : diccionario -> string -> float;;
val asignar_valor : diccionario -> string -> float -> diccionario;;
val mostrar_diccionario : diccionario -> unit;;

