
open Diccionario;;
open Expr_arit;;

type instruccion =
  Asignacion of string * expresion_aritmetica
| Calculo of expresion_aritmetica
| Comando of string;;

exception Fin_de_ejecucion;;

exception Comando_no_implementado of string;;

val ejecutar_instruccion : diccionario -> instruccion -> diccionario;;

