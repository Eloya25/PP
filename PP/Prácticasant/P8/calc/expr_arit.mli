
open Diccionario;;

type operador_binario = Suma | Resta | Producto | Division;;

type operador_unario = Opuesto | Funcion of string;;

type expresion_aritmetica =
  Constante of float
| Variable of string
| Operacion_unaria of operador_unario * expresion_aritmetica
| Operacion_binaria of operador_binario * expresion_aritmetica * expresion_aritmetica;;

val evaluar_expresion : diccionario -> expresion_aritmetica -> float;;

