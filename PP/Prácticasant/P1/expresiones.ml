();;
(*Salida: -: unit = ()*)

2+5*3;;
(*Salida: -: int = 17*)

1.0;;
(*Salida: -: float = 1.*)

(*1.0 * 2;;*)
(*Error de tipo: No se puede operar float por entero*)

1 * 2;;
(*Expresion correcta de la anterior*)
(*Salida: -: int = 2*)


(*2 - 2.0;;*)
(*Error de tipo: No se puede operar float por entero*)

2.0 -. 2.0;;
(*Expresion correcta de la anterior*)
(*Salida: - : float = 0.*)


(*3.0 + 2.0;;*)
(*Error de tipo: Al ser float el operador necesita ir seguido de un punto.*)

3.0 +. 2.0;;
(*Expresion correcta de la anterior*)
(*Salida: - : float = 5.*)


5/3;;
(*Salida: - : int = 1*)


5 mod 3;;
(*Salida: - : int = 2 *)


(*3.0 *. 2.0 ** 3.0;;*)
(*Error de tipo: La multiplicaci�n de float debe de ir en formato "*."*)

3.0 *. 2.0 *. 3.0;;
(*Expresion correcta de la anterior*)
(*Salida: - : int = 18*)


3.0 = float_of_int 3;;
(*Salida: - : bool = true*)


(*sqrt 4;;*)
(*Error de tipo: sqrt opera con formato float*)


sqrt 4.0;;
(*Expresion correcta de la anterior*)
(*Salida: - : float = 2.*)


int_of_float 2.1 + int_of_float (-2.9);;
(*Salida: - : int = 0*)


truncate 2.1 + truncate (-2.9);;
(*Salida: - : int = 0*)


floor 2.1 +. floor (-2.9);;
(*Salida: - : float = -1*)


(*ceil 2.1 +. ceil -2.9;;*)
(*Error sint�ctico: faltan par�ntesis*)

ceil 2.1 +. ceil (-2.9);;
(*Expresion correcta de la anterior*)
(*Salida: - : float = 1.*)


'B';;
(*Salida: - : char = 'B'*)


int_of_char 'A';;
(*Salida: - : int = 65*)


char_of_int 66;;
(*Salida: - : char 'B'*)


Char.code 'B';;
(*Salida: - : int = 66*)


Char.chr 67;;
(*Salida: - : char = 'C'*)


'\067';;
(*Salida: - : char = 'C'*)


Char.chr (Char.code 'a' - Char.code 'A' + Char.code '�');;
(*NOTA: es necesario cambiar la codificaci�n de caracteres del terminal por ISO-8859-15*)
(*Salida: - : char = '\241'*)


Char.uppercase '�';;
(*Salida: - : char = '\209'*)


Char.lowercase 'O';;
(*Salida: - : char = 'o'*)


"this is a string";;
(*Salida: - : string = "this is a string"*)


String.length "longuitud";;
(*Salida: - : int = 9*)


(*"1999" + "1";;*)
(*Error de tipo: no se pueden sumar strings*)

int_of_string "1999" + int_of_string "1";;
(*Expresion correcta de la anterior*)
(*Salida: - : int = 2000*)


"1999" ^ "1";;
(*Salida: - : string = "19991"*)


int_of_string "1999" + 1;;
(*Salida:  - : int = 2000*)


"\064\065";;
(*Salida: - : string = "@A"*)


string_of_int 010;;
(*Salida: - : string = "10"*)


not true;;
(*Salida: - : bool = false*)


true && false;;
(*Salida: - : bool = false*)


true || false;;
(*Salida: - : bool = true*)


(1<2) = false;;
(*Salida: - : bool = false*)


"1" < "2";;
(*Salida: - : bool = true*)


2<12;;
(*Salida: - : bool = true*)


"2"<"12";;
(*Salida: - : bool = false*)
(*La salida est� bien puesto que numericamente la cadena es menor*)


"uno" < "dos";;
(*Salida: - : bool = false*)


2,5;;
(*Salida: - : int * int = (2, 5)*)


"hola", "adios";;
(*Salida: - : string * string = ("hola", "adios")*)


0, 0.0;;
(*Salida: - : int * float = (0, 0.)*)


fst('a',0);;
(*Salida: - : char = 'a'*)


snd (false, true);;
(*Salida: - : bool = true*)


(1,2,3);;
(*Salida: - : int * int * int = (1, 2, 3)*)


(1,2),3;;
(*Salida: - : (int * int) * int = ((1, 2), 3)*)


fst ((1,2),3);;
(*Salida: - : int * int = (1, 2)*)


if 3=4 then 0 else 4;;
(*Salida: - : int = 4*)


if 3=4 then "0" else "4";; 
(*Salida: - : string = "4"*)


(*if 3=4 then 0 else "4";;*)
(*Error de tipo: tienen que ser los 2 resultados del mismo tipo*)

if 3=4 then string_of_int 0 else "4";;
(*Expresion correcta de la anterior*)
(*Salida: - : string = "4"*)


(if 3<5 then 8  else 10) + 4;; 
(*Salida: - : int = 12*)


let pi = 2.0 *. asin 1.0;;
(*Salida: val pi : float = 3.14159265358979312*)


sin (pi/. 2.);;
(*Salida: - : float = 1.*)
