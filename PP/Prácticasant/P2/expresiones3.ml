let x = 1;;
(*Salida: val x : int = 1*)

let y = 2;;
(*Salida: val y : int = 2*)

x - y;;
(*Salida: - : int = -1*)

let x = y in x-y;;
(*Salida: -: int = 0*)

x - y;;
(*Salida: -: int = -1*)

(*z;;*)
(*Error de ejecución: no se ha declarado ni dado valor a z*)

let z = 1;;
(*Correción de la expresión anterior*)
(*Salida: val z : int = 1*)

let z = x+y;;
(*Salida: val z : int = 3*)

z;;
(*Salida: -: int = 3*)

let x = 5;;
(*Salida: val x : int = 5*)

z;;
(*Salida: -: int = 3*)

let y = 5 in x+y;;
(*Salida: - : int = 10*)

x + y;;
(*Salida: - : int = 7*)

let p = 2,5;;
(*Salida: val p : int * int = (2, 5)*)

snd p, fst p;;
(*Salida: -:  int * int = (5, 2)*)

p;;
(*Salida: -:  int * int = (2, 5)*)

let p = 0,1 in snd p, fst p;;
(*Salida: - : int * int = (1, 0)*)

p;;
(*Salida: - : int * int = (2, 5)*)

let x,y = p;;
(*Salida: val x : int = 2
	  val y : int = 5*)

let z = x+y;;
(*Salida: val z : int = 7*)

let x,y = p,x;;
(*Salida: -:  val x : int * int = (2, 5)
	      val y : int = 2*)

let x = let x,y = 2,3 in x * x + y;;
(*Salida: val x : int = 7*)

x + y;;
(*Salida: - : int = 9*)

z;;
(*Salida: - : int = 7*)

let x = x + y in let y = x * y in x + y + z;;
(*Salida: -: int = 34*)

x + y + z;;
(*Salida: -: int = 16*)

int_of_float;;
(*Salida: -: float -> int = <fun>*)

float_of_int;;
(*Salida: -: int -> float = <fun>*)

int_of_char;;
(*Salida: -: char -> int = <fun>*)

char_of_int;;
(*Salida: -: int -> char = <fun>*)

abs;;
(*Salida: -: int -> int = <fun>*)

sqrt;;
(*Salida: -: float -> float = <fun>*)

truncate;;
(*Salida: -: float -> int = <fun>*)

ceil;;
(*Salida: -: float -> float = <fun>*)

floor;;
(*Salida: -: float -> float = <fun>*)

Char.code;;
(*Salida: -: char -> int = <fun>*)

String.length;;
(*Salida: -: string -> int = <fun>*)

fst;;
(*Salida: -: 'a * 'b -> 'a = <fun>*)

snd;;
(*Salida: -: 'a * 'b -> 'B = <fun>*)

function x -> 2 * x;;
(*Salida: -: int -> int = <fun>*)

(function x -> 2 * x) (2 + 1);;
(*Salida: -: int = 6*)

function (x,y) -> x;;
(*Salida: -: 'a * 'b -> 'a = <fun>*)

let f = function x -> 2 * x;;
(*Salida: val f : int -> int = <fun>*)

f (2+1);;
(*Salida: -: int = 6*)

f 2 + 1;;
(*Salida: -: int = 5*)

let n = 10;;
(*Salida: -: val n : int = 10*)

let sum n = function x -> n + x;;
(*Salida: -: val sum : int -> int -> int = <fun>*)

sum 5;;
(*Salida: - : int -> int = <fun>*)

sum 1 2;;
(*Salida: -: int = 3*)

let n = 1;;
(*Salida: val n : int = 1*)

sum n 10;;
(*Salida: -: int = 11*)

let sumn = sum n;;
(*Salida: val sumn : int -> int = <fun>*)

sum 100;;
(*Salida: -: int -> int = <fun>*)

let n = 1000;;
(*Salida: -: val n : int = 1000*)

sumn 100;;
(*Salida: -: int = 101*)
