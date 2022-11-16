(*Char.uppercase es una función que recibe un char y tiene como salida ese mismo char en mayúscula. Salida: - : char -> char = <fun>*)
let uppercase = fun c -> if ((Char.code c < 91) && (Char.code c > 64)) then c else
											if ((Char.code c > 191) && (Char.code c < 222)) then c
												else char_of_int (Char.code c-32);;

(*Char.lowercase es una función que recibe un char y tiene como salida ese mismo char en minúscula. Salida: - : char -> char = <fun>*)
let lowercase = fun c -> if ((Char.code c < 91) && (Char.code c > 64)) then char_of_int (Char.code c + 32) else
																					if ((Char.code c > 191) && (Char.code c < 222)) then char_of_int (Char.code c + 32) 
																					else c;;




						

