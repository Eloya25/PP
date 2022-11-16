let rec fact = function
	0 -> 1 |
	n -> n * fact (n - 1);;

if Array.length(Sys.argv)=2 then 
try
	print_endline (
		string_of_int(
			fact (
				int_of_string Sys.argv.(1)
			)
		)
	)
with
	  Failure "int_of_string" -> print_endline "Introduza un número"
	| Stack_overflow -> print_endline "Introduza un enteiro positivo"
	else print_endline("Número de argumentos inválido");;
