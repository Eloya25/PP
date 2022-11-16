let rec power x y = if y=0 then 1 else if y=1 then x else x*(power x (y-1));;

let rec power' x y = if y=0 then 1 else if y=1 then x else if y mod 2 = 0 then (power' (x*x) (y/2)) else x*(power' (x*x) (y/2));;

(*Seria mejor power' deberia ser mas eficiente que power ya que realizaria menos operaciones que power ya que la y en vez de ir de 1 en 1 se reduce a la mitad pero cuando compruebas los tiempos te da que power da un tiempo menor y esto sucede en tiempos muy pequeños ya que la operacion power' realiza dos operaciones tanto para obtener la x como para obtener la y y power la x ya se da dada y solo se calcularía la y *)

let rec powerf x n = if n=0 then 1. else if n=1 then x else if n mod 2 = 0 then (powerf (x *. x) (n/2)) else x *. (powerf (x *. x) (n/2));;
