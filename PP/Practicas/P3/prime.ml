let is_prime n = let rec check_from i = i >= n || (n mod i <> 0 && check_from (i+1)) in check_from 2;;

let next_prime n = let rec is__prime i = if is_prime i then i else is__prime (i+1) in is__prime (n+1);;                

let last_prime_to n = let rec isprime i = if is_prime i then i else isprime (i-1) in isprime n;;

let is_prime2 n = let rec check_from i = i >= int_of_float (sqrt (float_of_int n))  ||  (n mod i <> 0 && check_from (i+1)) in check_from 2;;
