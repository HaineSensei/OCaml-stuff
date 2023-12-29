(* 3.6.1 Prime Number *)

let rec (|!) n r = if r mod n = 0 then true else false

let bool_to_int b = if b then 1 else 0
let rec is_prime_inner n r = if r<2 then true else
                             if r |! n then false else
                             is_prime_inner n (r-1);;


print_int (bool_to_int (3 |! -4));;
print_endline "";;

let is_prime n = if n<2 then false else is_prime_inner n (n-1);;

print_int (bool_to_int (is_prime 24));;
print_endline "";;

(* 3.6.2 Naive Fibonacci *)
let rec fib n = if n=0 then 0
           else if n=1 then 1
           else if n>1 then fib (n-1) + fib (n-2)
           else fib (n+2) - fib (n+1);;

print_int (fib 27);;
print_endline "";;

(* 3.6.3 Quick Fibonacci *)
let neg_power n = if (n mod 2 = 0) then -1 else 1
let fib_step a b = (b,a+b)
let rec super_fib n a b = if n<0 then (neg_power n) * super_fib (-n) a b
                     else if n=0 then a
                     else match fib_step a b with (x, y) -> super_fib (n-1) x y

let xyz_to_yzx f y z x = f x y z
let fib = xyz_to_yzx super_fib 0 1;;

print_int (fib 200);;
print_endline "";;
