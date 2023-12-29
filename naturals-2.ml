(* naturals.ml *)

let compose f g x = f (g x)
type nat  = Zero
          | Succ of nat

let rec (+!) x y = match x with
  | Zero -> y
  | Succ n -> n +! Succ y

let zero = Zero
let one = Succ zero

let int_of_nat n = match n with
  | Zero -> 0
  | Succ n' -> let rec inner x m = match m with
    | Zero -> x
    | Succ m' -> inner (succ x) m'
    in inner 1 n'

let print_nat = compose print_int int_of_nat

let two = one +! one
let three = Succ two

let fib = 
  let step a b = (b,a+!b) in
  let rec inner a b n = match n with
    | Zero -> a
    | Succ n' -> match step a b with
      | (a',b') -> inner a' b' n' in
      inner zero one

exception NegativeError
let rec nat_of_int x = if x<0 then raise NegativeError else if x=0 then zero else Succ (nat_of_int (x-1))

let fib_nat = compose fib nat_of_int
;;

print_nat (fib_nat 20)
(*let (+!) x y = match x with
  | Natural xs -> match y with
    | Natural ys -> Natural (xs@ys)

let two = Natural [Zero;Zero]
let three = Natural [Zero] +! two
let string_of_natural x = 
  let rec inner xs = match xs with
    | [] -> ""
    | Zero :: [] -> "0"
    | Zero :: t -> "0;"^inner t
  in match x with
    | Natural xs -> "[" ^ inner xs ^ "]"

let print_natural = print_string * string_of_natural;;

let len =
  let rec inner n xs = match xs with
    | [] -> n
    | h :: t -> inner (n+1) t
  in inner 0
let size a = match a with
  | Natural xs -> len xs

let print_natural2 = print_int * size;;

print_natural (two +! three);;
print_endline "";;
print_natural2(two +! three);;
print_endline "";;

let one = Natural [Zero]
let zero = Natural []

let fib = 
  let step a b = (b,a+!b) in let rec inner a b n =
  match n with
    | Natural ns -> match ns with
      | [] -> a
      | h :: t -> match step a b with
        | (x,y) -> inner x y (Natural t)
  in inner zero one

exception Negative of string
let natural_of_int x = if x < 0 then raise (Negative "natural must be positive.") else
  let rec inner y l = if y = 0 then l else inner (y-1) (Zero :: l) in Natural (inner x [])

;;
print_natural2 (fib (natural_of_int 28));;
print_endline "" 
*)