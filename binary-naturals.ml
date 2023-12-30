(* binary-ℕ.ml *)

type pos_nat = One
             | ShiftZ of pos_nat
             | ShiftO of pos_nat

type nat = Zero
         | Nat of pos_nat

let rec pos_succ m = match m with
  | One -> ShiftZ One
  | ShiftZ m' -> ShiftO m'
  | ShiftO m' -> ShiftZ (pos_succ m')

let succ n = match n with
  | Zero -> Nat One
  | Nat n' -> Nat (pos_succ n')

let rec (++) n m = match n with
  | One -> pos_succ m
  | ShiftZ n' -> (match m with
    | One -> pos_succ n
    | ShiftZ m' -> ShiftZ (n' ++ m')
    | ShiftO m' -> ShiftO (n' ++ m'))
  | ShiftO n' -> (match m with
    | One -> pos_succ n
    | ShiftZ m' -> ShiftO (n' ++ m')
    | ShiftO m' -> ShiftZ (pos_succ (n' ++ m')))

let (+.) n m = match n with
  | Zero -> m
  | Nat n' -> match m with
    | Zero -> n
    | Nat m' -> Nat (n' ++ m')

let rec string_of_pos_nat curr n = match n with
  | One -> "1" ^ curr
  | ShiftZ n' -> string_of_pos_nat ("0"^curr) n'
  | ShiftO n' -> string_of_pos_nat ("1"^curr) n'

let string_of_nat n = match n with
  | Zero -> "0"
  | Nat n' -> string_of_pos_nat "" n'

let print_nat n = print_endline (string_of_nat n)

let zero = Zero
let one = Nat One
let two = succ one
let three = succ two
let five = two +. three

;;
print_nat zero;;
print_nat one;;
print_nat two;;
print_nat three;;
print_nat five;;
print_nat (five +. three)