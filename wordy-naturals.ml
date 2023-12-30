(* wordy-naturals.ml *)

type _ word = Eps
            | Header : ('finite * 'finite word) -> 'finite word

type zero_one = Z | O

type nat = Zero
         | OneHead of zero_one word

(* the bitwise xor function for zero_one words with heads successively compared.
TODO: make this function tail-recursive*)
let rec bwxor w1 w2 = match w1 with 
  | Eps -> w2
  | Header (x,w1') -> match w2 with
    | Eps -> w1
    | Header (y,w2') -> let w3 = bwxor w1' w2' in match x with
      | Z -> (match y with
        | Z -> Header (Z,w3)
        | O -> Header (O,w3))
      | O -> (match y with
        | Z -> Header (O,w3)
        | O -> Header (Z,w3))

let rec bwand w1 w2 = match w1 with 
  | Eps -> w2
  | Header (x,w1') -> match w2 with
    | Eps -> w1
    | Header (y,w2') -> let w3 = bwand w1' w2' in match x with
      | Z -> Header (Z,w3)
      | O -> match y with
        | Z -> Header (Z,w3)
        | O -> Header (O,w3)
         

let ltsft : (zero_one word -> zero_one word) = fun w -> Header (Z, w)
let rtsft : (zero_one word -> zero_one word) = fun w -> match w with | Eps -> Eps | Header (c, w') -> w'


(* this function is just wrong atm *)
let rec (+.) n m = match n with
  | Zero -> m
  | OneHead w1 -> match m with
    | Zero -> n
    | OneHead w2 -> OneHead ((bwxor w1 w2) +. ltsft (bwand w1 w2))

;;
print_int 1;;