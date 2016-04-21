type nat = { apply : 'a . ('a -> 'a) -> 'a -> 'a }

let zero : nat = { apply = fun f x -> x }

let succ : nat -> nat = fun n -> { apply = fun f x -> f (n.apply f x ) }

let rec of_int (n : int) =
    if n = 0 then zero else succ (of_int (n - 1))

let rec to_int (n : nat) = n.apply ((+) 1) 0

let add (m : nat) (n : nat) = m.apply succ n

let times (m : nat) (n : nat) = m.apply (add n) zero

let step (f: nat -> nat -> nat) ((a: nat), (b : nat)) = (f a (succ b), succ b)

let r (n : nat) (f : nat -> nat -> nat) (m : nat) = fst (m.apply (step f) (n, zero))

let fact (x : nat) = r (of_int 1) times x

module Test = struct
    Printf.printf "R n f 0 tests:\n";
    for n = 0 to 5 do
        Printf.printf "R %i add 0 = %i\n" n (to_int (r (of_int n) add zero))

    done;
    Printf.printf "\n";
    Printf.printf "R 1 f m tests:\n";
    for m = 0 to 5 do
        Printf.printf "R 0 add %i = %i\n" m (to_int (r (of_int 0) add (of_int m)))
    done;
    Printf.printf "\n";
    Printf.printf "fact tests:\n";
    for n = 0 to 5 do
        Printf.printf "fact %i = %i\n" n (to_int (fact (of_int n)));
    done
end
