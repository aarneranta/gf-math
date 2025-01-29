open import BaseConstants

postulate prop10 : Proof (even 0)

postulate prop20 : (n : Elem Nat) -> Proof (even n) -> Proof (odd (plus n 1))

postulate prop30 : (n : Elem Nat) -> Proof (odd n) -> Proof (even (plus n 1))

postulate prop40 : Proof (foral Nat (\ n -> or (even n) (odd n)))

postulate prop50 : Proof (foral Nat (\ n -> not (and (even n) (odd n))))

postulate prop60 : Proof (foral Nat (\ n -> Eq (plus n 0) n))

postulate prop70 : Proof (odd 1)

postulate prop80 : Proof (even 2)

postulate prop90 : (n : Elem Nat) -> Proof (if (and (even n) (prime n)) (Eq n 2))

postulate prop100 : (a : Elem Int) -> (b : Elem Int) -> (c : Elem Int) -> Proof (Eq (plus (plus a b) c) 0) -> Proof (Lt (times (times a b) c) 0)

postulate prop110 : (a : Elem Int) -> (c : Elem Int) -> Proof (and (odd a) (odd c)) -> Proof (foral Int (\ b -> even (plus (times a b) (times b c))))

postulate prop120 : (a : Elem Int) -> (b : Elem Int) -> Proof (iff (even (times a b)) (or (even a) (even b)))

postulate prop130 : (x : Elem Int) -> (y : Elem Int) -> Proof (if (odd (times x y)) (and (odd x) (odd y)))

same_parity : Elem Int -> Elem Int -> Prop

same_parity = \ x -> \ y -> or (and (even x) (even y)) (and (odd x) (odd y))

