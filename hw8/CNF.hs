module CNF where
import Expr

data CNF = List (CNF, Integer) CNF | Atom Integer | Nil deriving (Eq)
instance Show CNF where
  show (List (c', n) p) = show n ++ "*w^(" ++ show c' ++ ") + " ++ show p
  show (Atom n) = show n
  show Nil = "nil"

first :: CNF -> (CNF, Integer)
first Nil = (Nil, 0)
first (Atom a) = (Atom a, 1)
first (List a _) = a

rest :: CNF -> CNF
rest Nil        = Nil
rest (Atom _)   = Nil
rest (List _ b) = b

firstn :: CNF -> Integer -> CNF
firstn _ 0           = Nil
firstn a@(Atom _) _  = a
firstn (List x xs) n = List x (firstn xs (n - 1))
firstn _ _ = error "impossible"

restn :: CNF -> Integer -> CNF
restn a 0           = a
restn (Atom _) _    = Nil
restn (List _ xs) n = restn xs (n - 1)
restn _ _ = error "impossible"

fe :: CNF -> CNF
fe (Atom _)   = Atom 0
fe a          = fst $ first a

fc :: CNF -> Integer
fc (Atom a)   = a
fc a          = snd $ first a

atom :: CNF -> Bool
atom (Atom _) = True
atom _        = False

len :: CNF -> Integer
len Nil       = 0
len (Atom _)  = 0
len a         = 1 + len (rest a)

size :: CNF -> Int
size Nil      = 0
size (Atom _) = 1
size a        = size (fe a) + size (rest a)

append :: CNF -> CNF -> CNF
append Nil b      = b
append a Nil      = a
append (Atom _) b = b
append a b        = List (first a) (append (rest a) b)

cmp :: CNF -> CNF -> Ordering
cmp Nil Nil            = EQ
cmp Nil _              = LT
cmp _ Nil              = GT
cmp a (List (_, 0) bs) = a `cmp` bs
cmp (List (_, 0) as) b = as `cmp` b
cmp (Atom a) (Atom b)  = a `compare` b
cmp (Atom _) _         = LT
cmp _ (Atom _)         = GT
cmp (List (a1, a2) as) (List (b1, b2) bs)
    | cmp a1 b1 /= EQ  = a1 `cmp` b1
    | a2 /= b2         = a2 `compare` b2
    | otherwise        = as `cmp` bs

add :: CNF -> CNF -> CNF
add Nil b                   = b
add a Nil                   = a
add (Atom a) (Atom b)       = Atom (a + b)
add a b
    | fe a `cmp` fe b == LT = b
    | fe a `cmp` fe b == EQ = List (fe a, fc a + fc b) (rest b)
    | otherwise             = List (fe a, fc a) (rest a `add` b)

sub :: CNF -> CNF -> CNF
sub (Atom a) (Atom b)
    | a < b                 = Atom 0
    | otherwise             = Atom $ a - b
sub a b
    | fe a `cmp` fe b == LT = Atom 0
    | fe a `cmp` fe b == GT = a
    | fc a < fc b           = Atom 0
    | fc a > fc b           = List (fe a, fc a - fc b) (rest a)
    | otherwise             = rest a `sub` rest b

c :: CNF -> CNF -> Integer
c a b
  | fe b `cmp` fe a == LT = 1 + c (rest a) b
  | otherwise             = 0

count :: CNF -> CNF -> Integer -> Integer
count a b n = n + c (restn a n) b

padd :: CNF -> CNF -> Integer -> CNF
padd a b n = append (firstn a n) (restn a n `add` b)

pmult :: CNF -> CNF -> Integer -> CNF
pmult Nil Nil _           = Atom 0
pmult (Atom a) (Atom b) _ = Atom (a * b)
pmult a (Atom b) _        = List (fe a, fc a * b) (rest a)
pmult a b n
    = List (padd (fe a) (fe b) m, fc b) (pmult a (rest b) m)
                        where m = count (fe a) (fe b) n
mul :: CNF -> CNF -> CNF
mul a b = pmult a b 0

rest' (List _ (Atom n)) = n

exp1 p b
     | cmp (fe b) (Atom 1) == EQ
         = List (Atom $ fc b, p ^ rest' b) (Atom 0)
     | atom (rest b)
         = let e = List (sub (fe b) (Atom 1), fc b) (Atom 0) in
               List (e, p ^ rest' b) (Atom 0)
     | otherwise = List (List (sub (fe b) (Atom 1), 1) (fe c), fc c) (Atom 0)
                       where c = exp1 p (rest b)

exp2 a 1 = a
exp2 a q = mul (List (mul (fe a) (Atom $ q - 1), 1) (Atom 0)) a

limitp (Atom a) = a == 0
limitp a        = limitp $ rest a

limitpart (Atom a) = Atom 0
limitpart a        = List (first a) (limitpart $ rest a)

natpart (Atom a) = a
natpart a        = natpart $ rest a

helper_exp3 a p n 0 = Atom p
helper_exp3 a p n q = padd (mul (exp2 a q) (Atom p)) (helper_exp3 a p n (q - 1)) n

exp3 a 0 = Atom 1
exp3 a 1 = a
exp3 a q
    | limitp a  = exp2 a q
    | otherwise = padd (exp2 c q) (helper_exp3 c (natpart a) n (q-1)) n
      where
        c = limitpart a
        n = len a

exp4 a b = mul (List (mul (fe a) (limitpart b), 1) (Atom 0)) (exp3 a (natpart b))

exp :: CNF -> CNF -> CNF
exp (Atom 1) _        = Atom 1
exp _ (Atom 0)        = Atom 1
exp (Atom 0) _        = Atom 0
exp (Atom a) (Atom b) = Atom $ a ^ b
exp (Atom a) b        = exp1 a b
exp a (Atom b)        = exp3 a b
exp a b               = exp4 a b

expr2CNF :: Expr -> CNF
expr2CNF (Ord n)    = Atom n
expr2CNF Limit      = List (Atom 1, 1) (Atom 0)
expr2CNF (Pow l r)  = expr2CNF l `CNF.exp` expr2CNF r
expr2CNF (Mul l r)  = expr2CNF l `mul` expr2CNF r
expr2CNF (Plus l r) = expr2CNF l `add` expr2CNF r
