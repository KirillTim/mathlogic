module CNF where
import Expr

data CNF = List (CNF, Integer) CNF | Atom Integer | Nil deriving (Eq)
instance Show CNF where
  show (List (c, n) p) = show n ++ "*w^(" ++ show c ++ ") + " ++ show p
  show (Atom n) = show n
  show Nil = "nil"

first :: CNF -> (CNF, Integer)
first Nil = (Nil, 0)
first (Atom a) = (Atom a, 1)
first (List a _) = a

rest :: CNF -> CNF
rest Nil = Nil
rest (Atom _) = Nil
rest (List _ b) = b

firstn :: CNF -> Integer -> CNF
firstn _ 0 = Nil
firstn a@(Atom _) _ = a
firstn (List x xs) n = List x (firstn xs (n - 1))
firstn _ _ = error "impossible"

restn :: CNF -> Integer -> CNF
restn a 0 = a
restn (Atom _) _ = Nil
restn (List _ xs) n = restn xs (n - 1)
restn _ _ = error "impossible"

fe :: CNF -> CNF
fe (Atom _) = Atom 0
fe a = fst $ first a

fc :: CNF -> Integer
fc (Atom a) = a
fc a = snd $ first a

atom :: CNF -> Bool
atom (Atom _) = True
atom _ = False

len :: CNF -> Int
len Nil = 0
len (Atom _) = 0
len a = 1 + len (rest a)

size :: CNF -> Int
size Nil = 0
size (Atom _) = 1
size a = size (fe a) + size (rest a)

append :: CNF -> CNF -> CNF
append Nil b = b
append a Nil = a
append (Atom _) b = b
append a b = List (first a) (append (rest a) b)



expr2CNF :: Expr -> CNF
expr2CNF (Ord n)    = Atom n
expr2CNF Limit      = List (Atom 1, 1) (Atom 0)
expr2CNF (Pow l r)  = Atom 100500
expr2CNF (Mul l r)  = Atom 100500
expr2CNF (Plus l r) = Atom 100500
