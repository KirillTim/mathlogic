import Text.ParserCombinators.Parsec

data Expr = Brace Expr | Plus Expr Expr | Mul Expr Expr | Pow Expr Expr | Limit | Ord Int
instance Show Expr where
  show (Brace e) = "(" ++ show e ++ ")"
  show (Pow l r) = show l ++ "^(" ++ show r ++ ")"
  show (Mul l r) = show l ++ "*" ++ show r
  show (Plus l r) = show l ++ "+" ++ show r
  show Limit = "W"
  show (Ord i) = show i

stringToInt :: String -> Int
stringToInt str = fst (head (reads str :: [(Int, String)]))

lim :: Parser Expr
lim = do
  spaces *> char 'W' <* spaces
  return Limit

ord :: Parser Expr
ord = do
  spaces
  p <- many1 digit
  spaces
  return $ Ord (stringToInt p)

braces :: Parser Expr
braces = do
  spaces
  char '('
  p <- expr
  char ')'
  spaces
  return $ Brace p

term :: Parser Expr
term = try ord <|> (try lim <|> braces)

expr = do
  spaces
  x <- plus
  xs <- many ((spaces *> char '+' <* spaces) >> plus)
  spaces
  return $ foldl1 Plus (x:xs)

plus = do
  spaces
  x <- mul
  xs <- many ((spaces *> char '*' <* spaces) >> mul)
  spaces
  return $ foldl1 Mul (x:xs)

mul = do
  spaces
  x <- term
  xs <- many ((spaces *> char '^' <* spaces) >> term)
  spaces
  return $ foldr1 Pow (x:xs)


act = parse expr "err"


{-
rule :: Parser Rule
rule = do
    p <- many1 letter
    char ':'
    spaces

    v <- many1 (noneOf ";")
    char ';'
    spaces

    return $ Rule p v -}
