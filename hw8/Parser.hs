module Parser where
import Text.ParserCombinators.Parsec
import Expr

stringToInt :: String -> Integer
stringToInt str = fst (head (reads str :: [(Integer, String)]))

lim :: Parser Expr
lim = do
  _ <- spaces *> char 'w' <* spaces
  return Limit

ord :: Parser Expr
ord = do
  p <- spaces *> many1 digit <* spaces
  return $ Ord $ stringToInt p

braces :: Parser Expr
braces = spaces *> char '(' *> expr <* char ')' <* spaces

term :: Parser Expr
term = try ord <|> (try lim <|> braces)

expr :: Parser Expr
expr = do
  x <- spaces *> plus
  xs <- many ((spaces *> char '+' <* spaces) >> plus) <* spaces
  return $ foldl1 Plus (x:xs)

--expr :: Parser Expr
--expr = try braces <|> expr1

plus :: Parser Expr
plus = do
  x <- spaces *> mul
  xs <- many ((spaces *> char '*' <* spaces) >> mul) <* spaces
  return $ foldl1 Mul (x:xs)

mul :: Parser Expr
mul = do
  x <- spaces *> term
  xs <- many ((spaces *> char '^' <* spaces) >> term) <* spaces
  return $ foldr1 Pow (x:xs)

line :: Parser (Expr, Expr)
line = do
  l <- expr
  _ <- spaces *> char '=' <* spaces
  r <- expr
  return (l, r)

parseLine :: String -> Either ParseError (Expr, Expr)
parseLine = parse line ""
