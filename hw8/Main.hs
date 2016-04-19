module Main where
import CNF
import Parser

solve :: String -> String
solve line = line ++ ":" ++
  case parseLine line of
    Right (e1, e2) -> if expr2CNF e1 `cmp` expr2CNF e2 == EQ then "Равны"
                      else "Не равны"
    Left err -> show err

process :: [String] -> String
process ll = foldl1 (\x y -> x++"\n"++y) $ map solve ll


main :: IO ()
main = do
  context <- readFile "task8.in"
  let res = case lines context of
              [] -> ""
              dat -> process dat
  writeFile "task8.out" res
