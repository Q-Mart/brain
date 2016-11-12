main :: IO()
main = do input <- getLine
          putStrLn $ dispatch input

dispatch :: String -> String
dispatch x = foldr (\c acc -> c:'\n':acc) "" x
