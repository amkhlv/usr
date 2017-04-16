import Text.ParserCombinators.Parsec
import NaiveSExp

main :: IO ()
main = do
  example <- readFile "test/test.rkt"
  putStrLn $ case (parse sExpParser "" example) of
    Left _  -> "ERROR: my parser could not parse this!"
    Right x -> show x
  putStrLn "-- finished --"
