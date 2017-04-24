module Main where

import Control.Concurrent
import GHC.IO.Handle.FD 
import System.IO
import qualified Data.List.Split as DLS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.Map
import Data.Text
import qualified System.Process as SP
import System.Directory
import qualified Options.Applicative as OA
import Data.Monoid

data Clops = Clops
             { testing :: Bool }

cloparser :: OA.Parser Clops
cloparser = Clops
  <$> OA.switch (OA.short 't' <> OA.help "Testing")


keyName :: String -> Parser String
keyName xs = try (choice [alphaNum, (oneOf "_")] >>= (\x -> keyName (x:xs))) <|> return (Prelude.reverse xs)

keyTableRow :: Parser (Maybe (Char,String))
keyTableRow = do
  nm <- keyName ""
  spaces
  many1 alphaNum
  cs <- manyTill anyChar (try newline)
  case (unpack (strip (pack cs))) of
    [c]   -> return (Just (c,nm))
    x:xs  -> case (Prelude.head (unpack (strip (pack xs)))) of
      ';' -> return (Just (x,nm)) -- some lines have comments
      _   -> return Nothing
    _     -> return Nothing

keyTable :: (Map Char String) -> Parser (Map Char String)
keyTable m = try (
  do
    p <- keyTableRow
    case p of
      Nothing     -> keyTable m
      Just (c,nm) -> keyTable (insert c nm m)
  ) <|> return m

xdtArg :: String -> (Map Char String) -> String
xdtArg xs m = do
  x <- xs
  case x of
    ' ' -> " space"
    '\n' -> " Return"
    y   -> case Data.Map.lookup y m of
      Just ks -> ' ':ks
      Nothing -> [' ', y]

execStr :: String -> Bool -> IO ()
execStr xs isTesting = do
  let p = if isTesting then (SP.proc "cat" []) else (SP.proc "xdotool" ["-"])
  (Just hin, _ , _, procHandle) <- SP.createProcess p {SP.std_in = SP.CreatePipe}
  hPutStr hin xs
  hFlush hin
  hClose hin

myOptParser = OA.info (OA.helper <*> cloparser)
           ( OA.fullDesc
             <> OA.progDesc "type whatever comes on STDIN (using xdotool)"
             <> OA.header "auto-keys" )

main :: IO ()
main = do
  clops <- OA.execParser myOptParser
  s <- getContents  -- read text from stdin
  kstbl <- readFile "/usr/local/lib/amkhlv/keysyms.txt"
  case (parse (keyTable Data.Map.empty) "" kstbl) of
    Left _  -> putStrLn "ERROR"
    Right m -> execStr ("key" ++ xdtArg s m) (testing clops)

