module Main where

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

trm :: String -> String
trm x = unpack (strip $ pack x)

keyTableRow :: Parser (Maybe (Char,String))
keyTableRow = 
  try
  ( do 
    nm <- many1 $ choice [alphaNum, (oneOf "_")]
    spaces
    string "0x"
    many1 alphaNum
    cs <- manyTill anyChar (try newline)
    case (trm cs) of
      [c]   -> return $ Just (c,nm)
      x:xs  -> case Prelude.head (trm xs) of
        ';' -> return $ Just (x,nm) -- some lines have comments
        _   -> return Nothing
      _     -> return Nothing
  )
  <|> (manyTill anyChar (try newline) >> return Nothing)

keyTable :: (Map Char String) -> Parser (Map Char String)
keyTable m =
  ( keyTableRow >>= maybe (keyTable m) (\(c,nm) -> keyTable (insert c nm m)) )
  <|> return m

xdtArg :: String -> (Map Char String) -> String
xdtArg xs m = do
  x <- xs
  case x of
    ' '  -> " space"
    '\b' -> " BackSpace"
    '\n' -> " Linefeed"
    '\r' -> " Return"
    '\t' -> " Tab"
    y    -> maybe [' ', y] (' ':) (Data.Map.lookup y m)

execStr :: String -> Bool -> IO ()
execStr xs isTesting = do
  let p = if isTesting then (SP.proc "cat" []) else (SP.proc "xdotool" ["-"])
  (Just hin, _ , _, procHandle) <- SP.createProcess p {SP.std_in = SP.CreatePipe}
  hPutStr hin xs
  hPutStr hin "\n"
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
    Left err  -> putStrLn $ "ERROR: " ++ show err
    Right m   -> do
      if (testing clops) then putStrLn (show m) else return ()
      execStr ("key --delay 100" ++ xdtArg s m) (testing clops)

