module Main where
import Options.Applicative
import Data.Text (Text, pack, unpack, split)
import Data.List
import Data.Array
import Data.Map
import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Monad
import Control.Monad.Loops
import Control.Exception
import System.Console.ANSI
import System.IO
import System.IO.Error
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as C

data Clops = Clops
             { lineInterval :: Maybe Int
             , regexen :: [ String ]
             }

cloparser :: Parser Clops
cloparser = Clops
  <$> optional ( option auto ( short 'n' <> metavar "LINES" <> help "Maximal number of lines between occurence of patterns"))
  <*> (many $ argument str (metavar "REGEX"))

getMatchesInLine :: Clops -> String -> [(String, [(MatchOffset, MatchLength)])]
getMatchesInLine clps x = [(r, [m Data.Array.! 0  |  m <- (matchAll (prepRegex r) x)])
                          | r <- regexen clps]

cycleInput :: Clops -> Map String Int -> LimitedList String -> IO ()
cycleInput clps rgmap prevLines = do
  line <- try (hGetLine stdin)
  case line of
    Left e -> 
      if isEOFError e
      then return ()
      else ioError e
    Right ln -> do
      let matches = getMatchesInLine clps ln
      let newrgs = [(fst rm) | rm <- matches, length (snd rm) > 0]
      let newrgmap = mapWithKey (\r -> if (elem r newrgs) then (\n -> 0) else (\n -> n + 1)) rgmap
      newPrevLines <- case (lineInterval clps) of
                        Nothing -> -- like "grep regex" (just print lines matching all)
                          if getAll $ mconcat [All $ (elem x newrgs) | x <- (regexen clps)]
                          then printLine matches ln >> return zeroLL
                          else return zeroLL
                        Just n ->
                          if getAll $ mconcat [All $ (fromJust $ Data.Map.lookup x newrgmap) <= n  | x <- (regexen clps)]
                          then dumpLL prevLines >> putStr "•" >> printLine matches ln >> return (emptyLL n)
                          else return (appendLL ln prevLines)
      cycleInput clps newrgmap newPrevLines

printWithMarks :: [Int] -> [Int] -> Int -> String -> String -> IO ()
printWithMarks begs ends ind ""   acc =  putStrLn acc >> setSGR [Reset]
printWithMarks begs ends ind rest acc =
  let begsOnLeft = length [ b | b <- begs , b < ind ]
      endsOnLeft = length [ e | e <- ends , e < ind ]
  in
    if (elem ind ends)
    then
      if (begsOnLeft == endsOnLeft + 1)
      then
        putStr acc  >>
        if (elem ind begs) then return () else setSGR [Reset] >>
        putStr [head rest] >>
        printWithMarks begs ends (ind + 1) (tail rest) ""
      else
        printWithMarks begs ends (ind + 1) (tail rest) (acc ++ [head rest])
    else
      if (elem ind begs)
      then
        if (begsOnLeft == endsOnLeft)
        then
          putStr acc >>
          setSGR [SetColor Foreground Vivid Green] >>
          putStr [head rest] >>
          printWithMarks begs ends (ind + 1) (tail rest) ""
        else
          printWithMarks begs ends (ind + 1) (tail rest) (acc ++ [head rest])
      else
        printWithMarks begs ends (ind + 1) (tail rest) (acc ++ [head rest])

prepRegex :: String -> Regex
prepRegex x = makeRegex (C.pack x)

printLine :: [(String, [(MatchOffset, MatchLength)])] -> String -> IO ()
printLine mas x =
  let mStarts = join [ [ (fst m)
                       | m <- (snd ms) ]
                     | ms <- mas ]
      -- this is actually the next position after last in match :
      mEnds   = join [ [ (fst m) + (snd m) 
                       | m <- (snd ms) ]
                     | ms <- mas ]
  in
    printWithMarks mStarts mEnds 0 x "" 

data LimitedList a = LimitedList { limit :: Int , contents :: [a] }
appendLL :: a -> LimitedList a -> LimitedList a
appendLL x (LimitedList 0 xs) = LimitedList 0 []
appendLL x (LimitedList lim xs) =
  if ((length xs) < lim)
  then LimitedList lim $ xs ++ [x]
  else LimitedList lim $ (tail xs) ++ [x]
dumpLL :: LimitedList String -> IO ()
dumpLL xs = sequence_ [ putStrLn x | x <- contents xs ]
emptyLL :: Int -> LimitedList a
emptyLL n = LimitedList n []
zeroLL = LimitedList 0 []

main :: IO ()
main = execParser opts >>=
  (\clps ->
     cycleInput
     clps
     (Data.Map.fromList [(r , 2 + (fromMaybe 0 (lineInterval clps)))
                        | r <- regexen clps])
     (emptyLL $ fromMaybe 0 (lineInterval clps)))
  where
    opts = info (helper <*> cloparser)
           ( fullDesc
             <> progDesc "regex finder"
             <> header "find regexes close together" )


