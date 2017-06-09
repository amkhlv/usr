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
import Data.Text.ICU.Regex
import Data.Text.Foreign
import qualified Data.ByteString.Char8 as C

data Clops = Clops
             { lineInterval :: Maybe Int
             , regexen :: [ String ]
             }

cloparser :: Parser Clops
cloparser = Clops
  <$> optional ( option auto ( short 'n' <> metavar "LINES" <> help "Maximal number of lines between occurence of patterns"))
  <*> (many $ argument str (metavar "REGEX"))

type MatchOffset = I16
type MatchLength = I16

findAllRICU :: Regex -> [(MatchOffset, MatchLength)] -> IO [(MatchOffset, MatchLength)]
findAllRICU ricu acc = do
  b <- findNext ricu
  case b of
    True -> do
      s <- start_ ricu 0
      e <- end_   ricu 0
      findAllRICU ricu ((s, e - s) : acc)
    False -> return $ reverse acc

getMatchesInLine :: Values -> String -> IO [(String, [(MatchOffset, MatchLength)])]
getMatchesInLine vls x =
  sequence [
  do
    ricu <- fromJust $ Data.Map.lookup r (rgxs vls)
    setText ricu (pack x)
    allmatches <- findAllRICU ricu []
    return (r, allmatches)
  | r <- regexen (clps vls)]  

cycleInput :: Values -> Vars -> IO ()
cycleInput vls vrs = do
  line <- try (hGetLine stdin)
  case line of
    Left e -> 
      if isEOFError e
      then return ()
      else ioError e
    Right ln -> do
      let iomatches = getMatchesInLine vls ln
      matches <- iomatches
      let newrgs = [(fst rm) | rm <- matches, length (snd rm) > 0]
      let newrgmap = mapWithKey (\r -> if (elem r newrgs) then (\n -> 0) else (\n -> n + 1)) (rgxMap vrs)
      (newLprev, newPrevLines) <- case (lineInterval (clps vls)) of
        Nothing -> -- like "grep regex" (just print lines matching all)
          if getAll $ mconcat [All $ (elem x newrgs) | x <- (regexen (clps vls))]
          then printLine iomatches ln >> return (curLn vrs + 1, zeroLL)
          else return (prevRelLn vrs, zeroLL)
        Just n ->
          if getAll $ mconcat [All $ (fromJust $ Data.Map.lookup x newrgmap) <= n  | x <- (regexen (clps vls))]
          then do
            let prefix = if (isFull $ prevLns vrs)
                  then Just $ show (curLn vrs + 1) ++ " ━━━━━━━━━━━━━━━━ " ++ show (curLn vrs - prevRelLn vrs) ++ " ━━━━━━━━━━━━━━━━"
                  else Nothing
              in dumpLL vls prefix (prevLns vrs)
            printLine iomatches ln
            return (curLn vrs + 1, emptyLL n)
          else return (prevRelLn vrs, appendLL ln $ prevLns vrs)
      seq newPrevLines $ cycleInput vls (Vars (curLn vrs + 1) newLprev newrgmap newPrevLines)

printWithMarks :: [I16] -> [I16] -> I16 -> String -> String -> IO ()
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

printLine :: IO[(String, [(MatchOffset, MatchLength)])] -> String -> IO ()
printLine iomas x = do
  mas <- iomas
  let mStarts = join [   [ (fst m) | m <- (snd ms) ]   |   ms <- mas   ]
      -- this is actually the next position after last in match :
      mEnds   = join [   [ (fst m) + (snd m)   | m <- (snd ms) ]   |   ms <- mas   ]
  printWithMarks mStarts mEnds 0 x "" 

data LimitedList a = LimitedList { limit :: Int , contents :: ![a] }
appendLL :: a -> LimitedList a -> LimitedList a
appendLL x (LimitedList 0 xs) = LimitedList 0 []
appendLL x (LimitedList lim xs) =
  let newxs = if  (length xs < lim)  then  xs ++ [x]  else  let txs = tail xs in txs `seq` txs ++ [x]
  in LimitedList lim newxs
dumpLL :: Values -> Maybe String -> LimitedList String -> IO ()
dumpLL vls mprefix xs = do
  maybe (return ()) putStrLn mprefix
  sequence_ [ printLine (getMatchesInLine vls x) x | x <- contents xs ]
emptyLL :: Int -> LimitedList a
emptyLL n = LimitedList n []
zeroLL = LimitedList 0 []
isNotEmpty :: LimitedList a -> Bool
isNotEmpty x = not $ Data.Foldable.null (contents x)
isFull :: LimitedList a -> Bool
isFull (LimitedList lim cs) = (length cs) == lim

data Vars =
  Vars { curLn :: Int , prevRelLn :: Int , rgxMap :: Map String Int , prevLns :: LimitedList String }

data Values = Values { clps :: Clops , rgxs :: Map String (IO Regex) }

main :: IO ()
main = do
  let opts = info (helper <*> cloparser)
           ( fullDesc
             <> progDesc "regex finder"
             <> header "find regexes close together" )
  clps <- execParser opts
  let initialVars = Vars {
        curLn = 0 ,
        prevRelLn = 0 ,
        rgxMap = Data.Map.fromList [(r , 2 + (fromMaybe 0 (lineInterval clps)))
                                   | r <- regexen clps] ,
        prevLns = emptyLL $ fromMaybe 0 (lineInterval clps)
        }
  let mainValues =
        Values clps $ Data.Map.fromList [ (r, regex [CaseInsensitive] (pack r)) | r <- regexen clps]
  cycleInput mainValues initialVars
