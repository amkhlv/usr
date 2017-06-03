module NaiveSExp(
  SExp(Id, Sym, Keyword, Value, Comment, SExp),
  sExpParser,
  sExpParserTight,
  atExpParser,
  atExpParserTight
  ) where

{-
  This is *not* a Racket parser !
-}

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           Data.List
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe

data SExp = Id String
          | Sym String
          | Keyword String
          | Value String
          | Comment String
          | SExp [SExp]

instance Show SExp where
  show (Id x) = x
  show (Keyword x) = "#:" ++ x
  show (Value x) = "“" ++ x ++ "”"
  show (Comment x) = "«" ++ x ++ "»"
  show (Sym x) = "'" ++ x
  show (SExp x) = "(" ++ (intercalate " " $ map show x) ++ ")"

valueParser :: String -> Parser SExp
valueParser xs = try (Value <$> (char '"' >> return (reverse xs)))
  <|> try (char '\\' >> anyChar >>= \c -> valueParser ('\\':(c:xs)))
  <|> (anyChar >>= \c -> valueParser (c:xs))

commentParser :: String -> Parser SExp
commentParser cs =
  try (Comment <$> (newline >> return (reverse cs)))
  <|> (anyChar >>= \c -> commentParser (c:cs))

symbolParser :: Parser String
symbolParser = many1 $ noneOf "@()[]{} \n\",'`;|\\"

leafParser :: Parser SExp
leafParser = try (char '"' >> valueParser "")
  <|> try (char ';' >> commentParser "")
  <|> try (Keyword <$> ((string "#:") >> symbolParser))
  <|> fmap Id symbolParser

removeSpacesAround :: Parser a -> Parser a
removeSpacesAround p = skipMany space >> p >>= (skipMany space >>) . return

listOfSExpParser :: Parser [SExp]
listOfSExpParser = try (between (char '(') (char ')') (many (try atExpParser <|> sExpParser)))
  <|> (between (char '[') (char ']') (many (try atExpParser <|> sExpParser)))

sExpParserTight :: Parser SExp
sExpParserTight =
  try (string "'" >>
       (try (SExp . ((Id "quote"):) <$> listOfSExpParser) <|> Sym <$> symbolParser ))
  <|> try leafParser
  <|> SExp <$> listOfSExpParser

sExpParser :: Parser SExp
sExpParser =  removeSpacesAround sExpParserTight

atExpParserTight :: Parser SExp
atExpParserTight = 
  char '@' >> sExpParserTight >>= cmdParser >>= interpolatedTextParser >>= literalTextParser

atExpParser :: Parser SExp
atExpParser = removeSpacesAround atExpParserTight

cmdParser :: SExp -> Parser SExp
cmdParser (Id x) =
  try (SExp . (Id x:) <$> between (char '[') (char ']') (many sExpParser))
  <|> return (Id x)
cmdParser x = return x

interpolator :: [SExp] -> String -> Parser [SExp]
interpolator xs acc = 
  try (atExpParserTight >>=
       (\x -> interpolator (if acc == "" then (x:xs) else (x:(Value $ reverse acc):xs)) ""))
  <|> try (between (char '{') (char '}') (interpolator (Value (reverse $ '{':acc):xs) "") >>=
           flip interpolator "}")
  <|> try (noneOf "{}" >>= (\c -> interpolator xs (c:acc)))
  <|> return (if acc == "" then xs else (Value $ reverse acc):xs)

revmrg :: [SExp] -> [SExp]
revmrg xs = revmrg' [] xs where
  revmrg' acc [] = acc
  revmrg' acc (Value a : Value b : rst) = revmrg' acc (Value  (b ++ a) : rst)
  revmrg' acc (y:ys) = revmrg' (y:acc) ys
 
interpolatedTextParser :: SExp -> Parser SExp
interpolatedTextParser (Comment x) = return (Comment x)
interpolatedTextParser (SExp sexps) =
  try (SExp . (sexps ++) . revmrg <$> between (char '{') (char '}') (interpolator [] ""))
  <|> return (SExp sexps)
interpolatedTextParser x =
  try (SExp . revmrg <$> between (char '{') (char '}') (interpolator [x] "")) <|> return x


mirror :: Char -> Maybe Char
mirror c =
  let punctA = "->+="
      punctB = "-<+="
  in
    case findIndex (c ==) punctA of
      Just n -> Just (punctB !! n)
      Nothing -> case findIndex (c ==) punctB of
        Just n -> Just (punctA !! n)
        Nothing -> Nothing

getStop :: String -> Parser String
getStop acc = try (char '{' >> return ( "}" ++ acc ++ "|"))
  <|> (do
          c <- anyChar
          case (mirror c) of
            Just x -> getStop (x:acc)
            Nothing -> pzero)

literator :: String -> Parser SExp
literator acc = do
  dataHereDelim <- getStop ""
  Value <$> (manyTill anyChar (string dataHereDelim))

literalTextParser :: SExp -> Parser SExp
literalTextParser (Comment x) = return (Comment x)
literalTextParser (SExp sexps) =
  try ((\v -> SExp (sexps ++ [v])) <$> (char '|' >> literator "")) <|> return (SExp sexps)
literalTextParser x = try ((\v -> SExp [x,v]) <$> (char '|' >> literator "")) <|> return x
