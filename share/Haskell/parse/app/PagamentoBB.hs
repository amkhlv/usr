module PagamentoBB where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           Text.XML
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M


data RecLine = Date String
             | Value String
             | BarCode [String]
             deriving Show

data Prev = Neutral | ReadBarCode

type ReciboParser = CharParser (Prev, [RecLine])

barcodeParser :: [String] -> ReciboParser [String]
barcodeParser acc =
      try (many (char ' ') >> sepEndBy1 (many1 digit) (many1 (oneOf " -")) >>= (\x -> newline >> barcodeParser (acc ++ x)))
  <|> return acc

dateParser :: ReciboParser String
dateParser =
        do
          d1 <- digit
          d2 <- digit
          char '/'
          m1 <- digit
          m2 <- digit
          char '/'
          y1 <- digit
          y2 <- digit
          y3 <- digit
          y4 <- digit
          return [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2]

valueParser :: ReciboParser String
valueParser = do
  a <- many1 (skipMany (char '.') >> digit)
  char ','
  b <- many1 digit
  return $ a ++ "." ++ b

lineParser :: ReciboParser (Either Prev RecLine)
lineParser =
      try (
        do
          try (string "Data") <|> string "DATA"
          many1 space
          try (do
                date <- dateParser
                many1 space
                return (Right $ Date date)
              ) <|> do
                      description <- manyTill anyChar newline
                      date <- dateParser
                      manyTill anyChar newline
                      return (Right $ Date (description ++ ": " ++ date))
           )
  <|> try (
        do
          try (string "Valor") <|> string "VALOR"
          many1 space
          try (do
                value <- valueParser
                many1 space
                return (Right $ Value value)
               ) <|> do
                      description <- manyTill anyChar newline
                      value <- valueParser
                      manyTill anyChar newline
                      return (Right $ Value (description ++ ": " ++ value))
              )
  <|> try (string "Codigo" >> manyTill anyChar newline >> return (Left ReadBarCode))
  <|> try (string "=======" >> manyTill (char '=') newline >> return (Left ReadBarCode))
  <|> try (string "-------" >> manyTill (char '-') newline >> return (Left ReadBarCode))
  <|> try (manyTill anyChar newline >> return (Left Neutral))

reciboParser :: ReciboParser [RecLine]
reciboParser =
      try (
      do
        x <- lineParser
        case x of
          Left Neutral -> do
            updateState (\(p, acc) -> (Neutral,  acc))
            reciboParser
          Left ReadBarCode -> do
            bc <- barcodeParser []
            updateState (\(p, acc) -> if null bc then (Neutral, acc) else (Neutral, BarCode bc : acc))
            reciboParser
          Right x -> do
            updateState (\(p, acc) -> (Neutral, x:acc))
            reciboParser
           )
  <|> reverse . snd <$> getState

mkel :: RecLine -> Element
mkel (Date x) = Element (Name (T.pack "date") Nothing Nothing ) M.empty [NodeContent $ T.pack x]
mkel (Value x) = Element (Name (T.pack "value") Nothing Nothing ) M.empty [NodeContent $ T.pack x]
mkel (BarCode xs) = Element (Name (T.pack "barcode") Nothing Nothing ) M.empty [NodeContent $ T.pack x | x <- xs]

mkdoc :: [RecLine] -> Document
mkdoc ls = Document {
    documentPrologue = Prologue [] Nothing []
  , documentRoot = Element {
      elementName = Name (T.pack "pagamento") Nothing Nothing
    , elementAttributes = M.empty
    , elementNodes = [NodeElement (mkel l) | l <- ls]
  }
  , documentEpilogue = []
}
main :: IO ()
main = do
  txt <- getContents
  case runParser reciboParser (Neutral, []) "" txt of
    Left _  -> putStrLn "error"
    Right x -> putStrLn . T.unpack . LT.toStrict $  renderText def (mkdoc x)

