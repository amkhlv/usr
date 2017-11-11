module Main where

import qualified Options.Applicative as O
import           Data.Semigroup ((<>))
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML
import           Text.Pandoc.Options
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           Data.Set
import           System.Directory

data CLOps = CLOps { markdownFile :: String }

prsr :: O.Parser CLOps
prsr = CLOps <$> O.strArgument ( O.metavar "PATH_TO_MARKDOWN" <> O.help "path to .md" )

procInline :: Inline -> Inline
procInline (Link attr alts (url, ttl)) = case parse linkTransformer "" url of
  Left err -> error "=== ERROR parsing link ==="
  Right x  -> Link attr alts (x, ttl)
procInline x = x
  
startsWithProtocol :: Parser String
startsWithProtocol = mconcat <$> sequence [many1 letter, string "://", many anyChar]

innerLink :: Parser String
innerLink = mconcat <$> sequence [anyChar `manyTill` try (string ".md"), return ".html", many anyChar]

linkTransformer :: Parser String
linkTransformer = try startsWithProtocol <|> try innerLink <|> many anyChar

fltr :: Pandoc -> Pandoc
fltr pan = walk procInline pan

rdrOpts :: ReaderOptions
rdrOpts = def { readerExtensions = githubMarkdownExtensions }

wtrOpts :: String -> WriterOptions
wtrOpts template = def {
  writerTemplate = Just template,
  writerTableOfContents = True,
  writerTOCDepth = 3
  }

main :: IO ()
main = do
  CLOps mdFile <- O.execParser $
    O.info
    (prsr O.<**> O.helper)
    (O.fullDesc <> O.progDesc "a converter from Markdown to HTML")
  mdTxt    <- readFile mdFile
  homedir  <- getHomeDirectory
  template <- readFile $ homedir ++ "/.config/amkhlv/pandoc/template.html"
  case readMarkdown rdrOpts mdTxt of
    Left _    -> putStrLn "=== ERROR parsing Markdown ==="
    Right pan -> putStrLn $ writeHtmlString (wtrOpts template) (fltr pan)
