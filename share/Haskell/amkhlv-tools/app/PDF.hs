{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe
import Data.Text (Text, pack, replace, unpack)
import Options.Applicative
import PyF
import Shh
import System.Directory (doesFileExist)
import System.Environment
import System.IO

data Options = Options
  { pdfFile :: BS.ByteString,
    page :: Int
  }

clops :: Parser Options
clops =
  Options
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> help "pdf file"
      )
    <*> option
      auto
      ( long "page"
          <> short 'p'
          <> metavar "PAGE"
          <> help "page number"
      )

load SearchPath ["pdftk", "pdf2svg", "cairosvg", "inkscape", "rm", "mv", "echo", "grep", "cat", "ls", "find"]

range0 :: Options -> BS.ByteString
range0 opts = C8.pack ("A1-" ++ show (page opts - 1))

range1 :: Options -> BS.ByteString
range1 opts = C8.pack ("A" ++ show (page opts + 1) ++ "-end")

main :: IO ()
main = do
  opts <- execParser $ info (clops <**> helper) (fullDesc <> progDesc "Edit PDF" <> header "edit-pdf")
  let withoutPDF = BS.stripSuffix ".pdf" (pdfFile opts)
  let tmpFile = fromMaybe (pdfFile opts) withoutPDF <> C8.pack "_p" <> C8.pack (show $ page opts)
  let tmpFilePDF = tmpFile <> C8.pack ".pdf"
  let tmpFileSVG = tmpFile <> C8.pack ".svg"
  let tmpMainPDF = fromMaybe (pdfFile opts) withoutPDF <> C8.pack "-tmp.pdf"
  pdftk (pdfFile opts) "cat" (show $ page opts) "output" tmpFilePDF
  svgExists <- doesFileExist $ C8.unpack tmpFileSVG
  when svgExists $ putStrLn "😎 reusing previously traced page"
  unless svgExists (pdf2svg tmpFilePDF tmpFileSVG)
  inkscape tmpFileSVG
  putStrLn ""
  putStrLn "😅-- save (y/n) ? ......................."
  confirm <- getLine
  case confirm of
    "y" -> do
      cairosvg "--dpi" "75" "-f" "pdf" "-o" tmpFilePDF tmpFileSVG
      mv (pdfFile opts) tmpMainPDF
      pdftk (C8.pack "A=" <> tmpMainPDF) (C8.pack "B=" <> tmpFilePDF) "cat" (range0 opts) "B1" (range1 opts) "output" (pdfFile opts)
      rm tmpFilePDF
    _ -> do
      putStrLn "you chicken out"
      return ()
