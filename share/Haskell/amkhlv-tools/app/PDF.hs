{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import qualified Control.Monad.Combinators as CMC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Options.Applicative
import PyF
import Shh
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MPB
import qualified Text.Megaparsec.Byte.Lexer as MPBL

data Options = Options
  { pdfFile :: BS.ByteString,
    page :: Int,
    dpi :: Int,
    extractPage :: Bool
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
    <*> option
      auto
      ( long "dpi"
          <> short 'd'
          <> metavar "DPI"
          <> help "dpi"
          <> showDefault
          <> value 75
      )
    <*> switch
      ( long "one"
          <> short '1'
          <> help "extract page"
      )

npg :: MP.Parsec String BSL.ByteString Int
npg = do
  CMC.skipManyTill MP.anySingle $ MP.try (MPB.string "NumberOfPages: " >> MPBL.decimal)

getNumberOfPages :: BSL.ByteString -> Either (MP.ParseErrorBundle BSL.ByteString String) Int
getNumberOfPages = MP.parse npg ""

load SearchPath ["pdftk", "pdf2svg", "cairosvg", "inkscape", "rm", "mv", "echo", "grep", "mkdir", "ls", "find"]

range0 :: Options -> BS.ByteString
range0 opts =
  C8.pack $ "A1-" ++ show (page opts - 1)

range1 :: Options -> BS.ByteString
range1 opts =
  C8.pack ("A" ++ show (page opts + 1) ++ "-end")

extract :: Options -> IO ()
extract opts = do
  let withoutPDF = BS.stripSuffix ".pdf" (pdfFile opts)
  let tmpName = fromMaybe (pdfFile opts) withoutPDF <> C8.pack "_p" <> C8.pack (show $ page opts)
  let tmpPagePDF = tmpName <> C8.pack ".pdf"
  pdftk (pdfFile opts) "cat" (show $ page opts) "output" tmpPagePDF

editPage :: Options -> IO ()
editPage opts = do
  let withoutPDF = BS.stripSuffix ".pdf" (pdfFile opts)
  let tmpName = fromMaybe (pdfFile opts) withoutPDF <> C8.pack "_p" <> C8.pack (show $ page opts)
  let dir = fromMaybe (pdfFile opts) withoutPDF <> C8.pack ".d"
  let tmpPagePDF = tmpName <> C8.pack ".pdf"
  let tmpPageSVG = dir <> C8.pack "/p" <> C8.pack (show $ page opts) <> C8.pack ".svg"
  let tmpMainPDF = fromMaybe (pdfFile opts) withoutPDF <> C8.pack "-tmp.pdf"
  nPagesReported <- pdftk (pdfFile opts) "dump_data" |> capture
  nPages <- case getNumberOfPages nPagesReported of
    Left err -> do
      putStrLn "⚠ pdftk ERROR:"
      print err
      exitWith $ ExitFailure 1
    Right num -> do
      putStrLn $ "📄 " ++ show num ++ " pages"
      return num
  dirExists <- doesDirectoryExist $ C8.unpack dir
  unless dirExists $ mkdir dir
  pdftk (pdfFile opts) "cat" (show $ page opts) "output" tmpPagePDF
  svgExists <- doesFileExist $ C8.unpack tmpPageSVG
  when svgExists $ putStrLn "😎 reusing previously traced page"
  unless svgExists (pdf2svg tmpPagePDF tmpPageSVG)
  inkStatus <- tryFailure $ inkscape tmpPageSVG
  case inkStatus of
    Right _ -> return ()
    Left err -> do
      putStrLn "⚠ inkscape ERROR:"
      print err
      rm tmpPageSVG
      exitWith $ ExitFailure 1
  putStrLn ""
  putStrLn "😅-- save (y/n) ? ......................."
  confirm <- getLine
  case confirm of
    "y" -> do
      cairosvg "--dpi" (show $ dpi opts) "-f" "pdf" "-o" tmpPagePDF tmpPageSVG
      mv (pdfFile opts) tmpMainPDF
      status <-
        tryFailure $
          if nPages == 1
            then mv tmpPagePDF (pdfFile opts)
            else
              if (page opts) == 1
                then pdftk (C8.pack "A=" <> tmpMainPDF) (C8.pack "B=" <> tmpPagePDF) "cat" "B1" (range1 opts) "output" (pdfFile opts)
                else
                  if (page opts) == nPages
                    then pdftk (C8.pack "A=" <> tmpMainPDF) (C8.pack "B=" <> tmpPagePDF) "cat" (range0 opts) "B1" "output" (pdfFile opts)
                    else pdftk (C8.pack "A=" <> tmpMainPDF) (C8.pack "B=" <> tmpPagePDF) "cat" (range0 opts) "B1" (range1 opts) "output" (pdfFile opts)

      case status of
        Right _ -> do
          rm tmpMainPDF
          rm tmpPagePDF
        Left err -> do
          putStrLn "⚠ pdftk ERROR:"
          print err
    _ -> do
      putStrLn "🐔 chicken out"
      rm tmpPagePDF

main :: IO ()
main = do
  opts <- execParser $ info (clops <**> helper) (fullDesc <> progDesc "Edit PDF" <> header "edit-pdf")
  if extractPage opts
    then extract opts
    else editPage opts
