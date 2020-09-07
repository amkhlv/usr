{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module AYaml where

import Lib
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.YAML
import Data.Maybe
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (hPutStrLn, hPutStr)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (keys, elems)
import System.IO (stderr, hFlush, hClose)
import Control.Applicative
import System.Process
import System.Directory
import System.FilePath.Posix
import Data.Char (chr, ord)
import Data.Text.Encoding
import System.Console.ANSI


aYamlFile :: IO (FilePath)
aYamlFile = (\p -> p </> ("a.yaml" :: FilePath)) <$> getHomeDirectory 

-- runScript :: Text -> IO ()
-- runScript s = do
--   confDir <- configDir
--   _ <- createProcess $ shell $ "sh -c " ++ (confDir </> "bin" </> unpack s)
--   return ()

parseX :: FilePath -> IO (Maybe (Node Pos))
parseX ayaml = do
  raw <- BSL.readFile ayaml
  case decode1 raw of
    Left (loc,emsg) -> pure Nothing
    Right x -> pure $ Just x

printCharHint :: Int -> IO ()
printCharHint i = do
  setSGR [SetColor Foreground Vivid Green]
  putStr [(chr $ ord 'a' + i)]
  setSGR [Reset]
  putStr "│"

printKey :: Int -> Node Pos -> IO ()
printKey i (Scalar _ (SStr x)) = putStrLn (if i == 0 then "─┐" else " │") >> printCharHint i >> putStrLn (unpack x)
printKey i (Scalar _ (SInt x)) = putStrLn (if i == 0 then "─┐" else " │") >> printCharHint i >> putStrLn (show x)

dialog :: Node Pos -> IO ()
dialog (Mapping _ _ m) = do
  sequence_ [ printKey (fst k) (snd k) | k <- zip [0 ..] (keys m) ]
  putStrLn "─┘"
  hint <- getLine
  case hint of
    c:cs -> dialog ((elems m) !! (ord c - ord 'a'))
    _ -> setSGR [SetColor Foreground Vivid Green] >> putStrLn "-- Bye-Bye !" >> setSGR [Reset]

dialog (Scalar _ (SStr x)) = do
  putStrLn $ unpack x
  putStrLn ""
  setSGR [SetColor Foreground Vivid Green]
  putStr "x "
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "EXECUTE"
  setSGR [SetColor Foreground Vivid Green]
  putStr "c "
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "COPY"
  setSGR [Reset]
  putStrLn ""
  confirm <- getLine
  case confirm of
    'x':_ -> cmd x
    'c':_ -> xsel x
    _ -> setSGR [SetColor Foreground Vivid Yellow] >> putStrLn "-- Bye-Bye !" >> setSGR [Reset]

cmd :: Text ->  IO ()
cmd x = do
  (ih, _, _, _) <- createProcess (proc "bash" []) {std_in = CreatePipe}
  hPutStr (fromJust ih) (encodeUtf8 x)
  hFlush (fromJust ih)
  hClose (fromJust ih)

xsel :: Text ->  IO ()
xsel x = do
  (ih, _, _, _) <- createProcess (proc "xsel" ["-i"]) {std_in = CreatePipe}
  hPutStr (fromJust ih) (encodeUtf8 x)
  hFlush (fromJust ih)
  hClose (fromJust ih)
  
main :: IO ()
main = do
  ayaml <- aYamlFile
  n <- parseX ayaml
  case n of
    Just x -> dialog x
    Nothing -> putStrLn "ERROR"

  
