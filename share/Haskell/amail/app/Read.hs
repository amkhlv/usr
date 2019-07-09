{-# LANGUAGE OverloadedStrings #-}

module Read where

import Lib
import qualified Data.RFC5322 as RFC
import Data.MIME
import qualified Data.ByteString as B
import Data.MIME.Error
import qualified Data.Text as T
import qualified Control.Lens.Fold as CLF
import qualified Control.Lens.Getter as CLG
import Control.Monad ((>=>))
import System.Environment

main :: IO ()
main = do
  path <- getLine
  bs <- B.readFile path
  let esm = RFC.parse (message mime) bs
  case esm of
    Left x -> putStrLn x
    Right m ->
      case getTextPlain m of
        Just went ->
          case wire2txt went of
            Left (TransferEncodingError e) -> error "== Encoding Error =="
            Left (CharsetError e) -> error "== Charset Error =="
            Right (Message (Headers hs) x) -> putStrLn $ T.unpack x
        Nothing -> putStrLn "== No plain text detected =="






