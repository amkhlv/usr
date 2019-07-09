{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Grep where


import Lib
import qualified Data.RFC5322 as RFC
import Data.MIME
import qualified Data.ByteString as B
import Data.MIME.Error
import qualified Data.Text as T
import qualified Control.Lens.Fold as CLF
import qualified Control.Lens.Getter as CLG
import Control.Monad ((>=>))
import Control.Exception (try)
import System.IO.Error
import System.Environment
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as C

readSTDIN :: IO (Either IOError String)
readSTDIN = try getLine

main :: IO ()
main = do
  args <- getArgs
  mpath <- readSTDIN
  case mpath of
    Left isEOFError -> return ()
    Right path -> do
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
                Right (Message (Headers hs) x) ->
                  let (_,a,_,_) =  (T.unpack x) =~ C.pack (head args) :: (String, String, String, [String])  in
                    if length a == 0 then return () else putStrLn path
            Nothing -> error "== No plain text detected =="
      main


