{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getTextPlain
    , wire2txt
    ) where

import qualified Data.RFC5322 as RFC
import Data.MIME
import qualified Data.ByteString as B
import Data.MIME.Error
import qualified Data.Text as T
import qualified Control.Lens.Fold as CLF
import qualified Control.Lens.Getter as CLG
import Control.Monad ((>=>))
import System.Environment

getTextPlain :: MIMEMessage -> Maybe WireEntity
getTextPlain = CLF.firstOf (entities . CLF.filtered f)
  where
  f = matchContentType "text" (Just "plain") . CLG.view (headers . contentType)

wire2txt :: WireEntity -> Either EncodingError TextEntity
wire2txt = CLG.view transferDecoded >=> CLG.view charsetDecoded
