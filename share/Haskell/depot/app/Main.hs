{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Yesod
import           Yesod.Core (toWaiApp)
import           Network.Wai.Handler.Warp      (defaultSettings, setPort, setHost, runSettings)
import           Data.Text (Text, unpack, pack, concat)
import qualified Data.ByteString.Char8 as BS
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist.Sqlite
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Yaml
import           Data.Maybe (fromJust)
import           System.Environment

import Model
import Foundation
import Dispatch


data DepConf = DepConf { port :: Int , dbfile :: Text , root :: Text , dir :: Text }
                       deriving (Show)

instance FromJSON DepConf where
    parseJSON (Object v) = DepConf <$>
                           v .: "port" <*>
                           v .: "dbfile" <*>
                           v .: "root" <*>
                           v .: "dir"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse DepConf from YAML/JSON"

main :: IO ()
main = do
  args <- getArgs
  ymlData <- BS.readFile (head args)
  let conf = Data.Yaml.decode ymlData :: Maybe DepConf
  let stts = setPort (port (fromJust conf)) $ setHost "127.0.0.1" defaultSettings
  runStderrLoggingT $ withSqlitePool (dbfile (fromJust conf)) 10 $ \pool -> liftIO $ do
    waiApp <- toWaiApp $ Depot pool (root (fromJust conf)) (dir (fromJust conf))
    runSettings stts waiApp
