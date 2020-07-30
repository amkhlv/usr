{-# LANGUAGE OverloadedStrings #-}
module AmkhlvSecrets
    ( getSecrets
    , getAccount
    , getSite
    , Nick 
    , URL
    , Account (..)
    , Site (..)
    ) where

import Prelude hiding (readFile)
import System.Environment
import qualified Control.Monad.IO.Class as IOC
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLZ
import qualified Data.Char as C
import qualified Text.XML  as TX
import Text.XML.Cursor
import System.Process
import qualified System.Directory as SysDir
import GHC.IO.Handle

type Nick = T.Text
type URL = T.Text
data Account = Account
  { login :: T.Text
  , password :: T.Text
  , changedOn :: [T.Text]
  , expiringOn :: [T.Text]
  , description :: [T.Text]
  , notes :: [T.Text]
  , loginChallenge :: [T.Text]
  , forgotPasswordChallenge :: [T.Text]
  , secretNotes :: [T.Text]
  , tags :: [T.Text]
  } deriving Show
data Site = Site { nick :: Nick, url :: URL, accounts :: [Account] } deriving Show

getValues :: T.Text -> Cursor -> [T.Text]
getValues x cur =
  child >=> checkName (\nm -> x == TX.nameLocalName nm) >=> child >=> content $ cur

getAccount :: Cursor -> Account
getAccount cur =
  Account
  { login =
    head
    $ laxAttribute "login" cur
  , password =
    head
    $ child >=> checkName (\nm -> "password" == TX.nameLocalName nm) >=> child >=> content
    $ cur
  , changedOn =
    laxAttribute (T.pack "expires") cur
  , expiringOn =
    laxAttribute (T.pack "changed") cur
  , description =
    getValues "description" cur
  , notes =
    getValues "notes" cur
  , loginChallenge =
    getValues "login_challenge" cur
  , forgotPasswordChallenge =
    getValues "forgot_password_challenge" cur
  , secretNotes =
    getValues "secret_notes" cur
  , tags =
    child >=> checkName (\nm -> "tags" == TX.nameLocalName nm) >=> child >=> child >=> content
    $ cur
  }

getSite :: Cursor -> Site
getSite cur =
  Site
  (head $ laxAttribute (T.pack "nick") cur)
  (head $ laxAttribute (T.pack "url") cur)
  (map getAccount $ (child >=> checkName (\nm -> "account" == TX.nameLocalName nm)) cur)

getSecrets :: String -> String -> IO [Site]
getSecrets secretFile pwd = do
  args <- getArgs
  (Just stdin, Just stdout, Just stderr, hndl) <- createProcess
    (proc "gpg" ["--batch", "--passphrase-fd", "0", "--decrypt", secretFile])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  hPutStr stdin pwd >> hFlush stdin
  contents <- hGetContents stdout
  let txt = T.pack contents
      doc = TX.parseText_ TX.def (TLZ.pack contents)
      cur = fromDocument doc
  return $ map getSite $ (child >=> anyElement) $ head $ (child >=> anyElement) cur  
