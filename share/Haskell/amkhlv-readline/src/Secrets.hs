{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Secrets (
loadConfig,
loadPasswords,
savePasswords,
amkbd,
searchSites,
nicks,
logins,
arm,
search,
defaultAccount,
editAccount,
insertAccount,
newSite,
validateSites,
cleanupSite,
edit,
signalError,
Nick,
URL,
Account(..),
Site(..)
) where

import Prelude hiding (concat)
import qualified Dhall as DH
import Dhall.Marshal.Encode (embed, inject)
import Dhall.Pretty
import Data.Void()
import Data.Aeson
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.Functor ((<&>))
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as DBLC
import Data.Text (Text,pack,unpack,append,strip,isInfixOf,concat)
import qualified Data.Set as DS
import qualified Data.Text.IO as TIO
import System.Process
import GHC.IO.Handle
import GHC.IO.Exception
import qualified Prettyprinter.Render.Text as PrettyText
import System.IO (withFile, hPutStrLn, IOMode(WriteMode))
import System.Directory (getHomeDirectory)
import Control.Concurrent
import Control.Monad (join)

data Config = Config { 
  qmlDir :: Text
  , passwordsFile :: Text
  , keyboardPipe :: Text 
  , defaultLogin :: Text
  }
    deriving (DH.Generic, Show)

instance DH.FromDhall Config

type Nick = Text
type URL = Text
data Account = Account
  { login :: Text
  , password :: Text
  , changedOn :: Maybe Text
  , expiringOn :: Maybe Text
  , description :: Maybe Text
  , notes :: Maybe Text
  , loginChallenge :: Maybe Text
  , forgotPasswordChallenge :: Maybe Text
  , secretNotes :: Maybe Text
  , tags :: [Text]
  } deriving (DH.Generic, Show)
instance DH.ToDhall Account
instance DH.FromDhall Account
instance FromJSON Account
instance ToJSON Account
data Site = Site { nick :: Nick, url :: URL, accounts :: [Account] } deriving (DH.Generic, Show)
instance DH.ToDhall Site
instance DH.FromDhall Site
instance FromJSON Site
instance ToJSON Site

defaultAccount :: Maybe Text -> Text -> Account 
defaultAccount ml l = Account { 
     login = fromMaybe l ml,
     password = "", 
     changedOn = Nothing, 
     expiringOn = Nothing, 
     description = Nothing, 
     notes = Nothing, 
     loginChallenge = Nothing, 
     forgotPasswordChallenge = Nothing,
     secretNotes = Nothing,
     tags = []
    }

validateAccounts :: [Account] -> Bool
validateAccounts accs = let set = DS.fromList (login <$> accs) in length set == length accs

validateSites :: [Site] -> Bool
validateSites ss = let set = DS.fromList (nick <$> ss) in length set == length ss

loadConfig :: IO Config 
loadConfig = do
  home <- pack <$> getHomeDirectory 
  DH.input DH.auto (append home "/.config/amkhlv/readline.dhall")

loadPasswords :: IO [Site]
loadPasswords = do
  conf <- loadConfig
  DH.input DH.auto (passwordsFile conf)

savePasswords :: [Site] -> IO ()
savePasswords sites = do 
  conf <- loadConfig
  withFile (unpack $ passwordsFile conf) WriteMode (`PrettyText.hPutDoc` Dhall.Pretty.prettyCharacterSet Unicode (embed inject sites))

waitOnPipe :: String -> (String -> IO ExitCode) -> IO ExitCode
waitOnPipe x f = do
  (Nothing, Just stdout, Nothing, h) <- createProcess
    (proc "cat" [x])
    { std_in  = Inherit
    , std_out = CreatePipe
    , std_err = Inherit
    }
  cmd <- hGetContents stdout
  _ <- waitForProcess h
  f cmd

signalError :: String -> IO ()
signalError x = do
    putStrLn ("**" ++ ('*' <$ x) ++ "**")
    putStrLn ("* " ++ x ++ " *")
    putStrLn ("**" ++ ('*' <$ x) ++ "**")

strstrip :: String -> String
strstrip = unpack . strip . pack

-- |Search sites by substring in nick
searchSites :: [Site] -> Text -> [Site] 
searchSites ss subnick = filter (Data.Text.isInfixOf subnick . nick) ss
-- |Exact search; returns list of one element
-- searchSitesX :: [Site] -> Text -> [Site] 
-- searchSitesX ss subnick = filter (\s -> nick s == subnick) ss

showAccount :: Account -> IO ()
showAccount s = do
  putStrLn . unpack $ append (pack "  login: ") (login s)
  traverse_ (putStrLn . unpack . append (pack "    description: ")) (description s)
  traverse_ (putStrLn . unpack . append (pack "    notes:       ")) (notes s)

search :: [Site] -> Text -> IO ()
search ss nk = sequence_ [ 
  do
    putStrLn (unpack (nick s)) >> putStrLn (unpack (url s)) >> sequence [ showAccount a | a <- accounts s ]
    | s <- searchSites ss nk
    ]
 
newtype OptionsJSON = OptionsJSON { options :: [Text] } deriving (Generic,Show)
instance ToJSON OptionsJSON


chooser :: [Text] -> (Int -> IO ExitCode) -> IO ExitCode 
chooser xs cb = do 
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/chooser.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode (OptionsJSON { options = xs }))
  hFlush stdin
  hClose stdin
  forkIO $ do
    n <- read <$> hGetContents stdout 
    print n
    hClose stdout
    cb n
    return ()
  waitForProcess h

hidePassword :: Account -> Account
hidePassword a = Account {
  login = login a,
  password = pack (map (const '*') (unpack $ password a)),
  changedOn = Nothing, 
  expiringOn = Nothing, 
  description = Nothing, 
  notes = Nothing, 
  loginChallenge = Nothing, 
  forgotPasswordChallenge = Nothing,
  secretNotes = Nothing,
  tags = []
  }
  
editAccount :: Account -> IO (Maybe Account)
editAccount acc  = do
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/editor.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode acc)
  hFlush stdin
  hClose stdin
  j <- hGetContents stdout 
  let newacc = decode $ DBLC.pack j 
  maybe (putStrLn "-- edit cancelled") (print . hidePassword) newacc
  hClose stdout
  waitForProcess h >>= print
  return newacc

deleteAccount :: Account -> IO (Maybe Account)
deleteAccount acc = do
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/confirmation.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode acc)
  hFlush stdin
  hClose stdin
  yn <- hGetContents stdout 
  putStrLn yn
  hClose stdout
  waitForProcess h >>= print
  if strstrip yn == "yes" 
  then putStrLn ("deleting " ++ unpack (login acc)) >> return Nothing 
  else putStrLn ("keeping " ++ unpack (login acc)) >> return (Just acc)
cleanupAccounts :: Site -> IO (Maybe Site)
cleanupAccounts site = do
  lmacc <- sequence [deleteAccount a | a <- accounts site]
  let newlacc = join [maybeToList macc | macc <- lmacc]
  if null newlacc 
  then putStrLn "removing site with no accounts" >> return Nothing 
  else return (Just $ site { accounts = newlacc })
cleanupSite :: Nick -> [Site] -> IO [Site]
cleanupSite nk  = 
  foldr 
  (\s accum -> 
    if nk == nick s 
    then do 
      ms <- cleanupAccounts s
      ac <- accum
      return $ maybe ac (:ac) ms   
    else do
      ac <- accum  
      return (s:ac) 
   )
  (return [])

newAccount :: Maybe Text -> IO (Maybe Account)
newAccount ml = do
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/editor.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode $ defaultAccount ml (defaultLogin conf))
  hFlush stdin
  hClose stdin
  j <- hGetContents stdout 
  let newacc = decode $ DBLC.pack j 
  maybe (putStrLn "-- cancelled") (print . hidePassword) newacc
  hClose stdout
  waitForProcess h >>= print
  return newacc

insertAccount :: [Site] -> Nick -> Maybe Text -> IO [Site]
insertAccount ss nck ml = 
  sequence 
  [ if nck == nick s 
    then do  
      nacc <- newAccount ml
      if 
      (login <$> nacc) `elem` (Just . login <$> accounts s)
      then signalError "ERROR: duplicate login" >> return s
      else return Site {
                        nick = nick s, 
                        url = url s, 
                        accounts = case nacc of 
                          Just a -> a : accounts s 
                          Nothing -> accounts s
                        } 
    else return s
    | s <- ss]
newSite :: Nick -> URL -> Maybe Text -> IO (Maybe Site)
newSite s u ml = do 
  nacc <- newAccount ml
  return ((\x -> Site { nick = s, url = u, accounts = [x] }) <$> nacc)
  
editSite :: Site -> Text -> IO Site
editSite s lgn = 
  let naccs = [ if login acc == lgn then fromMaybe acc <$> editAccount acc   else return acc | acc <- accounts s ]
  in
  do 
    aa <- sequence naccs 
    return Site { nick = nick s, url = url s, accounts = aa }

edit :: [Site] -> Nick -> Text -> IO [Site]
edit ss nck lgn = sequence [ if nck == nick s then editSite s lgn else return s | s <- ss ]

arm :: [Site] -> IO ExitCode
arm ss = 
  chooser 
  (nick <$> ss) 
  (\n ->  
    chooser 
    (login <$> accounts (ss !! n))  
    (\m -> do 
      amkbd $ login (accounts (ss !! n) !! m)
      amkbd $ password (accounts (ss !! n) !! m)
      )
    )

nicks :: [Site] -> IO ()
nicks ss = 
  let ssn = zip [0..] ss in
  sequence_ [ TIO.putStrLn $ concat [pack $ show (n::Int), pack ": ", nick s ] | (n,s) <- ssn ]
          
logins :: [Account] -> IO ()
logins aa = 
  let aan = zip [0..] aa in 
  sequence_ [ TIO.putStrLn $ concat [pack $ show  (n::Int), pack ": ", login a ] | (n,a) <- aan]


amkbd :: Text -> IO ExitCode
amkbd s = do
  conf <- loadConfig
  waitOnPipe 
    (unpack $ keyboardPipe conf)
    (\cmd -> 
      case strstrip cmd of 
        "go" -> do
          (Just stdin, Nothing, Nothing, h) <- createProcess 
                                               (proc "amkbd" []) 
                                               { std_in = CreatePipe, std_out = Inherit, std_err = Inherit }
          TIO.hPutStr stdin s
          hFlush stdin 
          hClose stdin
          waitForProcess h
        "skip" -> do
          putStrLn "-- skipping"
          return ExitSuccess
        x -> do 
          putStrLn $ "-- unknown command on keyboardpipe: " ++ x
          return (ExitFailure 1)
        )
