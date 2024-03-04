{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Secrets (
loadConfig,
loadPasswords,
savePasswords,
amkbd,
searchSites,
searchSitesX,
nicks,
logins,
arm,
search,
findAndShowAccount,
withtags,
defaultAccount,
editAccount,
insertAccount,
newSite,
validateSites,
cleanupSite,
edit,
signalError,
changeNick,
changeURL,
hidePassword,
hideSecrets,
showAll,
showPassword,
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
import Data.Maybe (isJust, fromJust, fromMaybe, maybeToList)
import Data.Functor ((<&>))
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.List (find)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.ByteString as DB
import Data.Text (Text,pack,unpack,append,strip,isInfixOf,concat)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Set as DS
import qualified Data.Text.IO as TIO
import System.Process
import GHC.IO.Handle
import GHC.IO.Handle.FD(withFileBlocking)
import GHC.IO.Exception
import qualified Prettyprinter.Render.Text as PrettyText
import System.IO (withFile,   hPutStrLn, IOMode(WriteMode,ReadMode,ReadWriteMode))
import System.Directory (getHomeDirectory,removeFile,doesFileExist)
import System.Posix.Files (createNamedPipe,unionFileModes,ownerReadMode,ownerWriteMode,groupReadMode,groupWriteMode)
import Control.Concurrent
import Control.Monad (join,when,(>=>),(<=<))
import qualified System.IO.Strict as SIO

-- #110D0E,#312728,#721415,#9B141B,#EDD8B9,#110D0E,#312728,#721415

data Config = Config { 
  qmlDir :: Text
  , passwordsFile :: Text
  , keyboardPipe :: Text 
  , defaultLogin :: Text
  , bgColorChooseSiteWindow :: Text
  , bgColorChooseAccountWindow :: Text
  , bgColorSiteIndex :: Text
  , fgColorSiteIndex :: Text
  , bgColorSiteChoice :: Text
  , fgColorSiteChoice :: Text
  , bgColorAccountIndex :: Text
  , fgColorAccountIndex :: Text
  , bgColorAccountChoice :: Text
  , fgColorAccountChoice :: Text
  , bgColorEditAccountWindow :: Text
  , bgColorNewAccountWindow  :: Text
  , bgColorEditorNormalField :: Text
  , fgColorEditorNormalField :: Text
  , bgColorEditorSecretField :: Text
  , fgColorEditorSecretField :: Text
  , bgOKBtn :: Text
  , fgOKBtn :: Text
  , bgCancelBtn :: Text
  , fgCancelBtn :: Text
  , bgYesBtn :: Text
  , fgYesBtn :: Text
  , bgNoBtn :: Text
  , fgNoBtn :: Text
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

cleanPipe :: String -> IO ()
cleanPipe x = do
  (Nothing, Nothing, Nothing, hrm) <- createProcess
    (proc "rm" [x])
    { std_in  = Inherit
    , std_out = Inherit
    , std_err = Inherit
    }
  _ <- waitForProcess hrm
  (Nothing, Nothing, Nothing, hmk) <- createProcess
    (proc "mkfifo" [x])
    { std_in  = Inherit
    , std_out = Inherit
    , std_err = Inherit
    }
  _ <- waitForProcess hmk
  return ()

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
searchSitesX :: [Site] -> Text -> Maybe Site 
searchSitesX ss subnick = find (\s -> nick s == subnick) ss

findAndShowAccount :: [Site] -> Nick -> Text -> IO ()
findAndShowAccount ss nk l = 
  let ms = searchSitesX ss nk in
  maybe 
  (return ()) 
  (\s -> do
    putStrLn (unpack(nick s) ++ "  " ++ unpack (url s))
    sequence_ [ showAccount a | a <- accounts s, login a == l ]
    )
  ms
  

showAll :: Text -> Site -> IO ()
showAll l s  = let a = head [ acc | acc <- accounts s, login acc == l ] in 
  do 
    putStrLn (unpack(nick s) ++ "  " ++ unpack (url s))
    print $ hidePassword a

showAccount :: Account -> IO ()
showAccount s = do
  putStrLn ""
  putStrLn . unpack $ append (pack " -- login: ") (login s)
  traverse_ (putStrLn . unpack . append (pack "    description: ")) (description s)
  traverse_ (putStrLn . unpack . append (pack "    notes:       ")) (notes s)
  traverse_ (putStrLn . const "    *** secret notes ***") (secretNotes s)

search :: [Site] -> Text -> IO ()
search ss nk = sequence_ [ 
  do
    putStrLn "" >> putStr (unpack (nick s) ++ "  ") >> putStrLn (unpack (url s)) >> sequence [ showAccount a | a <- accounts s ]
    | s <- searchSites ss nk
    ] >> putStrLn ""

nulltags :: [Site] -> IO ()
nulltags = withtags [""] 

withtags :: [Text] -> [Site] -> IO ()
withtags [] ss = 
    let alltags = DS.fromList (ss >>= (accounts >=> tags)) in print alltags
withtags tgs ss = 
  let f = (\a -> and [tg `elem` tags a | tg <- tgs])::Account -> Bool 
  in 
  sequence_ [
    let accts1 = filter f (accounts s) 
    in
    if null accts1 then return () else do
      putStr (unpack $ nick s)
      putStrLn $ "  " ++ unpack (url s)
      sequence_ [ putStrLn $ "  " ++ unpack (login a) | a <- accts1 ]
    | s <- ss
    ]
 
data OptionJSON = OptionJSON {
  txt :: Text
  , fgIndex :: Text
  , bgIndex :: Text
  , fgChoice :: Text
  , bgChoice :: Text
} deriving (Generic,Show)
instance ToJSON OptionJSON

data OptionsJSON = OptionsJSON { 
   colorWindow :: Text,
   options :: [OptionJSON] 
  } deriving (Generic,Show)
instance ToJSON OptionsJSON


chooser :: OptionsJSON -> (Int -> IO ExitCode) -> IO ExitCode 
chooser xs cb = do 
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/chooser.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode xs)
  hFlush stdin
  hClose stdin
  _ <- forkIO $ do
    n <- SIO.run $ read <$> SIO.hGetContents stdout
    hClose stdout
    _ <- cb n
    return ()
  waitForProcess h

data PasswdJSON = PasswdJSON {
  passwd :: Text
} deriving (Generic, Show)
instance ToJSON PasswdJSON
showPassword :: [Site] -> Nick -> Text -> IO ()
showPassword ss nk l = do
  conf <- loadConfig
  let ms = searchSitesX ss nk in
    maybe 
    (return ()) 
    (\s -> 
      sequence_ 
      [ do 
          (Just stdin, Nothing, Nothing, h) <- createProcess 
            (proc "ioqml" [unpack (qmlDir conf) ++ "/show-password.qml"])
            { std_in = CreatePipe, std_out = Inherit, std_err = Inherit }
          hPutStrLn stdin $ DBLC.unpack (encode $ PasswdJSON { passwd = password a })
          hFlush stdin
          hClose stdin
          _ <- waitForProcess h
          return ()
       | a <- accounts s, login a == l ]
      )
    ms
    

hidePassword :: Account -> Account
hidePassword a = a {
  password = pack (map (const '*') (unpack $ password a))
  }
hideSecrets :: Account -> Account
hideSecrets a = a {
  password = pack (map (const '*') (unpack $ password a)),
  loginChallenge = pack . (const '*' <$>) . unpack <$> loginChallenge a, 
  forgotPasswordChallenge = pack . (const '*' <$>) . unpack <$> forgotPasswordChallenge a,
  secretNotes = pack . (const '*' <$>) . unpack <$> secretNotes a
  }
data EditorStyleJSON = EditorStyleJSON {
  bgEditorWindow :: Text
  , bgNormalField :: Text
  , fgNormalField :: Text
  , bgSecretField :: Text
  , fgSecretField :: Text
  , bgOKButton :: Text
  , fgOKButton :: Text
  , bgCancelButton :: Text
  , fgCancelButton :: Text
  , isConfirmation :: Bool
}  deriving (Generic,Show)
instance ToJSON EditorStyleJSON
editAccount :: Account -> IO (Maybe Account)
editAccount acc  = do
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/editor.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode acc)
  let style = EditorStyleJSON {
    bgEditorWindow = bgColorEditAccountWindow conf
    , bgNormalField = bgColorEditorNormalField conf
    , fgNormalField = fgColorEditorNormalField conf
    , bgSecretField = bgColorEditorSecretField conf
    , fgSecretField = fgColorEditorSecretField conf
    , bgOKButton = bgOKBtn conf
    , fgOKButton = fgOKBtn conf
    , bgCancelButton = bgCancelBtn conf
    , fgCancelButton = fgCancelBtn conf
    , isConfirmation = False 
  }
  hPutStrLn stdin $ DBLC.unpack (encode style)
  hFlush stdin
  hClose stdin
  j <- SIO.run $ SIO.hGetContents stdout 
  let newacc = decode $ DBLC.pack j 
  -- maybe (putStrLn "-- edit cancelled") (print . hideSecrets) newacc
  hClose stdout
  waitForProcess h >>= print
  return newacc

deleteAccount :: Account -> IO (Maybe Account)
deleteAccount acc = do
  conf <- loadConfig
  (Just stdin, Just stdout, Nothing, h) <- createProcess 
    (proc "ioqml" [unpack (qmlDir conf) ++ "/editor.qml"])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hPutStrLn stdin $ DBLC.unpack (encode acc)
  let style = EditorStyleJSON {
    bgEditorWindow = bgColorEditAccountWindow conf
    , bgNormalField = bgColorEditorNormalField conf
    , fgNormalField = fgColorEditorNormalField conf
    , bgSecretField = bgColorEditorSecretField conf
    , fgSecretField = fgColorEditorSecretField conf
    , bgOKButton = bgYesBtn conf
    , fgOKButton = fgYesBtn conf
    , bgCancelButton = bgNoBtn conf
    , fgCancelButton = fgNoBtn conf
    , isConfirmation = True 
  }
  hPutStrLn stdin $ DBLC.unpack (encode style)
  hFlush stdin
  hClose stdin
  yn <- SIO.run $ SIO.hGetContents stdout 
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
  let style = EditorStyleJSON {
    bgEditorWindow = bgColorNewAccountWindow conf
    , bgNormalField = bgColorEditorNormalField conf
    , fgNormalField = fgColorEditorNormalField conf
    , bgSecretField = bgColorEditorSecretField conf
    , fgSecretField = fgColorEditorSecretField conf
    , bgOKButton = bgOKBtn conf
    , fgOKButton = fgOKBtn conf
    , bgCancelButton = bgCancelBtn conf
    , fgCancelButton = fgCancelBtn conf
    , isConfirmation = False 
  }
  hPutStrLn stdin $ DBLC.unpack (encode style)
  hFlush stdin
  hClose stdin
  j <- SIO.run $ SIO.hGetContents stdout 
  let newacc = decode $ DBLC.pack j 
  -- maybe (putStrLn "-- cancelled") (print . hideSecrets) newacc
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
      else return $ s { accounts = case nacc of 
                          Just a -> a : accounts s 
                          Nothing -> accounts s
                        } 
    else return s
    | s <- ss]
newSite :: Nick -> URL -> Maybe Text -> IO (Maybe Site)
newSite s u ml = do 
  nacc <- newAccount ml
  return ((\x -> Site { nick = s, url = u, accounts = [x] }) <$> nacc)

changeURL :: Nick -> URL -> [Site] -> [Site]
changeURL nk newurl  =
  foldr
  (\s accum -> if nk == nick s then s {url = newurl}:accum else s:accum)
  []

changeNick :: Nick -> Nick -> [Site] -> [Site]
changeNick nk newnick =
  foldr
  (\s accum -> if nk == nick s then s {nick = newnick}:accum else s:accum)
  []

editSite :: Site -> Text -> IO Site
editSite s lgn = 
  let naccs = [ if login acc == lgn then fromMaybe acc <$> editAccount acc   else return acc | acc <- accounts s ]
  in
  do 
    aa <- sequence naccs 
    return s { accounts = aa }

edit :: [Site] -> Nick -> Text -> IO [Site]
edit ss nck lgn = sequence [ if nck == nick s then editSite s lgn else return s | s <- ss ]

arm :: [Site] -> IO ExitCode
arm ss = do
  conf <- loadConfig
  chooser 
    OptionsJSON { 
      colorWindow = bgColorChooseSiteWindow conf,
      options = [ 
        OptionJSON {
          txt = nick s 
          , bgIndex = bgColorSiteIndex conf
          , fgIndex = fgColorSiteIndex conf
          , bgChoice = bgColorSiteChoice conf
          , fgChoice = fgColorSiteChoice conf
        } | s <- ss ] }
    (\n ->  
      chooser 
      OptionsJSON { 
        colorWindow = bgColorChooseAccountWindow conf,
        options = [
          OptionJSON {
            txt = login a
            , bgIndex = bgColorAccountIndex conf
            , fgIndex = fgColorAccountIndex conf
            , bgChoice = bgColorAccountChoice conf
            , fgChoice = fgColorAccountChoice conf
          } | a <- accounts (ss !! n)] }
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
  cleanPipe (unpack $ keyboardPipe conf)
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
          putStrLn $ "-- unknown command on keyboardpipe: >>" ++ x ++ "<<"
          return (ExitFailure 1)
        )
