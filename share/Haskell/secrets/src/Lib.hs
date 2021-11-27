module Lib (
    TypingMode (..),
    typingRobot,
    notification
    ) where

import qualified Data.Text as T
import qualified System.Directory as SysDir
import System.Process
import GHC.IO.Handle
import Data.Maybe
import AmkhlvSecrets

data TypingMode = LoginThenPassword | PasswordThenLogin | JustPassword deriving Eq

typeText :: T.Text -> IO ()
typeText t = do
  (mstdin1, _, _, hndl1) <- createProcess
    (proc "/usr/local/lib/amkhlv/xvkbd-helper.sh" []){std_in = CreatePipe, std_err = CreatePipe}
  let stdin = fromJust mstdin1 in hPutStr stdin (T.unpack t) >> hFlush stdin
  waitForProcess hndl1
  return ()

notification :: String -> Maybe String -> IO ()
notification x mx = do
  dir <- SysDir.getHomeDirectory
  let icn = case mx of
        Just i -> ["-i", dir ++ "/.config/amkhlv/secrets/" ++ i]
        Nothing -> []
  _ <- createProcess (proc "notify-send" $ icn ++ ["-t", "1500", x])
  return ()

typingRobot :: TypingMode -> Account -> IO ()
typingRobot LoginThenPassword acct = do
  notification "ready to type login" (Just "login.svg")
  typeText $ login acct
  notification "typing login" (Just "login.svg")
  typeText $ password acct
  notification "typing password" (Just "password.svg")
typingRobot PasswordThenLogin acct = do
  notification "ready to type password" (Just "password.svg")
  typeText $ password acct
  notification "typing password" (Just "password.svg")
  typeText $ login acct
  notification "typing login" (Just "login.svg")
typingRobot JustPassword acct = do
  notification "ready to type password" (Just "password.svg")
  typeText $ password acct
  notification "typing password" (Just "password.svg")
