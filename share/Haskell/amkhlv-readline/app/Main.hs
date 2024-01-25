{-# LANGUAGE OverloadedStrings #-}

import System.Console.Haskeline
import Secrets
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Functor ((<&>))


help :: String
help = unlines [
    "arm <subnick>"
    , "edit <nick> <login>"
    , "newacc <nick> [<login>]"
    , "new <nick> <url> [<login>]"
    , "search <subnick>"
    , "help"
    , "quit"
    ]

savess :: [Site] -> IO [Site]
savess x = savePasswords x >> return x


main :: IO ()
main  = do
  ss <- loadPasswords 
  putStrLn help
  runInputT defaultSettings (loop ss)
    where
        loop :: [Site] -> InputT IO ()
        loop ss = do
            minput <- getInputLine "% "
            case T.words . T.pack <$> minput of
                Just [] -> return ()
                Just ["quit"] -> return ()
                Just ["help"] -> liftIO (putStrLn help) >> loop ss
                Just ["arm",nk]  -> liftIO (arm (searchSites ss nk)) >> loop ss
                Just ["edit",s,l]  -> liftIO (edit ss s l >>= savess) >>= loop 
                Just ("newacc":s:ll) -> liftIO (insertAccount ss s (listToMaybe ll) >>= savess) >>= loop
                Just ("new":s:u:ll)  -> do  
                                          ss1 <- liftIO $ do 
                                                    newss <- maybe ss (: ss) <$> newSite s u (listToMaybe ll) 
                                                    if validateSites newss then savess newss else signalError "ERROR: duplicate nick" >> return ss
                                          loop ss1
                Just ("neu":s:u:ll)  -> liftIO (newSite s u (listToMaybe ll) >>= savess . maybe ss (:ss)) >>= loop
                Just ["search",snk] -> liftIO (search ss snk) >> loop ss
                _ -> liftIO (putStrLn "-- I did not understand ...") >> loop ss
