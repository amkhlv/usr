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
    , "tags <tag1> [<tag2> ...]"
    , "clean <nick>"
    , "seturl <nick> <newurl>"
    , "setnick <nick> <newnick>"
    , "show <nick> <login>"
    , "showAll <nick> <login> --- will include secret notes"
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
            minput <- getInputLine "━━━━━━━┥"
            case T.words . T.pack <$> minput of
                Nothing -> return ()
                Just [] -> loop ss
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
                Just ["seturl",nk,u] -> liftIO (savess (changeURL nk u ss)) >>= loop
                Just ["setnick",nk,nk1] -> liftIO (
                                                    let ss1= changeNick nk nk1 ss 
                                                    in 
                                                    if validateSites ss1 then savess ss1 else signalError "ERROR: that nick already exists" >> return ss
                                                    ) >>= loop
                Just ["search",snk] -> liftIO (search ss snk) >> loop ss
                Just ["show",nk,l] -> liftIO (findAndShowAccount ss nk l) >> loop ss
                Just ("tags":tgs) -> liftIO (withtags tgs ss) >> loop ss
                Just ["showAll",nk,l] -> liftIO (let ms = searchSitesX ss nk in maybe (putStrLn "nick not found") (showAll l) ms) >> loop ss
                Just ["clean",nk] -> liftIO (cleanupSite nk ss >>= savess) >>= loop
                _ -> liftIO (putStrLn "-- I did not understand ...") >> loop ss
