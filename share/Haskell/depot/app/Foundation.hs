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

module Foundation where

import           Yesod
import           Yesod.Form.Jquery
import           Data.Text (Text, unpack, pack, concat)
import qualified Database.Esqueleto as E

import Model

data Depot = Depot { depPool :: E.ConnectionPool , depRoot :: Text , depUploadsDir :: Text }

mkYesodData "Depot" $(parseRoutesFile "config/routes")

instance Yesod Depot
    where
      approot = ApprootMaster depRoot
      maximumContentLength _ p = case p of
        (Just UploadR) -> Just (1024 * 1024 * 1024)
        _ -> Just (1024 * 1024)

instance YesodPersist Depot where
    type YesodPersistBackend Depot = E.SqlBackend
    runDB action = do
        Depot pool rt dud <- getYesod
        E.runSqlPool action pool

instance RenderMessage Depot FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodJquery Depot

