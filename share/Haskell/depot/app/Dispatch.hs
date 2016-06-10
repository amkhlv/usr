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

module Dispatch where

import           Yesod
import           Yesod.Form.Jquery
import           Data.Text (Text, unpack, pack, concat)
import           Data.Text.Encoding (encodeUtf8)
import           Data.String (String)
import qualified Data.ByteString.Lazy as DBSL
import           Data.Time.Clock
import           Data.Time.ISO8601
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist.Sqlite
import           System.FilePath
import           System.Directory (removeFile, doesFileExist)
import qualified Database.Esqueleto as E
import           Crypto.PasswordStore
import           Yesod.Default.Util

import Foundation
import Model

mkYesodDispatch "Depot" resourcesDepot

loginForm :: Html -> MForm Handler (FormResult User, Widget)
loginForm = renderDivs $ User
    <$> areq textField "login" Nothing
    <*> areq passwordField "password" Nothing

uploadForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadForm = renderDivs $ fileAFormReq "file"

data Foo = Foo{ x :: Text } deriving Show

fooForm :: Html -> MForm Handler (FormResult Foo, Widget)
fooForm = renderDivs $ Foo <$> areq hiddenField "" (Just "foo")

getAuthR :: Handler Html
getAuthR = do
  (widget, enctype) <- generateFormPost loginForm
  defaultLayout $(widgetFileNoReload def "login")

postAuthR :: Handler Html
postAuthR = do
  ((result, widget), enctype) <- runFormPost loginForm
  case result of
    FormSuccess user ->
      do
        case user of
          User nm pass -> do
            -- p <- runDB $ selectList [UserUserName ==. nm, UserPassword ==. pass] [LimitTo 1]
            pp <- runDB
              $ E.select 
              $ E.from $ \u -> do
                E.where_ (u E.^. UserUserName E.==. E.val nm)
                return (u E.^. UserPassword)
            case length pp of
              1 -> do
                let p = E.unValue (head pp)
                if (verifyPassword (encodeUtf8 pass) (encodeUtf8 p))
                  then do setSession "username" nm
                          getUploadR
                  else defaultLayout $(widgetFileNoReload def "wrong-password")
              0 -> defaultLayout $(widgetFileNoReload def "no-such-user")
              _ -> defaultLayout [whamlet|<p>ERROR: user registered more than once|]
    _ -> defaultLayout $ do
      [whamlet|<p>Invalid input, let's try again.|]
      $(widgetFileNoReload def "login")


getUploadR :: Handler Html
getUploadR = do
  (widget, enctype) <- generateFormPost uploadForm
  (wid, enc) <- generateFormPost fooForm
  sess <- getSession
  mu <- lookupSession $ pack "username"
  case mu of
    Just u -> do
      fs <- runDB
        $ E.select
        $ E.from $ \f -> do
          E.where_ (f E.^. UploadedFileOwnerName E.==. E.val u)
          return f
      defaultLayout $ do
        $(widgetFileNoReload def "upload")
        sequence_ $ map (\fl -> $(widgetFileNoReload def "file-item")) fs
    Nothing ->
       defaultLayout
         [whamlet|<p>not authorized|]

postUploadR :: Handler Html
postUploadR = do
  mu <- lookupSession $ pack "username"
  case mu of
    Just u -> do
      ((result, widget), enctype) <- runFormPost uploadForm
      case result of
        FormSuccess fileinfo -> do
          ts <- liftIO $ fmap formatISO8601Millis getCurrentTime
          Depot pl rt ud <- getYesod
          liftIO $ writeToServer fileinfo ud $ (unpack u) ++ "_" ++ ts
          runDB
            $ insert $ UploadedFile u (fileName fileinfo) (fileContentType fileinfo) ts
          getUploadR
        _ -> defaultLayout
          [whamlet|<p>Something wrong, no file upload|]
    Nothing ->
       defaultLayout
         [whamlet|<p>not authorized|]

writeToServer :: FileInfo -> Text -> String -> IO ()
writeToServer file dirname filename = do
  let path = (unpack dirname) </> filename 
  fileMove file path

getDownloadR :: String -> Handler TypedContent
getDownloadR ts = do
  mu <- lookupSession $ pack "username"
  case mu of
    Just u -> do
      fs <- runDB
        $ E.select
        $ E.from $ \f -> do
          E.where_ (f E.^. UploadedFileOwnerName E.==. E.val u  E.&&.  f E.^. UploadedFileTimeStamp E.==. E.val ts)
          return f
      Depot pl rt ud <- getYesod
      addHeader "Content-Disposition" $ Data.Text.concat
        [ "attachment; filename=\"", uploadedFileFileName (entityVal (head fs)), "\""]
      filecont <- liftIO $ DBSL.readFile ((unpack ud) </> ((unpack u) ++ "_" ++ ts))
      sendResponse (encodeUtf8 (uploadedFileContentType (entityVal (head fs))),
                    toContent filecont)
    Nothing -> do
      return $ TypedContent "" (ContentBuilder "not authorized" Nothing)

postDeleteR :: String -> Handler Html
postDeleteR ts = do
  mu <- lookupSession $ pack "username"
  case mu of
    Just u -> do
      ((result, widget), enctype) <- runFormPost fooForm
      case result of
        FormSuccess _ -> do
          Depot pl rt ud <- getYesod
          let filepath = (unpack ud) </> ((unpack u) ++ "_" ++ ts)
          liftIO $ removeFile filepath
          runDB
            $ E.delete $
                E.from $ \f -> do
                E.where_ (f E.^. UploadedFileTimeStamp E.==. E.val ts E.&&. f E.^. UploadedFileOwnerName E.==. E.val u)
          getUploadR
        _ -> defaultLayout
          [whamlet|<p>Something wrong, no file deletion|]
    Nothing ->
       defaultLayout
         [whamlet|<p>not authorized|]
