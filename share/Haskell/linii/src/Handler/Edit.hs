{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}




module Handler.Edit where

import Import
import Database.Persist.Sql (toSqlKey)
import Handler.Home
import Handler.New

idEntityAddress :: Entity Address -> Entity Address
idEntityAddress ea = ea

getEditR :: Int64 -> Handler Html
getEditR i = do
  maybeAddress <- runDB $ get (toSqlKey i)
  case maybeAddress of
    Just a -> do
      (editWidget, editEnctype) <- generateFormPost (editForm a)
      (deleteWidget, deleteEnctype) <- generateFormPost deleteForm
      defaultLayout $ do
        setTitle "EditAbkEntry"
        $(widgetFile "edit")
    Nothing -> defaultLayout $(widgetFile "error-in-edit-get")

postEditR :: Int64 -> Handler Html
postEditR i = do
    ((editResult, _), _) <- runFormPost (let n = Nothing in editForm $ Address "Last" n "Date" n n n n n n n n n n n)
    case editResult of
      FormSuccess res -> do
        maybeAddress <- runDB $ selectFirst [ AddressId ==. (toSqlKey i) ] []
        let ma = fmap idEntityAddress maybeAddress
        case ma of
          Just a -> runDB $ delete (entityKey a)
          _ -> liftIO $ putStrLn "ERROR: entry to be deleted is absent"
        _ <- runDB $ insert (a1toAddress res)
        getHomeR
      _ -> defaultLayout $(widgetFile "error-in-edit-post")


data Foo = Foo{ x :: Text } deriving Show

deleteForm :: Html -> MForm Handler (FormResult Foo, Widget)
deleteForm = renderDivs $ Foo <$> areq hiddenField "" (Just "foo")

editForm :: Address -> Html -> MForm Handler (FormResult A1, Widget)
editForm a = renderDivs $ A1
  <$> areq textField "Last Name" (Just (addressLast a))
  <*> aopt textField "First Name" (Just (addressFirst a))
  <*> areq textField "Date When Collected" (Just (addressDatecollected a))
  <*> aopt textareaField "email" (Just (fmap Textarea  (addressEmail a)))
  <*> aopt textareaField "workphone" (Just (fmap Textarea (addressWorkphone a)))
  <*> aopt textareaField "homephone" (Just (fmap Textarea (addressHomephone a)))
  <*> aopt textareaField "fax" (Just (fmap Textarea (addressFax a)))
  <*> aopt textareaField "cellphone" (Just (fmap Textarea (addressCellphone a)))
  <*> aopt textareaField "homeaddress" (Just (fmap Textarea (addressHomeaddress a)))
  <*> aopt textareaField "workaddress" (Just (fmap Textarea (addressWorkaddress a)))
  <*> aopt textareaField "website" (Just (fmap Textarea (addressWebsite a)))
  <*> aopt textareaField "tags" (Just (fmap Textarea (addressTags a)))
  <*> aopt textareaField "birthday" (Just (fmap Textarea (addressBirthday a)))
  <*> aopt textareaField "notes" (Just (fmap Textarea (addressNotes a)))


postDeleteR :: Int64 -> Handler Html
postDeleteR i = do
    ((deleteResult, _), _) <- runFormPost deleteForm 
    case deleteResult of
      FormSuccess res -> do
        maybeAddress <- runDB $ selectFirst [ AddressId ==. (toSqlKey i) ] []
        case maybeAddress of
          Just a -> runDB $ delete (entityKey a)
          _ -> liftIO $ putStrLn "ERROR: entry to be deleted is absent"
        getHomeR
      _ -> defaultLayout $(widgetFile "error-in-edit-post")
