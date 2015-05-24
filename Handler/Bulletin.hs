{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Bulletin where

import Import
import CommonWidget
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(isJust, fromJust)
import Yesod.Form.Bootstrap3 
import Database.Persist.Sql(toSqlKey)
import Yesod.Form
import Text.Julius(rawJS)

import qualified Data.Text as T 
import System.IO.Unsafe(unsafePerformIO)
import Data.Conduit
import Data.Time
import System.Locale
import Data.Text.Encoding(encodeUtf8)
import Data.ByteString.Lazy(fromStrict)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Data.Aeson(ToJSON(..), object, (.=), decode)
--import Text.Blaze (ToMarkup (toMarkup), unsafeByteString)
import Text.Blaze.Html (toHtml)

getConfRoomBulletinR :: Handler Html
getConfRoomBulletinR = do
    rawContent <- runDB $ getBulletinContent
    --liftIO $ print rawContent
    --let !content = toHtml rawContent  
    let content = rawContent   
    defaultLayout $ do
        bulletinId <- newIdent
        $(widgetFile "bulletin")

getEditRoomBulletinR :: Handler Html
getEditRoomBulletinR = do
    content <- runDB $ getBulletinContent
    (editBulletinWidget, formEnctype) <- generateFormPost (bulletinEditForm content)    
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getEditRoomBulletinR" :: Text
    defaultLayout $ do
        editBulletinFormId <- newIdent
        $(widgetFile "bulletinEdit")
    
postEditRoomBulletinR :: Handler Html
postEditRoomBulletinR = do
    content <- runDB $ getBulletinContent
    ((result, formWidget), formEnctype) <- runFormPost (bulletinEditForm content)
    case result of
        FormSuccess formInfo -> do
            -- liftIO $ print (bulletinEditContent formInfo) 
            runDB $ updateBulletinContent (unTextarea $ bulletinEditContent formInfo)
            defaultLayout $ do
                backNavWidget ("会议室公告更新完成" :: Text) ("") HomeR
        _ -> defaultLayout $ do
                backNavWidget emptyText ("内部错误，请重新编辑" :: Text) HomeR


{-
FieldSettings    
fsLabel :: SomeMessage master,
fsTooltip :: Maybe (SomeMessage master),
fsId :: Maybe Text,
fsName :: Maybe Text,
fsAttrs :: [(Text, Text)]
-}

bulletinSetting = FieldSettings "" Nothing Nothing Nothing 
                       [("style", "width:500px; height:600px")] --("type","text")

data BulletinText = BulletinText { bulletinEditContent :: Textarea}

bulletinEditForm :: Text -> Form BulletinText
bulletinEditForm content = renderBootstrap3 bulletinFormLayout $ 
    BulletinText       
        <$> areq textareaField bulletinSetting (Just $ Textarea content)
