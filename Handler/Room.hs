{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Room where

import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 

import Import
import CommonWidget
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils

import Control.Monad(when)
import Database.Persist.Sql(toSqlKey)
import Data.Text(unpack)
import Data.Conduit
import Data.Text.Encoding(encodeUtf8)
import Data.ByteString.Lazy(fromStrict)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import qualified Data.Text(pack)
import Data.Maybe(fromJust,isJust)
import Data.Aeson(ToJSON(..), object, (.=), decode)

------------------------------------------------------------------------------------------
---- AddRoom

getAddRoomR :: Handler Html
getAddRoomR = do
    (addRoomWidget, formEnctype) <- generateFormPost addRoomForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getAddRoomR" :: Text

    defaultLayout $ do
        addRoomFormId <- newIdent
        $(widgetFile "addroom")

postAddRoomR :: Handler Html
postAddRoomR = do
    ((result, formWidget), formEnctype) <- runFormPost addRoomForm
    let handlerName = "postAddRoomR" :: Text
    case result of
        FormSuccess formInfo -> do
            mayRoomId <- runDB $ addNewRoom formInfo
            case mayRoomId of
                 Nothing -> defaultLayout $ do
                          backNavWidget emptyText ("会议室信息已存在，请重新输入" :: Text) ManageRoomR
                 Just roomId -> do
                     liftIO $ print ("Add new room done: " ++ show (fromJust mayRoomId))
                     liftIO $ print formInfo
                     defaultLayout $ do
                         backNavWidget ("会议室信息已保存" :: Text) 
                                       (toHtmlRoomInfo formInfo) ManageRoomR
        _ -> defaultLayout $ do
                 backNavWidget emptyText ("无效的会议室信息, 请重新输入." :: Text) ManageRoomR

------------------------------------------------------------------------------------------
---- list room

getListRoomR :: Handler Value
getListRoomR = do
    -- TODO: User Auth Widget
    rooms <- runDB $ listRoomProfile
    if null rooms
       then return $ object $ []
       else do
            return $ object $ ["dataRows" .= (map toJSON rooms), 
                               "total" .= toJSON (length rooms :: Int)
                              ]

-- edit room will only edit the valid date / available / level
getEditRoomR :: Handler Html
getEditRoomR = do
    mayId <- lookupGetParam "editId"
    liftIO $ print $ "get editroom param: "
    liftIO $ print $ mayId
    let bValidData = isJust mayId
    if not bValidData
       then notFound
       else do
            let theId = (toSqlKey . read . unpack . fromJust $ mayId) :: RoomId
            roomInfo <- runDB $ get404 theId
            (editRoomWidget, formEnctype) <- generateFormPost (editRoomForm $ Just roomInfo)
            let submission = Nothing :: Maybe (FileInfo, Text)
                handlerName = "getEditRoomR" :: Text
            defaultLayout $ do
                editRoomFormId <- newIdent
                $(widgetFile "editroom")

postFinishEditRoomR :: RoomId -> Handler Html
postFinishEditRoomR theId = do
    liftIO $ print theId
    theInfo <- runDB $ get404 theId
    ((result, formWidget), formEnctype) <- runFormPost (editRoomForm . Just $ theInfo)
    let handlerName = "postFinishEditRoomR" :: Text
    liftIO $ print result
    case result of
        FormSuccess formInfo -> do
            runDB $ updateRoomProfile theId formInfo {roomNumber = (roomNumber theInfo)}
            defaultLayout $ do
                backNavWidget ("会议室信息已更新" :: Text) 
                                   (toHtmlRoomInfo formInfo) ManageRoomR
        _ -> defaultLayout $ do
                 backNavWidget emptyText ("无效的会议室信息, 请重新输入." :: Text) ManageRoomR
    
deleteDeleteRoomR :: Handler Value
deleteDeleteRoomR = do
    texts <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
    liftIO $ print texts
    let mayId = decode . fromStrict . encodeUtf8 $ texts !! 0
        bValidData = isJust mayId
    if bValidData
       then do
            doDelete $ fromJust mayId
            return $ object $ [("ret" :: Text) .= ("ok" :: Text)]
       else return $ object $ [("ret" :: Text) .= ("invalid data" :: Text)]

    where 
    doDelete deleteObj = do
        let theId = toSqlKey . read . unpack . deleteId $ deleteObj
        runDB $ deleteRoom (theId :: Key Room)
        return ()

------------------------------------------------------------------------------------------
---- other helpers

simpleFormLayoutForAddRoom = BootstrapHorizontalForm
                             {
                                  bflLabelOffset = ColMd 0
                                 ,bflLabelSize   = ColMd 4
                                 ,bflInputOffset = ColMd 0
                                 ,bflInputSize   = ColMd 4
                             }

toHtmlRoomInfo :: Room -> Text
toHtmlRoomInfo roomInfo = (
    "会议室编号: " <> (roomNumber roomInfo) <> "<br />  " <>
    "预订权限: " <> (toLevelString $ roomLevel roomInfo) <> "<br />  " <>
    "即时启用: " <> (boolToHanzi $ roomAvailable roomInfo) <> "<br />  " <>
    "会议室有效期至: " <> (Data.Text.pack $ show $ roomValidTime roomInfo) <> "<br />  " <>
    "会议室添加日期: " <> (Data.Text.pack $ show $ convertUtcToZoneTime $ roomFirstAdd roomInfo) <> 
    "<br />")


addRoomForm :: Form Room
addRoomForm = renderBootstrap3 simpleFormLayoutForAddRoom $ Room
        <$> areq textField "会议室编号" Nothing
        <*> areq (selectFieldList authLevel) "预订权限" Nothing
        <*> areq boolField "是否现在启用" (Just True)
        <*> areq (jqueryDayField def {jdsChangeMonth = True, jdsChangeYear = True}) 
                                 "会议室有效期至" (Just $ fromGregorian 2028 1 1)
        <*> lift (liftIO $ getCurrentTime)

editRoomForm :: Maybe Room -> Form Room
editRoomForm roomInfo = renderBootstrap3 simpleFormLayoutForAddRoom $ Room
        <$> pure (fromJust $ roomNumber <$> roomInfo)
        <*> areq (selectFieldList authLevel) "预订权限" (roomLevel <$> roomInfo)
        <*> areq boolField "是否现在启用" (roomAvailable <$> roomInfo)
        <*> areq (jqueryDayField def {jdsChangeMonth = True, jdsChangeYear = True}) 
                                 "会议室有效期至" (roomValidTime <$> roomInfo)
        <*> pure (fromJust $ roomFirstAdd <$> roomInfo)
