{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Booking where

import Yesod.Form.Types
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Text.Julius(rawJS)
import Text.Read(readMaybe)
import Data.List(elem)
import Data.Either
import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Yesod.Auth
import qualified Data.Text as T

getBookingR :: Handler Html
getBookingR = do
    maid <- maybeAuthId
    case maid of 
        Nothing -> redirect (AuthR LoginR)
        Just theEmail -> do
          Entity theUserId theUser <- runDB $ getBy404 theEmail
          roomEntities <- runDB $ listRoomProfile
          mayDay <- lookupGetParam "selectDay"          
          mayRoomId <- lookupGetParam "selectRoom"
          let theLevel = userLevel theUser
              theDay = getPreferDay (fmap T.strip mayDay)
              availableRoomPairs = getRoomPair . filterRoomByLevel theLevel $ roomEntities
              theRoom = getPreferRoom mayRoomId availableRoomParis              
          (newbookingWidget, formEnctype) <- 
                   generateFormPost (newbookingForm theUserId theDay theRoom availableRoomPairs)
          defaultLayout $ do
              addRoomFormId <- newIdent
              $(widgetFile "booking")

    where
    filterRoomByLevel theLevel = filter (\ Entity _ roomInfo -> roomLevel <= theLevel)
    getRoomPair = map (\ Entity roomid roominfo -> (roomNumber roominfo, roomid))    
    -- mayDay format: YYYY-MM-DD
    getPreferDay mayDay 
             | mayDay == Nothing = Nothing
             | otherwise = let dayText = fromJust mayDay
                               ymd = map (read . T.unpack) $ T.splitOn "-" dayText
                           in if (T.length dayText /= 10) || (length ymd /= 3)
                              then Nothing 
                              else Just $ fromGregorian (fromIntegral $ ymd!!0) (ymd!!1) (ymd!!2)

    -- a string of int64 key 
    getPreferRoom mayRoomId availableRoomPairs = 
        case mayRoomId of
            Nothing -> Nothing
            Just rooidObj -> 
                 case (readMaybe . unpack . selectRoom $ roomObj) :: Int64 of
                     Nothing -> Nothing
                     Just intKey -> 
                         let roomKey = toSqlKey intKey
                             bIn = elem roomKey (map snd availableRoomPairs)
                          in if bIn
                                then Just roomKey
                                else Nothing

postBookingR :: Handle Html
postBookingR = do
    -- TODO: add a return link
    ((result, formWidget), formEnctype) <- 
                           runFormPost $ newbookingForm (toSqlKey 0) Nothing Nothing []
    let handlerName = "postBookingR" :: Text        
    case result of
        FormSuccess bookingInfo -> do
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


        let submission = Nothing :: Maybe (FileInfo, Text)

            handlerName = "getAddRoomR" :: Text
                              
------------------------------------------------------------------------------------------
---- other helpers

daySetting      = FieldSettings (fromString "预定日期") Nothing (Just "daySid") Nothing []
roomSetting     = FieldSettings (fromString "会议室") Nothing (Just "roomSid") Nothing []
startDaySetting = FieldSettings (fromString "开始时间") Nothing (Just "startDaySid") Nothing []
endDaySetting   = FieldSettings (fromString "结束时间") Nothing (Just "endDaySid") Nothing []

-- using jsp to do all validation.
newbookingForm :: UserId -> Maybe Day -> Maybe RooId -> [(Text, RoomId)] -> Form Record
newbookingForm theUserId theDay theRoom roomPairs = renderBootstrap3 commonSimpleFormLayout $ 
    Record
    <$> pure theUserId
    <*> areq (jqueryDayField def {jdsChangeMonth = True}) daySetting theDay 
    <*> areq (selectFieldList roomPairs) roomSetting theRoom
    <*> areq (selectFieldList hourPairs) startDaySetting Nothing
    <*> areq (selectFieldList hourPairs) endDaySetting Nothing
    <*> lift (liftIO $ getCurrentTime)
    <*> pure False
    where       
    hourPairs :: [(Text, TimeOfDay)]
    hourPairs = zipWith toHourPair [7..24] [7..24]
    toHourPair a b = (T.pack $ show a, TimeOfDay b 0 0)

newBookingWidget :: Widget
newBookingWidget = toWidget $ [hamlet|
$newline never
<div class="row">
    <form class="form-horizontal control-label" method="post" action="@{HomeR}">
        <div class="col-md-2" align="left">
            <lable for="theday">日期
        <div class="col-md-10" align="left">
            <input type="text" name="日期" id="theday">
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-2" align="left">
            <lable for="theroom">会议室号
        <div class="col-md-10" align="left">
            <select name="">
                <option value="0">divcss5</option>
                <option value="1">DIVCSS5</option>
            </select><br />

            <input type="password" name="会议室号" id="theroom">
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-4" align="left">
            <input class="btn btn-info" type="submit" value="预定">    
|]

loginWidget authToMaster url = toWidget $ [hamlet|
|]    
