{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Booking where

import Import
import Yesod.Auth
import Yesod.Form.Types
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 
import Yesod.Static
import Settings.StaticFiles
import Text.Julius(rawJS)
import Text.Read(readMaybe)

import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import CommonWidget

import Data.Int(Int64)
import Data.List(elem)
import Data.Either
import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Database.Persist.Sql(toSqlKey)
import qualified Data.Text as T

getBookingR :: Handler Html
getBookingR = do
    curDT <- liftIO getCurDayAndTime
    let curDate = localDay curDT
        endDate = addGregorianMonthsClip 2 curDate
        curDateStr = show curDate
        endDateStr = show endDate
    maid <- maybeAuthId
    case maid of 
      Nothing -> redirect (AuthR LoginR)
      Just theEmail -> do
          (Entity theUserId theUser) <- runDB $ getBy404 $ UniqueEmail theEmail
          roomEntities <- runDB $ listRoomProfile
          mayDay <- lookupGetParam "selectDay"          
          mayRoomId <- lookupGetParam "selectRoom"
          let theLevel = userLevel theUser
              mayTheDay = mayStrToDay (fmap T.strip mayDay)
              availableRoomPairs = getRoomPair . filterRoomByLevel theLevel $ roomEntities
              theRoom = getPreferRoom mayRoomId availableRoomPairs
          (newWidget, formEnctype) <- 
                   generateFormPost (newbookingForm theUserId mayTheDay 
                                                    theRoom availableRoomPairs)
          -- very fucky bug, generated form will include jquery & jqueryUI, we have already 
          -- included bootstrapTable eailier, but it won't work after reinclude jquery&ui
          -- so we have to include it again, otherwise it will complain of undefined function.
          -- what a fuck bug!! we hack it by using widget combine.
          -- why javascript re included have no protection like c's header guard?
          let newbookingWidget = newWidget >> (addScript $ StaticR js_bootstrap_table_min_js)
          defaultLayout $ do
              aNewTable <- newIdent
              recordFormId <- newIdent
              showDateText <- newIdent
              $(widgetFile "booking")

postBookingR :: Handler Html
postBookingR = do
    maid <- maybeAuthId
    case maid of 
      Nothing -> redirect (AuthR LoginR)
      Just theEmail -> do
          (Entity theUserId theUser) <- runDB $ getBy404 $ UniqueEmail theEmail
          roomEntities <- runDB $ listRoomProfile
          mayDay <- lookupGetParam "selectDay"          
          mayRoomId <- lookupGetParam "selectRoom"
          let theLevel = userLevel theUser
              theDay = mayStrToDay (fmap T.strip mayDay)
              availableRoomPairs = getRoomPair . filterRoomByLevel theLevel $ roomEntities
              theRoom = getPreferRoom mayRoomId availableRoomPairs              
             
          ((result, _), formEnctype) <- 
                        runFormPost $ newbookingForm theUserId theDay theRoom availableRoomPairs
          liftIO $ print result
          case result of
              FormSuccess bookingInfo -> do
                  mayRecordId <- runDB $ bookingRoom bookingInfo
                  case mayRecordId of
                       Nothing -> defaultLayout $ do
                           backNavWidget emptyText ("此时段会议室已经被预定." :: Text) BookingR
                       Just recordId -> do                     
                           liftIO $ print ("Add new Record done: " ++ (show recordId))
                           liftIO $ print bookingInfo
                           roomInfo <- runDB $ get404 $ recordRoomId bookingInfo
                           defaultLayout $ do
                               backNavWidget ("会议室预定成功，请查收确认邮件." :: Text) 
                                      (toHtmlBookingInfo bookingInfo (roomNumber roomInfo)) HomeR
              _ -> defaultLayout $ do
                       backNavWidget emptyText ("无效的会议室预定, 请重新输入." :: Text) BookingR

filterRoomByLevel theLevel = filter (\(Entity _ roomInfo) -> (roomLevel roomInfo) <= theLevel)
getRoomPair = map (\(Entity roomid roominfo) -> (roomNumber roominfo, roomid))    

-- a string of int64 key 
getPreferRoom mayRoomId availableRoomPairs = 
    case mayRoomId of
        Nothing -> Nothing
        Just roomIdStr -> 
             case ((readMaybe . T.unpack $ roomIdStr) :: Maybe Int64) of
                 Nothing -> Nothing
                 Just intKey -> 
                     let roomKey = toSqlKey intKey
                         bIn = elem roomKey (map snd availableRoomPairs)
                      in if bIn
                            then Just roomKey
                            else Nothing

------------------------------------------------------------------------------------------
---- other helpers
daySetting      = FieldSettings ("预定日期") Nothing (Just "daySid") Nothing []
roomSetting     = FieldSettings ("会议室") Nothing (Just "roomSid") Nothing []
startDaySetting = FieldSettings ("开始时间") Nothing (Just "startDaySid") Nothing []
endDaySetting   = FieldSettings ("结束时间") Nothing (Just "endDaySid") Nothing []
roomUsageSetting = FieldSettings ("会议室用途") Nothing (Just "roomUsageSid") Nothing []
otherUsageSetting = FieldSettings ("") Nothing (Just "otherUsageSid") Nothing []

-- using jsp to do all validation.
newbookingForm :: UserId -> Maybe Day -> Maybe RoomId -> [(Text, RoomId)] -> Form Record
newbookingForm theUserId theDay theRoom roomPairs = renderBootstrap3 commonSimpleFormLayout $ 
    Record
        <$> pure theUserId
        <*> areq (jqueryDayField2  def{jdsChangeMonth = True}) daySetting theDay 
        <*> areq (selectFieldList roomPairs) roomSetting theRoom
        <*> areq (selectFieldList hourStartPairs) startDaySetting Nothing
        <*> areq (selectFieldList hourEndPairs) endDaySetting Nothing
        <*> lift (liftIO $ getCurrentTime)
        <*> areq (selectFieldList roomUsagePairs) roomUsageSetting Nothing
        <*> aopt textField otherUsageSetting Nothing
        <*> pure False

    where       
    hourStartPairs, hourEndPairs :: [(Text, TimeOfDay)]
    hourStartPairs = zipWith toHourPair bookingStartPeriod bookingStartPeriod
    hourEndPairs   = zipWith toHourPair bookingEndPeriod bookingEndPeriod

    toHourPair a b = (T.pack $ show a, TimeOfDay b 0 0)
    roomUsagePairs :: [(Text, RoomUsage)]
    roomUsagePairs = [ ("组会"   , UsageZuHui)
                      ,("学术会议", UsageXueShuHuiYi)
                      ,("学术报告", UsageXueShuBaoGao)
                      ,("研讨会"  , UsageYanTaoHui)
                      ,("其他"    , UsageOther)
                     ]

toHtmlBookingInfo :: Record -> Text -> Text
toHtmlBookingInfo bookingInfo roomNoStr = (
    "预订日期: "   <> (T.pack . show . recordDay $ bookingInfo)       <> "<br />  " <>
    "预订会议室: " <> (roomNoStr)                                     <> "<br />  " <>
    "开始时间: "   <> (T.pack . show . recordStartTime $ bookingInfo) <> "<br />  " <>
    "结束时间: "   <> (T.pack . show . recordEndTime $ bookingInfo)   <> "<br />  " <>
    "会议室用途: " <> (getRoomUsageInfo bookingInfo)                  <> "<br />  " <>
    "<br />" )
