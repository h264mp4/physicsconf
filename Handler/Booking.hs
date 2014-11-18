{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Booking where

import Import
import Yesod.Auth
import Yesod.Form.Types
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 
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
    maid <- maybeAuthId
    case maid of 
      Nothing -> redirect (AuthR LoginR)
      Just theEmail -> do
          (Entity theUserId theUser) <- runDB $ getBy404 $ UniqueEmail theEmail
          roomEntities <- runDB $ listRoomProfile
          mayDay <- lookupGetParam "selectDay"          
          mayRoomId <- lookupGetParam "selectRoom"
          let theLevel = userLevel theUser
              theDay = getPreferDay (fmap T.strip mayDay)
              availableRoomPairs = getRoomPair . filterRoomByLevel theLevel $ roomEntities
              theRoom = getPreferRoom mayRoomId availableRoomPairs              
          (newbookingWidget, formEnctype) <- 
                   generateFormPost (newbookingForm theUserId theDay theRoom availableRoomPairs)
          defaultLayout $ do
              newBookingFormId <- newIdent
              aNewTable <- newIdent
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
              theDay = getPreferDay (fmap T.strip mayDay)
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

-- using jsp to do all validation.
newbookingForm :: UserId -> Maybe Day -> Maybe RoomId -> [(Text, RoomId)] -> Form Record
newbookingForm theUserId theDay theRoom roomPairs = renderBootstrap3 commonSimpleFormLayout $ 
    Record
        <$> pure theUserId
        <*> areq (jqueryDayField  def{jdsChangeMonth = True}) daySetting theDay 
        <*> areq (selectFieldList roomPairs) roomSetting theRoom
        <*> areq (selectFieldList hourStartPairs) startDaySetting Nothing
        <*> areq (selectFieldList hourEndPairs) endDaySetting Nothing
        <*> lift (liftIO $ getCurrentTime)
        <*> pure False
    where       
    hourStartPairs, hourEndPairs :: [(Text, TimeOfDay)]
    hourStartPairs = zipWith toHourPair [7..23] [7..23]
    hourEndPairs   = zipWith toHourPair [8..24] [8..24]
    toHourPair a b = (T.pack $ show a, TimeOfDay b 0 0)

toHtmlBookingInfo :: Record -> Text -> Text
toHtmlBookingInfo bookingInfo roomNoStr = (
    "预订日期: " <> (T.pack . show . recordDay $ bookingInfo) <> "<br />  " <>
    "预订会议室: " <> (roomNoStr) <> "<br />  " <>
    "开始时间: " <> (T.pack . show . recordStartTime $ bookingInfo) <> "<br />  " <>
    "结束时间: " <> (T.pack . show . recordEndTime $ bookingInfo) <> "<br />  " <>
    "<br />")
