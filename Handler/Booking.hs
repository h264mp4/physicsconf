{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Booking where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Text.Julius(rawJS)

import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Yesod.Auth

/dobooking DoBookingR GET POST
bookingRoom theUserId theRoomId theDay timespan = do

getBookingR :: Hander Html
getBookingR = do
    maid <- maybeAuthId
    case maid of 
        Nothing -> redirect (AuthR LoginR)
        Just theEmail -> do
          Entity theUserId theUser <- runDB $ getBy404 theEmail
          roomEntities <- runDB $ listRoomProfile
          let theLevel = userLevel theUser
              availableRoomPairs = getRoomPair . filterRoomByLevel theLevel $ roomEntities
        
          (newbookingWidget, formEnctype) <- generateFormPost 
                                                 (newbookingForm theUserId availableRoomPairs)

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


        let submission = Nothing :: Maybe (FileInfo, Text)

            handlerName = "getAddRoomR" :: Text
                              
    where
    filterRoomByLevel theLevel = filter (\ Entity _ roomInfo -> roomLevel <= theLevel)
    getRoomPair = map (\ Entity roomid roominfo -> (roomNumber roominfo, roomid))    

    
newbookingForm :: [(Text, RoomId)] -> Form Record


carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq carYearField "Year" (carYear <$> mcar)
    <*> aopt (selectFieldList colors) "Color" (carColor <$> mcar)
    where
    colors :: [(Text, Color)]
    colors = [("Red", Red), ("Blue", Blue), ("Gray", Gray), ("Black", Black)]

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

Record
    userId UserId
    roomId RoomId
    day Day
    timespan Timespan
    bookingTime UTCTime
    cancel Bool
    UniqueRecord userId roomId day timespan
    deriving Show
