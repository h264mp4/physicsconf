{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module CommonWidget where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(fromJust)
import Data.Aeson(ToJSON(..), object, (.=))
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 
import Text.Julius(rawJS)

backNavWidget :: Text -> Text -> (Route App) -> Widget
backNavWidget title info theLink = toWidget [hamlet|
    <div class="row">
        <div class="col-md-12">
            <input type=button value="返回" class="btn btn-primary" onClick="location.href='@{theLink}'">
        <div class="col-md-12">
            <h3> #{title}
        <div class="col-md-12"> 
            <p> #{preEscapedToMarkup info}                     
|]

-- list user / room info, with a column that can edit & delete the item.
listinfoWidget :: (Route App) -> (Route App) -> (Route App) -> Text -> Text -> Widget
listinfoWidget listLink editLink deleteLink dataType aRandomId = $(widgetFile "listinfo")
    
--
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
