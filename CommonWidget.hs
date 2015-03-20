{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module CommonWidget where

import Import
import Yesod.Auth
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 

import Text.Julius(rawJS)
import qualified Data.Text as T

import Handler.DBOperation
import Handler.MiscTypes

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
backNavWidget :: Text -> Text -> (Route App) -> Widget
backNavWidget slogen info theLink = toWidget [hamlet|
    <div class="row">
        <div class="col-md-12">
            <input type=button value="返回" class="btn btn-primary" onClick="location.href='@{theLink}'">
        <div class="col-md-12">
            <h3> #{slogen}
        <div class="col-md-12"> 
            <p> #{preEscapedToMarkup info}                     
|]

-- list user / room info, with a column that can edit & delete the item.
listinfoWidget :: (Route App) -> (Route App) -> (Route App) -> Text -> Text -> Widget
listinfoWidget listLink editLink deleteLink dataType aRandomId = $(widgetFile "listinfo")
    
userMenuWidget :: Maybe User -> Widget
userMenuWidget maybeUser = toWidget [hamlet|
    $maybe userInfo <- maybeUser
        <div class="row" id="separator">
            <pre> 
                <h4>你好, <b>#{userName userInfo}</b>
            <a href=@{AuthR LogoutR}> 
                <h6 align="left"> 注销
        <div class="row">
             <hr>                          

        <div class="row" id="functionalities">
            <ul class="nav nav-pills nav-stacked">
                <li> 
                    <a href="@{HomeR}">首页
                <li> 
                    <a href="@{UserBookingManageR}">我的预订管理
                <li> 
                    <a href="@{UserProfileManageR}">个人信息设置
                $if userLevel userInfo == AuthAdmin
                    <li> 
                        <a href="@{ManageR}">预定系统管理
                 

        <div class="row">
             <hr width="90%">                          

    $nothing 
        <div class="row" id="separator">
            <pre>
                <h4>请<a href=@{AuthR LoginR}>登入</a>进行预定
                

    <div class="row" id="intros">
        <ul class="nav nav-pills nav-stacked">
            <li> 
                <a href="##">会议室预订介绍
            <li> 
                <a href="##">其他介绍
|]

listUserBookingRecordsWidget :: UserId -> Widget
listUserBookingRecordsWidget theUserId = do
    curRecords <- handlerToWidget $ runDB $ getUserBookingInfosByUserId theUserId False
    historyRecords <- handlerToWidget $ runDB $ getUserBookingInfosByUserId theUserId True
    currentBookingTable <- handlerToWidget newIdent
    historyBookingTable <- handlerToWidget newIdent
    cancelBookingClass  <- handlerToWidget newIdent
    $(widgetFile "listuserbooking")

listUserBookingRecordsByEmailWidget :: Text -> Widget
listUserBookingRecordsByEmailWidget theEmail = do
    maybeUser <- handlerToWidget $ runDB $ getBy $ UniqueEmail theEmail
    case maybeUser of
        Just (Entity theUserId _) -> listUserBookingRecordsWidget theUserId
        Nothing ->  toWidget [hamlet| <p> 用户信息错误，不存在 #{theEmail} |]
    
-- @{UserProfileManageR}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
---- hacking for html5 input type=date
jqueryDayField2 :: (RenderMessage site FormMessage, YesodJquery site) => JqueryDaySettings -> Field (HandlerT site IO) Day
jqueryDayField2 jds = Field
    { fieldParse = parseHelper $ maybe
                  (Left MsgInvalidDay)
                  Right
              . readMay
              . T.unpack
    , fieldView = \theId name attrs val isReq -> do
        let inputType = "text" :: Text
        toWidget [shamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="#{inputType}" :isReq:required="" value="#{showVal val}">
|]
        --addScript' urlJqueryJs
        --addScript' urlJqueryUiJs
        --addStylesheet' urlJqueryUiCss
        toWidget [julius|
$(function(){
    var i = document.getElementById("#{rawJS theId}");
    if (i.type != "date") {
        $(i).datepicker({
            dateFormat:'yy-mm-dd',
            changeMonth:#{jsBool $ jdsChangeMonth jds},
            changeYear:#{jsBool $ jdsChangeYear jds},
            numberOfMonths:#{rawJS $ mos $ jdsNumberOfMonths jds},
            yearRange:#{toJSON $ jdsYearRange jds}
        });
    }
});
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (T.pack . show)
    jsBool True = toJSON True
    jsBool False = toJSON False
    mos (Left i) = show i
    mos (Right (x, y)) = concat
        [ "["
        , show x
        , ","
        , show y
        , "]"
        ]

    readMay :: Read a => String -> Maybe a
    readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

--------------------------------------------------------------------------------
