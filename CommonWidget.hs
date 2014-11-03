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