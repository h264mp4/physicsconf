{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.UserBookingManage where

import Import
import Yesod.Auth
import Text.Julius(rawJS)

import Handler.DBOperation
import CommonWidget

------------------------------------------------------------------------------------------
---- User Rrcord Manage.

getUserBookingManageR :: Handler Html
getUserBookingManageR = do
    maid <- maybeAuthId
    case maid of 
        Nothing -> redirect (AuthR LoginR)
        Just theEmail -> do
            maybeUserInfo <- runDB $ getUserInfoByUniqueUserEmail theEmail 
            curRecords <- runDB $ getUserBookingInfosByUserEmail theEmail False
            historyRecords <- runDB $ getUserBookingInfosByUserEmail theEmail True
            defaultLayout $ do
                currentBookingTable <- newIdent
                historyBookingTable <- newIdent
                cancelBookingClass  <- newIdent
                toWidget $(widgetFile "userbookingmanage")
