{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.UserBookingManage where

import Import
import Yesod.Auth
import Text.Julius(rawJS)

import Data.Text.Encoding(encodeUtf8)
import Data.ByteString.Lazy(fromStrict)
import Data.Maybe(fromJust,isJust)
import Data.Aeson(ToJSON(..), object, (.=), decode)
import Data.Conduit
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import qualified Data.Text(pack)

import Handler.DBOperation
import Handler.MiscTypes
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


deleteCancelBookingR :: Handler Value
deleteCancelBookingR = do
    texts <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
    case decode . fromStrict . encodeUtf8 $ texts !! 0 of
        Just cancelObj -> do
            doCancel cancelObj
            return $ object $ [("ret" :: Text) .= ("ok" :: Text)]
        Nothing -> return $ object $ [("ret" :: Text) .= ("invalid data" :: Text)]

    where 
    doCancel cancelObj = do
        case strToSqlKey . cancelId $ cancelObj of
            Nothing -> return ()
            Just theId -> runDB $ cancelABooking (theId :: Key Record) >> return ()