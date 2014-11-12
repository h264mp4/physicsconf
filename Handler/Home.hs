{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Home where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Text.Julius(rawJS)

import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Yesod.Auth
import Text.Julius(rawJS)
   
getHomeR :: Handler Html
getHomeR = do
    -- curDay will be passed to Juliu, 
    -- to limit the day selection range to 2 month.
    curDT <- liftIO getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT
        curDayStr = show curDay
    maid <- maybeAuthId
    mayUserInfo <- do
        case maid of 
            Nothing -> return Nothing
            Just theEmail -> runDB $ getUserInfoByUniqueUserEmail theEmail
        
    defaultLayout $ do
        postId <- newIdent
        $(widgetFile "homepage")


postHomeR :: Hander Html
postHomeR = do
