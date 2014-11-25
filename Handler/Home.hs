{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Home where

import Import
import Text.Julius(rawJS)

import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Yesod.Auth
import Text.Julius(rawJS)

import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import CommonWidget
   
getHomeR :: Handler Html
getHomeR = do
    -- req <- waiRequest
    -- req' <- getRequest
    -- liftIO $ print req
    -- liftIO $ print "Get request body json"
    --  
    -- !res <- runRequestBody
    -- liftIO $ print $ fst res
    -- liftIO $ print $ reqLangs req'
    --  
    -- ss <- getSession
    -- liftIO $ print ss

    -- curDay will be passed to Julius, 
    -- to limit the day selection range to 2 month.
    curDT <- liftIO getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT
        curDayStr = show curDay

    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeUser = case maybeUserInfo of
                        Nothing -> Nothing
                        Just (Entity _ aUser) -> Just aUser
    defaultLayout $ do
        newbookingId <- newIdent
        $(widgetFile "homepage")