{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Home where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Text.Julius(rawJS)

import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Network.Wai


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

{-  uid <- runDB $ getUserIdByUniqueUserEmail "peng@123.com"
    case uid of
                   -- lift io to the current monad layer
        Nothing -> liftIO $ print "Nothing to be delete, cannot find peng@123.com"
        Just theId -> runDB $ deleteUser theId
-}


testUser t1 = User "peng_pxt@163.com" "hah" "peng" AuthNormal "wahtandwhat" t1
testRoom t1 t2 = Room "1001" AuthNormal True t1 t2 

testBookingRoom aDay curTime t1 = do
    mayUserId <- runDB $ addNewUser (testUser t1)
    mayRoomId <- runDB $ addNewRoom (testRoom aDay t1)
    runDB $ bookingRoom (fromJust mayUserId) (fromJust mayRoomId) aDay (Timespan curTime curTime)
    
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text

    -- curDay will be passed to Juliu, to limit the day selection range to 2 month.
    curDT <- liftIO getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT
        curDayStr = show curDay
    -- liftIO $ print ("today in Day format: " ++ curDayStr ++ ". Time: " ++ (show curTime))
    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")

postHomeR :: Handler Value
postHomeR = do
    req <- waiRequest
    req' <- getRequest
    --liftIO $ print req
    --liftIO $ print "Get request body json"
    -- !res <- runRequestBody
    --liftIO $ print $ fst res
    --liftIO $ print $ reqLangs req'
    return $ object [ ("sEcho" :: Text) .= (1 :: Int) ]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing