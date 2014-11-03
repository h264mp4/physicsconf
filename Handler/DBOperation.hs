{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Handler.DBOperation where

import Network.Mail.SMTP
import Data.Maybe(fromJust)
import Database.Persist.Class
import Database.Persist.Types

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.Map as M

import Import
import Handler.Utils
import Handler.MiscTypes

-- | All basic operations are defined here and will called by Handler or Widget.

------------------------------------------------------------------------------------------
-- | Some types and constants

data BookingStatus = BookingCancel | BookingSuccess
    deriving (Show, Eq, Ord)

data InfoKey = InfoUser | InfoRoom | InfoRecord

------------------------------------------------------------------------------------------
-- | User Operaions: addNewUser editUserProfile deleteUser

-- addNewUser : return Maybe (Key User)
addNewUser newUser = do
    --runMigration migrateAll
    userExist <- selectList [UserEmail ==. (userEmail newUser)] [LimitTo 1]
    if not (null userExist)
       then do let errMsg = "User" ++ (show $ userEmail newUser) ++
                              "Already Exists, should use others"
               liftIO $ print errMsg
               return Nothing
       else do
            newUserId <- insert newUser
            return $ Just newUserId

updateUserProfile theUserId newInfo = do
    -- only name, password and level can be editted.
    update theUserId [ UserName     =. (userName newInfo), 
                       UserPassword =. (userPassword newInfo),
                       UserLevel    =. (userLevel newInfo)
                     ]
    return ()

listUserProfile = do
    users <- selectList [] [Asc UserEmail]
    return (users :: [Entity User])   

deleteUser theUserId = do
    delete theUserId
    return ()

------------------------------------------------------------------------------------------
-- | Room operations: addNewRoom, editRoomProfile, deleteRoom
addNewRoom newRoom = do
    roomExist <- selectList [RoomNumber ==. (roomNumber newRoom)] [LimitTo 1]
    if not (null roomExist)
       then do let errMsg = "Room" ++ (show $ roomNumber newRoom) ++
                              "Already Exists, room configuration can be editted"
               liftIO $ print errMsg
               return Nothing
       else do
            newRoomId <- insert newRoom
            return $ Just newRoomId

updateRoomProfile theRoomId newInfo = do
    -- Only the following field can be changed
    update theRoomId [ RoomAvailable =. (roomAvailable newInfo), 
                       RoomValidTime =. (roomValidTime newInfo),
                       RoomLevel     =. (roomLevel newInfo)
                     ]
    return "OK"

listRoomProfile = do
    rooms <- selectList [] [Asc RoomId]
    return (rooms :: [Entity Room])

deleteRoom = delete -- theRoomId

------------------------------------------------------------------------------------------
-- | Booking management: bookingRoom,  deleteABooking

-- | TODO: we should check timespan overlapping
bookingRoom theUserId theRoomId theDay timespan = do
    -- check whether the record exist
    maybeExist <- getBy $ UniqueRecord theUserId theRoomId theDay timespan
    case maybeExist of 
        Just (Entity theId theValue) -> return theId
        Nothing -> do
            curTime <- liftIO $ getCurrentTime
            let newRecord = Record theUserId theRoomId theDay timespan curTime False          
            newRecordId <- insert newRecord
            maybeUser <- get theUserId
            maybeRoom <- get theRoomId

            -- email the user
            liftIO $ emailNotification newRecord (fromJust maybeUser) 
                               (fromJust maybeRoom) BookingSuccess 

            -- we add this new recordId to DayRecords
            maybeDay <- getBy $ UniqueDay theDay
            case maybeDay of
                Nothing -> do
                    newId <- insert $ DayRecords [newRecordId] theDay
                    return newRecordId
                Just (Entity existId records) -> do
                    let existRecords = dayRecordsIds records
                        updateRecords = existRecords ++ [newRecordId]
                    update existId [DayRecordsIds =. updateRecords]
                    return newRecordId

cancelABooking recordId = do
    maybeRecord <- get recordId
    case maybeRecord of
        Nothing -> return ()
        Just aRecord -> do 
              update recordId [RecordCancel =. True]
              let aUserId = recordUserId aRecord
                  aRoomId = recordRoomId aRecord
              maybeUser <- get aUserId          
              maybeRoom <- get aRoomId
              liftIO $ emailNotification aRecord (fromJust maybeUser) 
                                         (fromJust maybeRoom) BookingCancel
              return ()

------------------------------------------------------------------------------------------
-- | Email Operations: sending booking/cancel emails
emailNotification :: Record -> User -> Room -> BookingStatus -> IO ()
emailNotification aRecord aUser aRoom status = do
    let adminUserName = "testConfroom" -- 163 username and passphrase
        adminPassword = "testConfroom123"
        viaHost       = "smtp.163.com"
        viaPort       = 25

    let emailText  = userEmail aUser
        nameText   = userName  aUser
        roomText   = roomNumber aRoom
        theDay     = recordDay aRecord
        timespan   = recordTimespan aRecord
        statusText = getStatusText status
        (year, month, day)   = toGregorian theDay
        (startTime, endTime) = timeSpanToTimeString timespan

    -- create the email
    let fromAddress = Address (Just $ T.pack "Conference Room Admin") 
                              (T.pack "testConfroom@163.com")
        toAddress   = [Address (Just nameText) emailText]
        ccAddress   = []
        bccAddress  = []
        subject     = T.pack (statusText !! 0 ++ ": " ++ "Conference Room " ++ 
                              T.unpack roomText ++ " [from " ++ startTime ++ " to " ++ endTime ++ 
                              ", " ++ show year ++ "-" ++ show month ++ "-" ++ show day ++ "]")
        allParts    = plainTextPart (TL.pack "This is a test")
        theMail     = simpleMail fromAddress toAddress ccAddress bccAddress subject [allParts]

    sendMailWithLogin viaHost adminUserName adminPassword theMail 
    return ()  

getStatusText status | status == BookingSuccess = ["Booking Success Notification", ""]
                     | status == BookingCancel  = ["Booking Cancel Notification", ""]
                     | otherwise = ["",""]

timeSpanToTimeString (Timespan start end) = 
                    let startHour = todHour start 
                        startMin  = todMin start
                        endHour = todHour end 
                        endMin  = todMin end
                     in (show startHour ++ ":" ++ show startMin,
                         show endHour ++ ":" ++ show endMin)

------------------------------------------------------------------------------------------
-- | Lookup functions

-- lookup by day: used when render the main page show one day's booking status
getRecordIdsByDay theDay = do
    maybeDay <- getBy $ UniqueDay theDay
    case maybeDay of
        Nothing -> do
                   liftIO $ print "invlaid day as the key, no corresponding records"
                   return Nothing
        Just (Entity theId theRecords) -> return $ Just (theId, dayRecordsIds theRecords)

-- lookup by id: User or Room
getOnePieceInfoByDBId theId = do
    maybeValue <- get theId
    case maybeValue of
        Nothing -> do
                   liftIO $ print "invlaid id as the key, no corresponding values"
                   liftIO $ print theId 
                   return Nothing
        Just (Entity _ aPiece) -> return $ Just aPiece

-- lookup a user's booking info: little complex, return a list of [(recordId, Record, User, Room)]
-- uniqueKey could be [roomId or userId]
getUserBookingInfosByUserEmail theEmail bHistory = do
    curDT <- liftIO $ getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT
        selectOperation = if bHistory then (<=.) else (>=.)

    maybeValue <- getBy $ UniqueEmail theEmail
    case maybeValue of
        Nothing -> do
                   liftIO $ print "invlaid key, no corresponding value."
                   return []
        Just (Entity theUserId _) -> do
            theDayRecordsEntityList <- selectList [DayRecordsDay `selectOperation` curDay] []
            let theRecordsKeyList = concat . map getRecordIdsFromDayRecordsEntity $ 
                                                 (theDayRecordsEntityList)
            theRecordsList <- selectList [RecordId <-. theRecordsKeyList] []
            let matchRecordsList = filter (\ (Entity _ r) -> recordUserId r == theUserId) 
                                          theRecordsList
            mapM marshalOneRecordToTypeValues matchRecordsList
    where
    getRecordIdsFromDayRecordsEntity (Entity _ aDayRecords) = dayRecordsIds aDayRecords

 
marshalOneRecordToTypeValues (Entity aRecordId aRecord) = do
    maybeUser <- get (recordUserId aRecord)
    maybeRoom <- get (recordRoomId aRecord)
    return (aRecordId, aRecord, fromJust maybeUser, fromJust maybeRoom)
 
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- other helpers, may not be used
getUserIdByUniqueUserEmail theEmail = do
    maybeUser <- getBy $ UniqueEmail theEmail
    case maybeUser of
        Nothing -> return Nothing
        Just (Entity theId auser) -> return $ Just theId
