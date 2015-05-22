{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Handler.DBOperation where

import Network.Mail.SMTP
import Data.List(sort)
import Data.Maybe(fromJust)
import Database.Persist.Class
import Database.Persist.Types
import Control.Concurrent(forkIO)
import Database.Persist.Sql
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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
addNewUser newUser = do
    --runMigration migrateAll
    liftIO $ print newUser
    userExist <- selectList [UserEmail ==. (userEmail newUser)] [LimitTo 1]
    if not (null userExist)
       then do let errMsg = "User" ++ (show $ userEmail newUser) ++
                              "Already Exists, should use others"
               liftIO $ print errMsg
               return Nothing
       else do
            newUserId <- insert newUser
            liftIO $ forkIO $ emailUserInfo newUser "Your Confrence Room Booking Account Info In School of Physics "
            return $ Just newUserId

updateUserProfile theUserId newInfo = do
    -- only name, password and level can be editted.
    update theUserId [ UserName     =. (userName newInfo), 
                       UserPassword =. (userPassword newInfo),
                       UserLevel    =. (userLevel newInfo)
                     ]
    liftIO $ forkIO $ emailUserInfo newInfo "Confrence Room Booking Account Changing Notification"
    return ()

listUserProfile = do
    users <- selectList [] [Asc UserEmail]
    return (users :: [Entity User])   

deleteUser = delete

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

listRoomProfile bAvailable bValid = do
    rooms <- selectList [] [Asc RoomId]
    curDT <- liftIO $ getCurDayAndTime
    let curDay = localDay curDT
        availableRooms = if bAvailable
                            then filter (\ (Entity _ r) -> roomAvailable r == True) rooms
                            else rooms
    case bValid of
        True  ->  return $ filter (\(Entity _ r) -> roomValidTime r >= curDay) availableRooms
        False ->  return (availableRooms :: [Entity Room])

deleteRoom = delete -- theRoomId

------------------------------------------------------------------------------------------
-- | Booking management: bookingRoom,  deleteABooking

-- | TODO: we should check timespan overlapping
bookingRoom newRecord@(Record theUserId theDay theRoomId 
                              startTime endTime curTime _ _ _) = do
    -- check whether the record exist
    bInUsing <- isHourInUsing newRecord
    case bInUsing of 
        True -> do
            liftIO $ print $ "It has already been booked"
            return Nothing
        False -> do
            --curTime <- liftIO $ getCurrentTime
            newRecordId <- insert newRecord
            maybeUser <- get theUserId
            maybeRoom <- get theRoomId

            -- email the user
            liftIO $ forkIO $ emailNotification newRecord (fromJust maybeUser) 
                               (fromJust maybeRoom) BookingSuccess 

            -- we add this new recordId to DayRecords
            maybeDay <- getBy $ UniqueDay theDay
            case maybeDay of
                Nothing -> do
                    newId <- insert $ DayRecords [newRecordId] theDay
                    return $ Just newRecordId
                Just (Entity existId records) -> do
                    let existRecords = dayRecordsIds records
                        updateRecords = existRecords ++ [newRecordId]
                    update existId [DayRecordsIds =. updateRecords]
                    return $ Just newRecordId

isHourInUsing newRecord = do
    records <- getOneRoomRecordsOfDay (recordDay newRecord) (recordRoomId newRecord)
    let hourInUsing = sort . getHoursInUsing $ records
    return $ (todHour . recordStartTime $ newRecord) `elem` hourInUsing
    where
    getHoursInUsing :: [Record] -> [Int]
    getHoursInUsing []     = []
    getHoursInUsing (x:xs) =
        let start = todHour . recordStartTime $ x
            end   = todHour . recordEndTime $ x
         in [start .. (end - 1)] ++ getHoursInUsing xs

getOneRoomRecordsOfDay theDay theRoomId = do
    mayRids <- getRecordIdsByDay theDay
    case mayRids of
        Nothing -> return []
        Just rids -> do
            records <- mapM get404 rids
            return $ filter (\ r -> recordRoomId r == theRoomId) records

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
              liftIO $ forkIO $ emailNotification aRecord (fromJust maybeUser) 
                                         (fromJust maybeRoom) BookingCancel
              return ()

------------------------------------------------------------------------------------------
-- | Email Operations: sending booking/cancel emails
emailNotification :: Record -> User -> Room -> BookingStatus -> IO ()
emailNotification aRecord aUser aRoom status = do
    let adminUserName = "pekingphyconfadmin" -- 163 username and passphrase
        adminPassword = "asdfgh"
        viaHost       = "smtp.163.com"
        viaPort       = 25

    let emailText  = userEmail aUser
        nameText   = userName  aUser
        roomText   = roomNumber aRoom
        theDay     = recordDay aRecord
        startTime  = recordStartTime aRecord
        endTime    = recordEndTime aRecord
        statusText = getStatusText status
        (year, month, day)   = toGregorian theDay

    -- create the email
    let fromAddress = Address (Just $ T.pack "Conference Room Admin") 
                              (T.pack "pekingphyconfadmin@163.com")
        toAddress   = [ Address (Just nameText) emailText ]
        ccAddress   = []
        bccAddress  = []
        subject     = T.pack (statusText !! 0 ++ ": " ++ "Conference Room " ++ 
                              T.unpack roomText ++ " [from " ++ (show startTime) ++ " to " 
                              ++ (show endTime) ++ 
                              ", " ++ show year ++ "-" ++ show month ++ "-" ++ show day ++ "]" 
                              )
        allParts    = plainTextPart (("Don't Reply This Email.\nRoom Usage: ") <> 
                                      (TL.fromStrict $ getRoomUsageInfo aRecord))
        theMail     = simpleMail fromAddress toAddress ccAddress bccAddress subject [allParts]

    sendMailWithLogin viaHost adminUserName adminPassword theMail 
    return ()  

getStatusText status | status == BookingSuccess = ["Booking Success Notification", ""]
                     | status == BookingCancel  = ["Booking Cancel Notification", ""]
                     | otherwise = ["", ""]

timeSpanToTimeString (Timespan start end) = 
                    let startHour = todHour start 
                        startMin  = todMin start
                        endHour = todHour end 
                        endMin  = todMin end
                     in (show startHour ++ ":" ++ show startMin,
                         show endHour ++ ":" ++ show endMin)

emailUserInfo :: User -> Text -> IO ()
emailUserInfo aUser title = do
    let adminUserName = "pekingphyconfadmin" -- 163 username and passphrase
        adminPassword = "asdfgh"
        viaHost       = "smtp.163.com"
        viaPort       = 25

    let emailText  = userEmail aUser
        nameText   = userName  aUser
        passwordText = userPassword aUser

    -- create the email
    let fromAddress = Address (Just $ T.pack "Conference Room Admin") 
                              (T.pack "pekingphyconfadmin@163.com")
        toAddress   = [ Address (Just nameText) emailText ]
        ccAddress   = []
        bccAddress  = []
        subject     = title
        allParts    = plainTextPart (("Don't Reply This Email.\nYour User Name: ") <> 
                                      (TL.fromStrict emailText) <> ("\nYour User Password: ") <>
                                      (TL.fromStrict passwordText) <> ("\nYour Name: ") <>
                                      (TL.fromStrict nameText))
        theMail     = simpleMail fromAddress toAddress ccAddress bccAddress subject [allParts]

    sendMailWithLogin viaHost adminUserName adminPassword theMail 
    return ()  


------------------------------------------------------------------------------------------
-- | Lookup functions

-- lookup by day: used when render the main page show one day's booking status
getRecordIdsByDay theDay = do
    maybeDay <- getBy $ UniqueDay theDay
    case maybeDay of
        Nothing -> do
                   liftIO $ print "invlaid day as the key, no corresponding records"
                   return Nothing
        Just (Entity _ theRecords) -> return . Just $ dayRecordsIds theRecords

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
getUserBookingInfosByUserId theUserId bHistory = do
    getOneUserBookingInfos theUserId bHistory    

getUserBookingInfosByUserEmail theEmail bHistory = do
    maybeUser <- getBy $ UniqueEmail theEmail
    case maybeUser of
        Nothing -> do
                   liftIO $ print "invlaid key, no corresponding value."
                   return []
        Just (Entity theUserId _) -> getOneUserBookingInfos theUserId bHistory    

getOneUserBookingInfos theUserId bHistory = do
    curDT <- liftIO $ getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT
        selectOperation = bHistory ? ( (<.),  (>=.) )

    theDayRecordsEntityList <- selectList [DayRecordsDay `selectOperation` curDay] []
    let theRecordsKeyList = concat . map getRecordIdsFromDayRecordsEntity $ 
                                         (theDayRecordsEntityList)
    theRecordsList <- selectList [RecordId <-. theRecordsKeyList] []
    let matchRecordsList = filter (\ (Entity _ r) -> recordUserId r == theUserId) 
                                                                       theRecordsList
    mapM marshalOneRecordToRep matchRecordsList
    where
    getRecordIdsFromDayRecordsEntity (Entity _ aDayRecords) = dayRecordsIds aDayRecords

marshalOneRecordToRep (Entity aRid aRecord) = do
    maybeUser <- get (recordUserId aRecord)
    maybeRoom <- get (recordRoomId aRecord)
    return $ RepRecord {
                  userNameRep = userName . fromJust $ maybeUser
                , roomName    = roomNumber . fromJust $ maybeRoom
                , bookingDay  = T.pack . show . recordDay $ aRecord
                , occupyTime  = (T.pack . show . todHour . recordStartTime $ aRecord) <> " - "  <>
                                (T.pack . show . todHour . recordEndTime $ aRecord)
                , bookingUsage    = getRoomUsageInfo aRecord
                , bookingStatus   = getStatus . recordCancel $ aRecord
                , bookingRecordId = T.pack . show . fromSqlKey $ aRid
             }
                       
     where 
     getStatus True  = "已取消"
     getStatus False = ""

-- for user booking status representation in: getUserBookingManageR
data RepRecord = RepRecord {
      userNameRep     :: Text
     ,roomName        :: Text
     ,bookingDay      :: Text
     ,occupyTime      :: Text -- start to end
     ,bookingUsage    :: Text
     ,bookingStatus   :: Text -- whether canceld
     ,bookingRecordId :: Text -- T.pack . show . fromSqlKey $ recordId 
     } deriving (Show)
