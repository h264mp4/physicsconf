{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.DayBookingStatus where

import Import
import qualified Data.Text as T

import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils

fields :: [Text]
fields = ["房间 / 时间","7", "8","9","10","11","12","13","14","15","16",
                       "17","18","19","20","21","22", "23"]
tableDataName = "dataRows"
toDataRows = map (\ x -> object $ zipWith (\ k v -> (k, toJSON v)) fields x)
toJsonRet rows = object $ [tableDataName .= toDataRows rows, "total" .= toJSON (4::Int)]

contentsExample :: [[Text]]
contentsExample = [ 
            ["1001", "彭兴涛<br />研讨会", "吴桃李<br />组会ing", "tao", "peng", 
             "吴桃李，组会ing", "none", "peng", "吴桃李，组会ing", "tao", "peng", 
             "吴桃李，组会ing", "none", "peng", "吴桃李，组会ing", "tao"]
           ]

getDayBookingStatusR :: Handler Value -- actually return a json.
getDayBookingStatusR = do
    maybeDayStr <- lookupGetParam "queryDay"
    maybeRoomIdStr <- lookupGetParam "queryRoomId"
    let maybeRoomId = mayStrToSqlKey maybeRoomIdStr
        maybeDay    = mayStrToDay maybeDayStr

    liftIO $ print "inside getDayBookingStatusR: "    
    liftIO $ print maybeDay 
    liftIO $ print maybeRoomId 

    case maybeDay of
        Nothing -> sendResponseStatus status404 ("Not a valid day" :: Text)
        Just theDay -> do
            mayRids <- runDB $ getRecordIdsByDay theDay
            case mayRids of
                Nothing -> getAllAvailableOfTheDay [] maybeRoomId 
                Just rids -> do
                    liftIO $ print ("Looking for booking info of day " ++ show theDay)
                    getBookingInfos rids maybeRoomId

getBookingInfos :: [RecordId] -> Maybe RoomId -> Handler Value
getBookingInfos rids maybeRoomId = do
    --won't too much, for one day can at max have 16 records.
    records <- mapM (runDB . get404) rids
    case maybeRoomId of
        Just theRoomId -> do
            roomInfo <- runDB $ get404 theRoomId
            -- only return one room's booking status
            return $ toJsonRet [dayBookingStatusForOneRoom True (theRoomId, roomInfo) records]
        Nothing -> getAllAvailableOfTheDay records Nothing

dayBookingStatusForOneRoom' :: [Record] -> [Text]
dayBookingStatusForOneRoom' records =
    -- we traverse from 7 to 23
    go bookingStartPeriod records
    where
    go [] _  = []
    go (x:xs) records = 
        let mayRecord = getRecordByHour x records
         in case mayRecord of
                Nothing -> "none" : go xs records
                Just aRecord -> (getRoomUsageInfo aRecord) : go xs records

dayBookingStatusForOneRoom :: Bool -> (RoomId, Room) -> [Record] -> [Text]
dayBookingStatusForOneRoom bNeedFilter (theRoomId, roomInfo) records =
    -- kickout already Cancel Records 
    let finalRecords = bNeedFilter ? 
                          ((filter (\r -> recordRoomId r == theRoomId && 
                                         recordCancel r == False) records), records)        
    in [roomNumber roomInfo] ++ (dayBookingStatusForOneRoom' finalRecords)

-- no records in that day, return all room's or jsut one room.
getAllAvailableOfTheDay :: [Record] -> Maybe (RoomId) -> Handler Value
getAllAvailableOfTheDay records maybeRoomId = do
    case maybeRoomId of
        Just theRoomId -> do
            roomInfo <- runDB $ get404 theRoomId
            return . toJsonRet $ [dayBookingStatusForOneRoom True (theRoomId, roomInfo) records]
        -- list all rooms in that day.
        Nothing -> do
            entityRooms <- runDB $ listRoomProfile
            let rooms = map (\(Entity theRoomId roomInfo) -> (theRoomId, roomInfo)) entityRooms
            return . toJsonRet $ map (flip (dayBookingStatusForOneRoom True) records) rooms

getRecordByHour :: Int -> [Record] -> Maybe Record
getRecordByHour _        []     = Nothing
getRecordByHour timeHour (x:xs) = 
    let start = todHour . recordStartTime $ x
        end   = todHour . recordEndTime $ x
     in if timeHour `elem` [start..(end-1)]
           then Just x
           else getRecordByHour timeHour xs

-- not used right now
getHoursInUsing :: [Record] -> [Int]
getHoursInUsing []     = []
getHoursInUsing (x:xs) =
    let start = todHour . recordStartTime $ x
        end   = todHour . recordEndTime $ x
     in [start .. (end - 1)] ++ getHoursInUsing xs

