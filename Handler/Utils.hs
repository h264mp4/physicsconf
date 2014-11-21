{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes, BangPatterns #-}
module Handler.Utils where

import Yesod.Form.Bootstrap3 

import Prelude
import Data.Text
import Data.Time
import Data.Maybe
import Data.Int(Int64)
import Data.String(IsString)
import System.IO.Unsafe(unsafePerformIO)
import Database.Persist.Sql(toSqlKey)

myTimeZone :: TimeZone
myTimeZone = unsafePerformIO $ getCurrentTimeZone

getCurDayAndTime :: IO LocalTime
getCurDayAndTime = do
    timeZ <- getCurrentTimeZone
    utcT  <- getCurrentTime
    return $ utcToLocalTime timeZ utcT

convertUtcToZoneTime :: UTCTime -> ZonedTime
convertUtcToZoneTime = utcToZonedTime myTimeZone

(?) :: Bool -> (a, a) -> a
True  ? (x, _) = x
False ? (_, y) = y
infixl 0 ?

boolToHanzi :: forall a. IsString a => Bool -> a
boolToHanzi b | b == True = "是"
              | otherwise = "否"

emptyString :: String
emptyString = []

emptyText :: Text
emptyText = ""

bookingStartPeriod = [7..23]
bookingEndPeriod   = [8..24]

commonSimpleFormLayout = BootstrapHorizontalForm
                         {
                              bflLabelOffset = ColMd 0
                             ,bflLabelSize   = ColMd 4
                             ,bflInputOffset = ColMd 0
                             ,bflInputSize   = ColMd 4
                         }