{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes, BangPatterns #-}
module Handler.Utils where

import Prelude
import Data.Time
import Data.String(IsString)
import System.IO.Unsafe(unsafePerformIO)
import Data.Text

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