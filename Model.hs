{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.Sql
import Data.Time
import Data.Int(Int64)
import qualified Data.Text as T
import Text.Read(readMaybe)

import Handler.MiscTypes
import Handler.Utils

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:  http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
    toJSON (Entity key userInfo) = object $ 
                               [ "Email" .= (userEmail userInfo)
                               , "姓名"  .= (userName userInfo) 
                               , "权限"  .= (toLevelString $ userLevel userInfo) 
                  , "注册时间" .= (T.pack $ show $ convertUtcToZoneTime $ userFirstAdd userInfo)
                               , "sqlkey" .= (T.pack $ show $ fromSqlKey key)
                               ]

instance ToJSON (Entity Room) where
    toJSON (Entity key roomInfo) = object $ 
                               [ "会议室编号" .= (roomNumber roomInfo)
                               , "权限" .= (toLevelString $ roomLevel roomInfo)
                               , "启用" .= ((boolToHanzi $ roomAvailable roomInfo) :: T.Text)
                               , "有效期"  .= (show $ roomValidTime roomInfo)
                  , "注册时间" .= (T.pack $ show $ convertUtcToZoneTime $ roomFirstAdd roomInfo)
                               , "sqlkey" .= (T.pack $ show $ fromSqlKey key)
                               ]

------------------------------------------------------------------------------------------
---- type helper functions
toLevelString :: Level -> T.Text
toLevelString lev
    | lev == AuthNormal  = "普通"
    | lev == AuthAdvance = "领导"
    | lev == AuthAdmin   = "管理员"
    | otherwise          = "Wrong Level"

toRoomUsageString :: RoomUsage -> T.Text
toRoomUsageString use 
    | use == UsageZuHui            = "组会"
    | use == UsageXueShuBaoGao     = "学术报告"
    | use == UsageYanTaoHui        = "研讨会" 
    | use == UsageXueShengHuoDong  = "学生活动" 
    | use == UsageJiaoXueKeCheng   = "教学课程"
    | use == UsageDangHui          = "党会"
    | use == UsageOther            = "其他" 
    | otherwise                    = "无标明" 

getRoomUsageInfo :: Record -> T.Text
getRoomUsageInfo bookingInfo = 
    let usage = recordRoomUsage bookingInfo
     in case usage of
         UsageOther -> 
             case recordOtherUsage bookingInfo of
                 Nothing -> "无标明"
                 Just u  -> u
         _          -> toRoomUsageString usage
                                
authLevel :: [(T.Text, Level)]
authLevel = [("普通", AuthNormal), ("领导", AuthAdvance),("管理员", AuthAdmin)]

---- read str to key
mayStrToSqlKey :: ToBackendKey SqlBackend a => Maybe T.Text -> Maybe (Key a)
mayStrToSqlKey mayKeyStr = 
    case mayKeyStr of
        Nothing -> Nothing
        Just idStr -> 
             case ((readMaybe . T.unpack $ idStr) :: Maybe Int64) of
                 Nothing -> Nothing
                 Just intKey -> Just . toSqlKey $ intKey 

strToSqlKey :: ToBackendKey SqlBackend a => T.Text -> Maybe (Key a)
strToSqlKey idStr = 
    case ((readMaybe . T.unpack $ idStr) :: Maybe Int64) of
        Nothing -> Nothing
        Just intKey -> Just . toSqlKey $ intKey 

-- mayDay format: YYYY-MM-DD
mayStrToDay :: Maybe T.Text -> Maybe Day
mayStrToDay mayDayStr =
    case mayDayStr of
        Nothing -> Nothing
        Just dayText -> 
             let ymd = map (read . T.unpack) $ T.splitOn "-" dayText
                 in if (T.length dayText /= 10) || (length ymd /= 3)
                    then Nothing 
                    else Just $ fromGregorian (fromIntegral $ ymd!!0) (ymd!!1) (ymd!!2)
