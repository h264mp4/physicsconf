{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.DayBookingStatus where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import qualified Data.HashMap.Strict as HM
import Data.Maybe(fromJust)
import Data.Aeson(toJSON, object, (.=))

-- 15 ge 
fields :: [Text]
fields = ["房间 / 时间","8","9","10","11","12","13","14","15","16",
                     "17","18","19","20","21","22"]
contents :: [[Text]]
contents = [ ["1001", "彭兴涛<br />研讨会", "吴桃李<br />组会ing", "tao", "peng", "吴桃李，组会ing", "tao", 
                      "peng", "吴桃李，组会ing", "tao", "peng", "吴桃李，组会ing", 
                       "tao", "peng", "吴桃李，组会ing", "tao"],
             ["1002", "无", "吴桃李，组会2", "彭兴涛,研讨会", "无", 
                      "吴桃李，组会2", "t2", "无", "吴桃李，组会2", "t2", 
                      "无", "吴桃李，组会2", "t2", "无", "吴桃李，组会2", "t2"],
             ["1003", "p3", "吴桃李，组会3", "t3", "彭兴涛,研讨会", "吴桃李，组会3", 
                       "t3", "p3", "吴桃李，组会3", "t3", 
                      "p3", "吴桃李，组会3", "t3", "p3", "吴桃李，组会3", "t3"],
             ["1004", "p4", "吴桃李，组会4", "t4", "p4", "吴桃李，组会4", "t4", "p4", 
                      "吴桃李，组会4", "t4", 
                      "p4", "吴桃李，组会4", "t4", "p4", "吴桃李，组会4", "彭兴涛,研讨会"]
           ]
              

-- for now, return fake data
fakeDataName = "dataRows"
--fakeDataRows :: [Value]
fakeDataRows = map (\ x -> object $ zipWith (\ k v -> (k, toJSON v)) fields x) contents

fakeJsonRet = object $ [fakeDataName .= fakeDataRows, "total" .= toJSON (4::Int)]

getDayBookingStatusR :: Handler Value -- actually return a json.
getDayBookingStatusR = do
    --req <- waiRequest
    --req' <- getRequest
    --liftIO $ print req
    --liftIO $ print $ reqLangs req'
    valueMB <- lookupGetParam "queryDay"
    case valueMB of
        Nothing -> liftIO $ print "not passed"
        Just x  -> liftIO $ print x
    return $ fakeJsonRet

