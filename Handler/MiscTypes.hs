{-# LANGUAGE DeriveGeneric #-}
module Handler.MiscTypes where

import Prelude
import Database.Persist.TH
import Data.Time
import Data.Text(Text)
import Data.Aeson
import GHC.Generics

data Level = AuthNormal | AuthAdvance | AuthAdmin 
    deriving (Show, Read, Eq, Ord)
derivePersistField "Level"

-- start and end of a room's booking 
data Timespan = Timespan TimeOfDay TimeOfDay
    deriving (Show, Read, Eq, Ord)
derivePersistField "Timespan"

data DeleteId = DeleteId {deleteId :: Text}
    deriving (Show, Generic) 

data EditId = EditId {editId :: Text}
    deriving (Show, Generic) 

instance FromJSON DeleteId
instance FromJSON EditId