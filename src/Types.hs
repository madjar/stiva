{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import           ClassyPrelude

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Attoparsec.Text
import           Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import           Servant.Common.Text

data Task = Task { tProject :: Text, tName :: Text} deriving (Show, Generic, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1 . toLower} ''Task)

data Week = Week {wYear :: Integer, wWeek :: Int} deriving (Show, Generic, Eq, Ord)

-- This will be in MonadFail, someday
parseWeek :: Monad m => Text -> m Week
parseWeek = either fail return . parseOnly weekParser
  where weekParser = Week <$> decimal <* "-Wk" <*> decimal <* takeText

-- TODO move this to some utils module
firstDayOfWeek :: Week -> Day
firstDayOfWeek week = fromWeekDate (wYear week) (wWeek week) 1

weekOfDay :: Day -> Week
weekOfDay d = Week y w
  where (y, w, _) = toWeekDate d

isWorkDay :: Day -> Bool
isWorkDay d = weekDay <= 5
  where (_, _, weekDay) = toWeekDate d

instance FromJSON Week where
  parseJSON = withText "week" parseWeek

instance ToJSON Week where
  toJSON (Week year week) = String (tshow year ++ "-Wk" ++ tshow week)

instance FromText Week where
  fromText = parseWeek
