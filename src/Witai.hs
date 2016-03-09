{-# LANGUAGE TemplateHaskell #-}
module Witai where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Data.Time

witToken = "FWBO35WXET2RIBCDF3IJTHXX6ENQUXG6"

data Outcome = Outcome { o_text :: Text
                       , oIntent :: Text
                       , oEntities :: Value
                       , oConfidence :: Double
                       } deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1 . toLower} ''Outcome)


witMessage :: Text -> IO [Outcome]
witMessage msg = do r <- getWith opts "https://api.wit.ai/message"
                    --putStrLn (r ^. responseBody . to decodeUtf8 . to toStrict)
                    return $ r ^. responseBody . key "outcomes" . _JSON
  where opts = defaults & param "q" .~ [msg]
                        & header "Authorization" .~ ["Bearer " ++ witToken]

-- TODO : not a maybe, but a proper Either
toDayInterval :: Outcome -> Maybe (Day, Day)
toDayInterval o =
  do dt <- oEntities o ^? key "datetime" . nth 0 -- TODO what about when there's more than one?
     case dt ^? key "type" . _String of
       Just "interval" -> (,) <$> dt ^? key "from" . key "value" . _JSON . to utctDay
                              <*> dt ^? key "to" . key "value" . _JSON . to utctDay
       Just "value" -> do from <- dt ^? key "value" . _JSON . to utctDay
                          let to = case dt ^? key "grain" . _String of
                                Just "day" -> from
                                Just "week" -> addDays 6 from
                                Just "month" -> addDays (-1) . addGregorianMonthsClip 1 $ from
                                Just "year" -> addDays (-1) . addGregorianYearsClip 1 $ from
                          return (from, to)
       Just t -> error $ "datetime intent type not handled: " ++ unpack t
       Nothing -> error $ "datetime intent has no type ?!?"
-- TODO check grain

-- TODO use context to give a list of projects to wit.ai
-- TODO use threads

traceJ :: (Monad m, ToJSON a) => a -> m ()
traceJ = traceM . unpack . decodeUtf8 . encodePretty


data Intent = Get Day Day
            | Set Day Day
            | SetWithTask Day Day Text
            deriving (Show)

-- TODO detect greetings, insults, and thanks

-- TODO : when instance is incomplete, don't fail, but ask for the rest
interpretIntent :: MonadIO m => Text -> m (Maybe Intent)
interpretIntent t =
  do outcomes <- liftIO $ witMessage t
     let mout = headMay outcomes
     return (toIntent =<< mout)
  where toIntent out = case oIntent out of
          "get_tasks" -> toGetTasks out
          "set_task" -> toSetTask out

toGetTasks :: Outcome -> Maybe Intent
toGetTasks o = do (from, to) <- toDayInterval o
                  return $ Get from to

toSetTask :: Outcome -> Maybe Intent
toSetTask o = do (from, to) <- toDayInterval o
                 let mtask = oEntities o ^? key "task" . nth 0 . key "value" . _String
                 return $ case mtask of
                   Just t -> SetWithTask from to t
                   Nothing -> Set from to
