module Conversation where

import ClassyPrelude
import Data.Time
import Control.Error hiding (headMay)
import Control.Concurrent
import Control.Lens hiding (index)
import Data.Aeson.Lens
import Data.List.Extra (groupOn)
import System.IO (hFlush)

import System.Process (callProcess)
import Data.Acid (AcidState)
import Data.Acid.Advanced   ( query', update' )

import Types
import Epop
import Witai
import Store

class MonadIO m => MonadConv m where
  listen :: m Text
  say :: Text -> m ()

instance MonadConv IO where
  listen = putStr "You: " >> hFlush stdout >> getLine
  say t = putStr "Stiva: " >> putStrLn t

confirm :: MonadConv m => m Bool
confirm = fmap (== "yes") listen  -- TODO use wit.ai for that one


conversation :: MonadConv m => AcidState BotState -> Text -> m ()
conversation acid cid =
  do mcreds <- query' acid (GetCreds cid)
     case mcreds of
       Just (login, pass) -> convLoop login pass
       Nothing -> do _ <- listen -- TODO hack to ignore the first time they say "hi"
                     say "Hello, I'm Stiva, your friendly epop administration robot!"
                     say "To help you with epop, I'm going to need your epop login and password"
                     say "What is your epop login?"
                     login <- listen
                     say "Okay, what is your epop password?"
                     pass <- listen
                     update' acid (AddCreds cid login pass)
                     -- TODO check the creds are good
                     say "Okay, let's go! You can ask me `What are my tasks this week?` for example."
                     convLoop login pass


convLoop :: MonadConv m => Text -> Text -> m ()
convLoop login pass =
  do intent <- interpretIntent =<< listen
     case intent of
       Just i -> handleIntent login pass i
       Nothing -> say "Sorry, I didn't understand"
     convLoop login pass


formatDay = pack . formatTime defaultTimeLocale "%d %B %Y"

-- TODO handle failure
handleIntent :: MonadConv m => Text -> Text -> Intent -> m ()
handleIntent login pass (Get from to) =
  do say $ "Okay, fetching tasks between " ++ formatDay from ++ " and " ++ formatDay to
     Right tasks <- run login pass (getTasks [from..to]) -- TODO bad
     forM_ (groupOn snd tasks) $ \daysAndTasks ->
       do let (firstDay, mtask) = headEx daysAndTasks
              (lastDay, _) = lastEx daysAndTasks
              task = maybe "Nothing" tName mtask
          if firstDay == lastDay
            then say $ task ++ " on " ++ formatDay firstDay
            else say $ task ++ " from " ++ formatDay firstDay ++ " to " ++ formatDay lastDay
     -- TODO if some are not filled, propose to fill
handleIntent login pass (Set from to) =
  do say $ "Okay, let's time track between " ++ formatDay from ++ " and " ++ formatDay to
     say "Let me check what tasks I have for that period"
     Right tasks <- run login pass (getAvailableTasks from) -- TODO bad
     askAndSetTask login pass from to tasks
handleIntent login pass (SetWithTask from to task) =
  do say $ "Let me check if I can find a task that matches \"" ++ task ++ "\" between " ++ formatDay from ++ " and " ++ formatDay to
     Right tasks <- run login pass (getAvailableTasks from) -- TODO bad
     case find (\t -> toLower task `isInfixOf` toLower (tName t)) tasks of
       Just t -> do say $ "Found " ++ tName t ++ ". Do you want me to set it to that task?"
                    doIt <- confirm
                    if doIt
                       then setTask login pass from to t
                       else say "Okay, not changing anything"
       Nothing -> say "Sorry, could not find it." >> askAndSetTask login pass from to tasks

askAndSetTask :: MonadConv m => Text -> Text -> Day -> Day -> [Task] -> m ()
askAndSetTask login pass from to tasks =
  do say "Which task where your working on?"
     forM_ (zip [1..] tasks) $ \(i, t) -> say $ "`" ++ tshow i ++ "` " ++ tName t
     answer <- listen
     case (readZ . unpack) answer >>= \i -> index tasks (i - 1) of
           Just t -> setTask login pass from to t
           Nothing -> say "Sorry, I didn't understand"

-- TODO, say what actually changed (be nicely idempotent)
setTask :: MonadConv m => Text -> Text -> Day -> Day -> Task -> m ()
setTask login pass from to task =
  do say $ "Okay, setting task to " ++ tName task ++ " between " ++ tshow from ++ " and " ++ tshow to
     result <- run login pass (setTasks (zip [from..to] (repeat (Just task))))
     case result of
       Right () -> say "Done"
       Left err -> say $ "There was an error:" ++ pack err



run :: MonadIO m => Text -> Text -> Epop a -> m (Either String a)
run login pass = liftIO . runExceptT . runEpop (unpack login) (unpack pass)
