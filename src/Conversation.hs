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
import Data.Acid

import Data.Acid.Advanced   ( query', update' )

import Types
import Epop
import Witai
import Store

conversation :: AcidState BotState -> IO ()
conversation acid =
  do mcreds <- query' acid (GetCreds "hardcodedlocal")
     case mcreds of
       Just (login, pass) -> convLoop login pass
       Nothing -> do say "Hello, I'm Stiva, your friendly epop administration robot!"
                     say "To help you with epop, I'm going to need your epop login and password"
                     say "What is your epop login?"
                     login <- listen
                     say "Okay, what is your epop password?"
                     pass <- listen
                     update' acid (AddCreds "hardcodedlocal" login pass)
                     -- TODO check the creds are good
                     say "Okay, let's go! You can ask me `What are my tasks this week?` for example."
                     convLoop login pass


convLoop :: Text -> Text -> IO ()
convLoop login pass =
  do intent <- interpretIntent =<< listen
     case intent of
       Just i -> handleIntent login pass i
       Nothing -> say "Sorry, I didn't understand"
     convLoop login pass


formatDay = pack . formatTime defaultTimeLocale "%d %B %Y"

-- TODO handle failure
handleIntent :: Text -> Text -> Intent -> IO ()
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

askAndSetTask :: Text -> Text -> Day -> Day -> [Task] -> IO ()
askAndSetTask login pass from to tasks =
  do say "Which task where your working on?"
     forM_ (zip [1..] tasks) $ \(i, t) -> say $ "`" ++ tshow i ++ "` " ++ tName t
     answer <- listen
     case (readZ . unpack) answer >>= \i -> index tasks (i - 1) of
           Just t -> setTask login pass from to t
           Nothing -> say "Sorry, I didn't understand"

-- TODO, say what actually changed (be nicely idempotent)
setTask :: Text -> Text -> Day -> Day -> Task -> IO ()
setTask login pass from to task =
  do say $ "Okay, setting task to " ++ tName task ++ " between " ++ tshow from ++ " and " ++ tshow to
     result <- run login pass (setTasks (zip [from..to] (repeat (Just task))))
     case result of
       Right () -> say "Done"
       Left err -> say $ "There was an error:" ++ pack err

listen :: IO Text
listen = putStr "You: " >> hFlush stdout >> getLine

say :: Text -> IO ()
say t = putStr "Stiva: " >> putStrLn t

confirm :: IO Bool
confirm = fmap (== "yes") listen  -- TODO use wit.ai for that one

run :: Text -> Text -> Epop a -> IO (Either String a)
run login pass = runExceptT . runEpop (unpack login) (unpack pass)
