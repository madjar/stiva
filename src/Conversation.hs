{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Conversation where

import           ClassyPrelude
import           Control.Error        hiding (headMay)
import           Control.Monad.Logger
import           Data.Acid            (AcidState)
import           Data.Acid.Advanced   (query', update')
import           Data.Aeson
import           Data.List.Extra      (groupOn)
import           System.IO            (hFlush)

import           Epop
import           Store
import           Types
import           Witai

class (MonadIO m, MonadLogger m) => MonadConv m where
  listen :: m Text
  say :: Text -> m ()


instance MonadConv (LoggingT IO) where
  listen = liftIO $ putStr "You: " >> hFlush stdout >> getLine
  say t = liftIO $ putStr "Stiva: " >> putStrLn t

instance MonadConv m => MonadConv (ExceptT e m) where
  listen = lift listen
  say = lift . say

confirm :: MonadConv m => m Bool
confirm = do outcomes <- witContextMessage (object ["state" .= ("asked_confirmation" :: Text)]) =<< listen
             case interpretIntent outcomes of
               Just Yes -> return True
               Just No -> return False
               _ -> say "Not sure I understood, I'll assume you said no" >> return False

conversation :: MonadConv m => AcidState BotState -> Text -> m ()
conversation acid cid =
  do mcreds <- query' acid (GetCreds cid)
     case mcreds of
       Just (login, pass) -> convLoop login pass
       Nothing -> do _ <- listen -- ignore the first thing they say, and introduce myself
                     say "Hello, I'm Stiva, your friendly epop administration robot!"
                     say "To help you with epop, I'm going to need your epop login and password"
                     (login, pass) <- askCreds
                     say "Okay, let's go! You can ask me `What are my tasks this week?` for example." -- TODO more examples
                     convLoop login pass
  where askCreds = do say "What is your epop login?" -- TODO suggest a login from name, remove the g1?
                      login <- listen
                      say "Okay, what is your epop password?"
                      pass <- listen
                      say "Thanks, I'll check that right away. Maybe you can delete the last message in the meantime, so your password doesn't stay there."
                      success <- runExceptT $ run login pass isLoggedIn
                      case success of
                        Right True -> do update' acid (AddCreds cid login pass)
                                         $(logInfo) $ "Successful login for " ++ login
                                         return (login, pass)
                        Right False ->  do say "I'm sorry, the credentials don't seem to work. Let's try again."
                                           $(logInfo) $ "Failed login attempt for " ++ login
                                           askCreds
                        Left e -> do say $ "I'm very sorry, an error occured while I was checking the credentials: . It says:\n```" ++ pack e ++ "```"
                                     $(logWarn) $ "error while testing creds: " ++ pack e
                                     askCreds


convLoop :: MonadConv m => Text -> Text -> m ()
convLoop login pass =
  do intent <- interpretIntent <$> (witMessage =<< listen)
     case intent of
       Just i -> runExceptT (handleIntent login pass i) >>= \case
         Right () -> return ()
         Left e -> do say $ "I really sorry, it looks like something broke when I tried to touch it. It says:\n```" ++ pack e ++ "```"
                      $(logWarn) $ "Error while handling intent:\n" ++ tshow i ++ "\n" ++ pack e
       Nothing -> say "Sorry, I didn't understand"
     convLoop login pass


formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale "%d %B %Y"

-- TODO handle failure
handleIntent :: MonadConv m => Text -> Text -> Intent -> ExceptT String m ()
handleIntent login pass (Get from to) =
  do say $ "Okay, fetching tasks between " ++ formatDay from ++ " and " ++ formatDay to
     tasks <- run login pass (getTasks [from..to])
     forM_ (groupOn snd tasks) $ \daysAndTasks ->
       do let (firstDay, mtask) = headEx daysAndTasks
              (lastDay, _) = lastEx daysAndTasks
              task = maybe "Nothing" tName mtask
          if firstDay == lastDay
            then say $ task ++ " on " ++ formatDay firstDay
            else say $ task ++ " from " ++ formatDay firstDay ++ " to " ++ formatDay lastDay ++ ". That's " ++ tshow (length daysAndTasks) ++ " days."
     -- TODO if some are not filled, propose to fill
handleIntent login pass (Set from to) =
  do say $ "Okay, let's time track between " ++ formatDay from ++ " and " ++ formatDay to
     say "Let me check what tasks I have for that period"
     tasks <- run login pass (getAvailableTasks from) -- TODO bad
     askAndSetTask login pass from to tasks
handleIntent login pass (SetWithTask from to task) =
  do say $ "Let me check if I can find a task that matches \"" ++ task ++ "\" between " ++ formatDay from ++ " and " ++ formatDay to
     tasks <- run login pass (getAvailableTasks from) -- TODO bad
     case find (\t -> toLower task `isInfixOf` toLower (tName t)) tasks of
       Just t -> do say $ "Found " ++ tName t ++ ". Do you want me to set it to that task?"
                    doIt <- confirm
                    if doIt
                       then say "Okay, doing it now" >> setTask login pass from to t
                       else say "Okay, not changing anything"
       Nothing -> say "Sorry, could not find it." >> askAndSetTask login pass from to tasks
handleIntent _ _ Greetings = say "Hello to you!"
handleIntent _ _ Compliment = say "Thanks, I try to do my best!"
handleIntent _ _ Thanks = say "It's a pleasure to help"
handleIntent _ _ Help = say "I'm here to help you with epop. You can ask me `What are my tasks this week?` or `Let's time track last week!` for example."
handleIntent _ _ i = do $(logWarn) $ "Unexpected intent: " ++ tshow i
                        say "I wasn't expecting you to say that."

askAndSetTask :: MonadConv m => Text -> Text -> Day -> Day -> [Task] -> ExceptT String m ()
askAndSetTask login pass from to tasks =
  do say "Which task where your working on?"
     forM_ (zip [1..] tasks) $ \(i, t) -> say $ "`" ++ tshow i ++ "` " ++ tName t
     answer <- listen
     case (readZ . unpack) answer >>= \i -> index tasks (i - 1) of
           Just t -> setTask login pass from to t
           Nothing -> say "Sorry, I didn't understand"

-- TODO, say what actually changed (be nicely idempotent)
setTask :: MonadConv m => Text -> Text -> Day -> Day -> Task -> ExceptT String m ()
setTask login pass from to task =
  do say $ "Okay, setting task to " ++ tName task ++ " between " ++ tshow from ++ " and " ++ tshow to
     run login pass (setTasks (zip [from..to] (repeat (Just task))))
     say "Done"

-- TODO if it fails, try again
run :: MonadIO m => Text -> Text -> Epop a -> ExceptT String m a
run login pass = mapExceptT liftIO . runEpop (unpack login) (unpack pass)
