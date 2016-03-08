module Bot where

import ClassyPrelude

import Web.Slack
import Web.Slack.Message
import Control.Lens
import Network.Wreq

import Witai

isDirect :: ChannelId -> Bool
isDirect c = "D" `isPrefixOf` (c ^. getId)

echoBot :: SlackBot ()
--echoBot m = print m
echoBot (Message cid (UserComment user) msg _ _ _)
  | isDirect cid && ( user ^. getId == "U060EJLF3" ) =  --Hardcoded for me
  do r <- liftIO $ witMessage msg
     sendMessage cid ("```" ++ tshow r ++ "```")
echoBot _ = return ()

runSlackBot :: IO ()
runSlackBot = do
  let apiToken = "xoxb-23258812385-7LWKFE8WILancZgTz7ZAcUoC" :: String
      config = SlackConfig apiToken
  runBot config echoBot ()
