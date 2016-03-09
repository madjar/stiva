{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Bot where

import ClassyPrelude

import Web.Slack
import Web.Slack.Message
import Control.Lens
import Network.Wreq
import Data.Acid (AcidState)
import Data.Acid.Advanced ( query', update' )
import Control.Concurrent (forkIO)
import Control.Monad.Reader (asks)

import Store
import Witai
import Conversation

data ConvChannels = ConvChannels { fromThem :: Chan Text,
                                   fromUs :: Chan Text}

-- Give Web.Slack.Types.Id (Id) the Ord instance to set it as key for this Map
type Convs = Map Text ConvChannels
type ConvReader = ReaderT ConvChannels IO
instance MonadConv ConvReader where
  listen = do chan <- asks fromThem
              readChan chan
  say t = do chan <- asks fromUs
             writeChan chan t


runSlackBot :: AcidState BotState -> IO ()
runSlackBot acid = do
  let apiToken = "xoxb-23258812385-7LWKFE8WILancZgTz7ZAcUoC" :: String
      config = SlackConfig apiToken
  runBot config (epopBot acid) mempty


epopBot :: AcidState BotState -> SlackBot Convs
--echoBot m = print m
epopBot acid (Message cid (UserComment user) msg _ _ _)
  | isDirect cid && ( (user ^. getId) `elem` ["U060EJLF3", "U062HDBJ9"] ) =  --Hardcoded for me and ludo
  do let tCid = cid ^. getId
     -- TODO add logging before allowing others to use it
     mchannels <- uses userState (lookup tCid)
     fromThem <- case mchannels of
       Just cs -> return (fromThem cs)
       Nothing -> do channels <- mkChannels cid
                     userState %= insertMap tCid channels
                     void $ liftIO $ forkIO $ runReaderT (conversation acid tCid) channels
                     return (fromThem channels)
     liftIO $ writeChan fromThem msg
epopBot _ _ = return ()

mkChannels :: ChannelId -> Slack s ConvChannels
mkChannels cid = do fromUs <- liftIO newChan
                    fromThem <- liftIO newChan
                    send <- sendMessageLater
                    liftIO $ forkIO (forwardMessage (send cid) fromUs)
                    return ConvChannels {..}

forwardMessage :: (Text -> IO ()) -> Chan Text -> IO ()
forwardMessage send chan = do msg <- readChan chan
                              send msg
                              forwardMessage send chan

isDirect :: ChannelId -> Bool
isDirect c = "D" `isPrefixOf` (c ^. getId)
