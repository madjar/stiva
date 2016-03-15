{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, FlexibleContexts, UndecidableInstances #-}
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
import Control.Monad.Logger

import Store
import Witai
import Conversation

data ConvChannels = ConvChannels { fromThem :: Chan Text,
                                   fromUs :: Chan Text}

-- Give Web.Slack.Types.Id (Id) the Ord instance to set it as key for this Map
type Convs = Map Text ConvChannels

instance (MonadIO m, MonadReader ConvChannels m, MonadLogger m) => MonadConv m where
  listen = do chan <- asks fromThem
              liftIO $ readChan chan
  say t = do chan <- asks fromUs
             liftIO $ writeChan chan t


runSlackBot :: AcidState BotState -> IO ()
runSlackBot acid = do
  let apiToken = "xoxb-23258812385-7LWKFE8WILancZgTz7ZAcUoC" :: String
      config = SlackConfig apiToken
  runBot config (epopBot acid) mempty


epopBot :: AcidState BotState -> SlackBot Convs
epopBot acid (Message cid (UserComment user) msg _ _ _)
  | isDirect cid =
  do isSelf <- uses (session . slackSelf . selfUserId)
                    (== user)
     unless isSelf $ runStderrLoggingT $ do
       $(logDebug) ("User " ++ (user ^. getId) ++ " says " ++ msg)

       let tCid = cid ^. getId
       mchannels <- uses userState (lookup tCid)
       fromThem <- case mchannels of
         Just cs -> return (fromThem cs)
         Nothing -> do $(logInfo) ("First contact with " ++ (user ^. getId) ++ " this session. Creating conversation thread.")
                       channels <- lift $ mkChannels cid
                       userState %= insertMap tCid channels
                       void . liftIO . forkIO $ runStderrLoggingT $ runReaderT (conversation acid tCid) channels
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
                              send msg  -- TODO Be a bit slower, and add "typing"
                              forwardMessage send chan

isDirect :: ChannelId -> Bool
isDirect c = "D" `isPrefixOf` (c ^. getId)
