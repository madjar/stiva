{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Store where

import           ClassyPrelude
import           Control.Monad.Reader (asks)
import           Control.Monad.State  (get, modify, put)
import           Data.Acid            (AcidState, Query, Update, makeAcidic,
                                       openLocalState)
import           Data.Data            (Data, Typeable)
import           Data.SafeCopy        (base, deriveSafeCopy)

type UserId = Text

data BotState = BotState { creds :: Map UserId (Text, Text)
                         } deriving (Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''BotState)

initialBotState :: BotState
initialBotState = BotState mempty

addCreds :: UserId -> Text -> Text -> Update BotState ()
addCreds uid login pass = modify (BotState . insertMap uid (login, pass) . creds)

getCreds :: UserId -> Query BotState (Maybe (Text, Text))
getCreds uid = asks (lookup uid . creds)

$(makeAcidic ''BotState ['addCreds, 'getCreds])

