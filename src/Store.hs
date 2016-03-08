{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Store where

import ClassyPrelude

-- import Control.Applicative  ( (<$>) )
-- import Control.Exception    ( bracket )
-- import Control.Monad        ( msum )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
-- import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Control.Monad.State  ( get, put, modify )
import Control.Monad.Reader (asks)

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

