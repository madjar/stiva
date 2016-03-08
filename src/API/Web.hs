{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module API.Web where

-- import ClassyPrelude
-- import Servant
-- import Control.Monad.Trans.Either
-- import Control.Error

-- import Types
-- import Epop

-- type EpopAPI = "weeks" :> Get '[JSON] [Week]
--           :<|> "timesheet" :> Capture "week" Week :> Get '[JSON] ([Task], [(Day, Maybe Task)])


-- liftEpop :: ExceptT String IO a -> EitherT ServantErr IO a
-- liftEpop = EitherT . runExceptT . fmapLT makeError
--    where makeError msg = err500 { errReasonPhrase = msg}

-- login = error "Hardcoded not login"
-- password = error "Hardcoded not password"

-- weeksR :: ExceptT String IO [Week]
-- weeksR = runEpop login password listTimeSheets

-- timeSheetR :: Week -> ExceptT String IO ([Task], [(Day, Maybe Task)])
-- timeSheetR week = error "Yep, this module is derelict"

-- epopServer :: Server EpopAPI
-- epopServer = liftEpop weeksR :<|> liftEpop . timeSheetR

-- epopApi :: Proxy EpopAPI
-- epopApi = Proxy


