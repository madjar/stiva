module Main where

import           ClassyPrelude
-- import Network.Wai.Middleware.Cors
-- import Servant
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Network.Wai.Middleware.RequestLogger
import           Control.Error
import           Data.Acid       (AcidState, Query, Update, closeAcidState,
                                  makeAcidic, openLocalState)
import           Data.Acid.Local (createCheckpointAndClose, localCopy)

import           API.Swagger
import           API.Web
import           Bot
import           Conversation
import           Epop
import           Store
import           Types
import Data.Acid.Abstract (downcast)
import Text.Show.Pretty
import System.Environment (getEnv)

-- app :: Application
-- app = logStdoutDev . simpleCors $ serve api swaggerServer

-- startApp :: IO ()
-- startApp = run 8080 app


main :: IO ()
main = do
  getEnv "WIT_TOKEN" -- Fail fast when we don't have a token
  slackToken <- getEnv "SLACK_TOKEN"
  bracket (openLocalState initialBotState)
          createCheckpointAndClose
          --(\acid -> conversation acid "hardcodedlocal")
          (runSlackBot slackToken)

dumpState :: IO ()
dumpState = bracket (openLocalState initialBotState) closeAcidState dumpIt
  where dumpIt :: AcidState BotState -> IO ()
        dumpIt s = do st <- readIORef . localCopy . downcast $ s
                      putStrLn $ pack $ ppShow st
