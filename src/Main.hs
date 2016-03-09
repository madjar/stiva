module Main where

import ClassyPrelude
-- import Network.Wai.Middleware.Cors
-- import Servant
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Network.Wai.Middleware.RequestLogger
import Control.Error
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Local      ( createCheckpointAndClose )

import Epop
import Types
import API.Swagger
import API.Web
import Bot
import Conversation
import Store

-- app :: Application
-- app = logStdoutDev . simpleCors $ serve api swaggerServer

-- startApp :: IO ()
-- startApp = run 8080 app



main :: IO ()
main = do
  bracket (openLocalState initialBotState)
          createCheckpointAndClose
          --(\acid -> conversation acid "hardcodedlocal")
          runSlackBot
