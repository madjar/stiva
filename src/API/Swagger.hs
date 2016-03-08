{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module API.Swagger where

-- import ClassyPrelude
-- import Control.Lens
-- import Servant
-- import Servant.Swagger
-- import Data.Swagger

-- import Types
-- import API.Web

-- type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- type API = EpopAPI
--       :<|> SwaggerAPI

-- instance ToSchema Task
-- instance ToSchema Week where
--   declareNamedSchema _ = pure $ NamedSchema (Just "week") (mempty & type_ .~SwaggerString)


-- instance ToParamSchema Week where
--   toParamSchema _ = mempty
--      & type_ .~ SwaggerString

-- epopSwagger :: Swagger
-- epopSwagger = toSwagger (Proxy :: Proxy EpopAPI)

-- api :: Proxy API
-- api = Proxy


-- swaggerServer :: Server API
-- swaggerServer = epopServer :<|> return epopSwagger
