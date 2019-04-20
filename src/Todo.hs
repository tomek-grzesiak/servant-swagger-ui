{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Todo where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.Swagger hiding (put, get, post)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (..), fromGregorian)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

-- | The API of a Todo service.
type TodoAPI
    = "todo" :> Get '[JSON] [Todo]
 :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoId
 :<|> "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
 :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] TodoId

-- | Combined API of a Todo service with Swagger documentation.
--type API = SwaggerAPI :<|> TodoAPI
type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> TodoAPI

-- | A single Todo entry.
data Todo = Todo
  { created :: UTCTime  -- ^ Creation datetime.
  , summary :: Text     -- ^ Task summary.
  } deriving (Show, Generic, Typeable)

-- | A unique Todo entry ID.
newtype TodoId = TodoId Int
  deriving (Show, Generic, Typeable, ToJSON, FromHttpApiData)

instance ToJSON Todo
instance FromJSON Todo

instance ToSchema Todo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Todo right here"
    & mapped.schema.example ?~ toJSON (Todo (UTCTime (fromGregorian 2015 12 31) 0) "get milk")

instance ToParamSchema TodoId
instance ToSchema TodoId

-- | Swagger spec for Todo API.
todoSwagger :: Swagger
todoSwagger = toSwagger todoAPI
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- | Combined server of a Todo service with Swagger documentation.
server :: Server API
server = swaggerSchemaUIServer todoSwagger 
  :<|> getAll 
  :<|> post 
  :<|> get
  :<|> put

api = Proxy::Proxy API

getAll :: Handler [Todo]
getAll = return [Todo (UTCTime (fromGregorian 2015 12 31) 0) "get milk"]

post :: Todo -> Handler TodoId
post x = return $ TodoId 1  

get :: TodoId -> Handler Todo
get x = return (Todo (UTCTime (fromGregorian 2015 12 31) 0) "get milk")

put :: TodoId -> Todo -> Handler TodoId
put x y = return $ TodoId 1