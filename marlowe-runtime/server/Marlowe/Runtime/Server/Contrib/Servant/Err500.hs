module Marlowe.Runtime.Server.Contrib.Servant.Err500 where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Servant (ServerError (..))
import qualified Servant as Servant

err500 :: BSL.ByteString -> ServerError
err500 body = do
  let accessControlAllowOriginKey = "Access-Control-Allow-Origin"
      accessControlAllowOrigin = (accessControlAllowOriginKey, "*")
  Servant.err500
    { errHeaders = [accessControlAllowOrigin]
    , errBody = body
    }

err500JSON :: Aeson.Value -> ServerError
err500JSON body = do
  let accessControlAllowOriginKey = "Access-Control-Allow-Origin"
      accessControlAllowOrigin = (accessControlAllowOriginKey, "*")
      applicationJson = "application/json"
  Servant.err500
    { errHeaders = [accessControlAllowOrigin, ("Content-Type", applicationJson)]
    , errBody = Aeson.encode body
    }
