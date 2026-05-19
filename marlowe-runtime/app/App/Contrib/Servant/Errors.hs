module App.Contrib.Servant.Err500 where

import Data.Aeson qualified as A
import Servant (ServerError (..), err500)

err500 :: ByteString -> ServerError
err500 body = do
  let accessControlAllowOriginKey = "Access-Control-Allow-Origin"
      accessControlAllowOrigin = (accessControlAllowOriginKey, "*")
  err500
    { errHeaders = [accessControlAllowOrigin]
    , errBody = body
    }

err500JSON :: A.Value -> ServerError
err500JSON body = do
  let accessControlAllowOriginKey = "Access-Control-Allow-Origin"
      accessControlAllowOrigin = (accessControlAllowOriginKey, "*")
      applicationJson = "application/json"
  err500
    { errHeaders = [accessControlAllowOrigin, ("Content-Type", applicationJson)]
    , errBody = A.encode body
    }
