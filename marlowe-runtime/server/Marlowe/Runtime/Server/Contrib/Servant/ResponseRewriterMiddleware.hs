module Marlowe.Runtime.Server.Contrib.Servant.ResponseRewriterMiddleware where

import Data.List qualified as L
import Data.Maybe (isNothing)
import Network.HTTP.Simple (Header)
import Network.HTTP.Types (Status (..), hContentType)
import Network.Wai (
  Middleware,
  Response,
  responseHeaders,
  responseLBS,
  responseStatus,
 )

-- We return `Maybe` to indicate that the rewriter did not match
-- This can be *probably* useful during chaining of rewriters.
type ResponseRewriter =
  Status -> Maybe Header -> Response -> IO (Maybe Response)

defaultErrorRewriter :: ResponseRewriter
defaultErrorRewriter status contentType _ = do
  let -- If the status is not an error, we return Nothing to indicate no change.
      isErrorStatus = case status of
        Status code _ -> code >= 400 && code < 600
  if isErrorStatus && isNothing contentType
    then do
      let body =
            "{\"message\": \"Request processing error. Possibly input (body JSON or URL) parsing failed\", \"type\": \"request_processing_error\"}"
      pure $ Just $ responseLBS status [(hContentType, "application/json")] body
    else pure Nothing

mkMiddleware :: ResponseRewriter -> Middleware
mkMiddleware rewrite baseApp req respond = do
  let getContentTypeHeader :: Response -> Maybe Header
      getContentTypeHeader = L.find ((hContentType ==) . fst) . responseHeaders
  baseApp req $ \response -> do
    let status = responseStatus response
        contentType = getContentTypeHeader response
    rewrite status contentType response >>= \case
      Just response' -> respond response'
      Nothing -> respond response
