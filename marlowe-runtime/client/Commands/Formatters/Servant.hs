module Commands.Formatters.Servant where

import Data.Aeson qualified as A
import Data.CaseInsensitive qualified as CI
import Servant.Client.Core.Request (RequestF(requestAccept, requestHeaders, requestHttpVersion, requestMethod, requestPath, requestQueryString))
import Servant.Client (ClientError (DecodeFailure, FailureResponse, UnsupportedContentType, InvalidContentTypeHeader, ConnectionError))
import Servant.Client.Streaming (responseHeaders, responseBody, responseStatusCode, Response)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding.Error as T
import Control.Arrow ((***))

textFromBS :: BS.ByteString -> T.Text
textFromBS = T.decodeUtf8With T.lenientDecode

textFromLBS :: LBS.ByteString -> T.Text
textFromLBS = textFromBS . LBS.toStrict

responseToJSON :: Response -> A.Value
responseToJSON res = do
  let
    -- try to decode body as JSON or fallback to the raw json text
    bodyJSON :: A.Value
    bodyJSON = case A.decode (responseBody res) of
      Just v -> v
      Nothing -> A.String $ textFromLBS (responseBody res)
  A.object
    [ "statusCode" A..= show (responseStatusCode res)
    , "headers" A..= (fmap (textFromBS . CI.original *** textFromBS) . responseHeaders $ res)
    , "body" A..= bodyJSON
    ]

clientErrorToJSON :: ClientError -> A.Value
clientErrorToJSON = \case
  FailureResponse req res -> A.object
    [ "errorType" A..= ("FailureResponse" :: String)
    , "request" A..= A.object
        [ "path" A..= (show $ requestPath req)
        , "queryString" A..= (show $ requestQueryString req)
        , "body" A..= ("?" :: String)
        , "accept" A..= (show $ requestAccept req)
        , "headers" A..= (show $ requestHeaders req)
        , "httpVersion" A..= (show $ requestHttpVersion req)
        , "method" A..= (show $ requestMethod req)
        ]
    , "response" A..= responseToJSON res
    ]
  DecodeFailure err res -> A.object
    [ "errorType" A..= ("DecodeFailure" :: String)
    , "error" A..= err
    , "response" A..= responseToJSON res
    ]
  UnsupportedContentType mediaType res -> A.object
    [ "errorType" A..= ("UnsupportedContentType" :: String)
    , "mediaType" A..= show mediaType
    , "response" A..= responseToJSON res
    ]
  InvalidContentTypeHeader res -> A.object
    [ "errorType" A..= ("InvalidContentTypeHeader" :: String)
    , "response" A..= responseToJSON res
    ]
  ConnectionError err -> A.object
    [ "errorType" A..= ("ConnectionError" :: String)
    , "error" A..= show err
    ]

