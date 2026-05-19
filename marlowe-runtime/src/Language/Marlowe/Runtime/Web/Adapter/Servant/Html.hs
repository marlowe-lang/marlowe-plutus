module Language.Marlowe.Runtime.Web.Adapter.Servant.Html where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept, MimeRender (mimeRender), contentType)

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

newtype Html = Html {getHtml :: ByteString}

instance MimeRender HTML Html where
  mimeRender _ = getHtml

