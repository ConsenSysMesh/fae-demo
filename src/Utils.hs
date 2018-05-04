module Utils where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable
import Data.Map.Lazy (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import FaeTX.Post
import FaeTX.Types
import Prelude
import Types

encodeMsg :: Msg -> Text
encodeMsg a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

parseMsg :: Text -> Maybe Msg
parseMsg jsonTxt = decode $ C.pack $ T.unpack jsonTxt
