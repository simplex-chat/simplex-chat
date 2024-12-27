module Simplex.Chat.Types.Util where

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Simplex.Messaging.Encoding.String

textParseJSON :: TextEncoding a => String -> J.Value -> JT.Parser a
textParseJSON name = J.withText name $ maybe (fail $ "bad " <> name) pure . textDecode
