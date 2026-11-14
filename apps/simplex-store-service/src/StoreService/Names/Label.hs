{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Label
  ( LabelError (..),
    validLabel,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength)
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexTLD)

data LabelError = LELength | LEChars
  deriving (Eq, Show)

validLabel :: SimplexTLD -> Text -> Either LabelError Text
validLabel tld label
  | len < minNameLength || len > maxNameLength = Left LELength
  | T.toLower label /= label = Left LEChars
  | otherwise = case strDecode (encodeUtf8 $ label <> decodeLatin1 (strEncode tld)) of
      Right SimplexDomain {domain, subDomain = []} | domain == label -> Right label
      _ -> Left LEChars
  where
    len = T.length label
