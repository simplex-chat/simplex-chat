{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Links
  ( LinkKind (..),
    linkMaxBytes,
    linkKindOf,
    linkHosts,
  )
where

import Control.Applicative (optional, (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.List.NonEmpty as L
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Simplex.Messaging.Agent.Protocol (ConnShortLink (..), ConnectionMode (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Protocol (ProtocolServer (..))
import Simplex.Messaging.Transport.Client (TransportHost)

data LinkKind = LKContact | LKChannel
  deriving (Eq, Show)

linkMaxBytes :: Int
linkMaxBytes = 4096

-- the page's rule for one short link, so both refuse the same links
linkKindOf :: Text -> Maybe LinkKind
linkKindOf link
  | B.length (encodeUtf8 link) > linkMaxBytes = Nothing
  | otherwise = either (const Nothing) Just $ A.parseOnly (shortLinkP <* A.endOfInput) link

shortLinkP :: Parser LinkKind
shortLinkP = do
  _ <- ("https://" *> A.takeWhile1 hostChar *> "/") <|> "simplex:/"
  kind <- (LKContact <$ A.char 'a') <|> (LKChannel <$ A.char 'c')
  _ <- A.char '#' *> A.takeWhile1 keyChar
  _ <- optional $ A.char '?' *> A.takeWhile queryChar
  pure kind
  where
    hostChar c = isAsciiLower c || isDigit c || c == '.' || c == '-'
    keyChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'
    queryChar c = keyChar c || c == '=' || c == '&' || c == '.'

-- the hosts as the chat core reads them, empty for a link it cannot read
linkHosts :: Text -> [TransportHost]
linkHosts = either (const []) hosts . strDecode . encodeUtf8
  where
    hosts :: ConnShortLink 'CMContact -> [TransportHost]
    hosts (CSLContact _ _ ProtocolServer {host} _) = L.toList host
