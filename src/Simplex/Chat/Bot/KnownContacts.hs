{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Bot.KnownContacts where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Options.Applicative
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (safeDecodeUtf8)

data KnownContact = KnownContact
  { contactId :: Int64,
    localDisplayName :: Text
  }
  deriving (Eq)

knownContactNames :: [KnownContact] -> String
knownContactNames = T.unpack . T.intercalate ", " . map (("@" <>) . localDisplayName)

parseKnownContacts :: ReadM [KnownContact]
parseKnownContacts = eitherReader $ parseAll knownContactsP . encodeUtf8 . T.pack

knownContactsP :: A.Parser [KnownContact]
knownContactsP = contactP `A.sepBy1` A.char ','
  where
    contactP = do
      contactId <- A.decimal <* A.char ':'
      localDisplayName <- safeDecodeUtf8 <$> A.takeTill (A.inClass ", ")
      pure KnownContact {contactId, localDisplayName}
