{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Bot.KnownContacts where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative
import Simplex.Chat.Library.Commands (displayNameP)
import Simplex.Messaging.Parsers (parseAll)

data KnownContact = KnownContact
  { contactId :: Int64,
    localDisplayName :: Text
  }
  deriving (Eq)

data KnownGroup = KnownGroup
  { groupId :: Int64,
    localDisplayName :: Text
  }

knownContactNames :: [KnownContact] -> Text
knownContactNames = T.intercalate ", " . map (("@" <>) . (\KnownContact {localDisplayName = n} -> n))

parseKnownContacts :: ReadM [KnownContact]
parseKnownContacts = eitherReader $ parseAll knownContactsP . encodeUtf8 . T.pack

knownContactsP :: A.Parser [KnownContact]
knownContactsP = contactP `A.sepBy1` A.char ','
  where
    contactP = do
      contactId <- A.decimal <* A.char ':'
      localDisplayName <- displayNameP
      pure KnownContact {contactId, localDisplayName}

parseKnownGroup :: ReadM KnownGroup
parseKnownGroup = eitherReader $ parseAll knownGroupP . encodeUtf8 . T.pack

knownGroupP :: A.Parser KnownGroup
knownGroupP = do
  groupId <- A.decimal <* A.char ':'
  localDisplayName <- displayNameP
  pure KnownGroup {groupId, localDisplayName}
