{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Library.Commands.Parsers where

import Control.Applicative ((<|>))
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Functor (($>))
import Simplex.Chat.Controller (UserPwd (..))
import Simplex.Messaging.Util (safeDecodeUtf8, (<$?>))

cmdChoice :: [Parser a] -> Parser a
cmdChoice = A.choice . map (\p -> p <* A.takeWhile (== ' ') <* A.endOfInput)

onOffP :: Parser Bool
onOffP = ("on" $> True) <|> ("off" $> False)

pwdP :: Parser UserPwd
pwdP = jsonP <|> (UserPwd . safeDecodeUtf8 <$> A.takeTill (== ' '))

jsonP :: J.FromJSON a => Parser a
jsonP = J.eitherDecodeStrict' <$?> A.takeByteString
