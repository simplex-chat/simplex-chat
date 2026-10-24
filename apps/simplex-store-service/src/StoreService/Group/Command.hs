{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Group.Command
  ( GroupCmd (..),
    CmdAction (..),
    groupCmdAction,
    groupCommands,
    codeP,
    badgeTypeP,
    nameCodeP,
    textTokenP,
    maxMonths,
    maxUses,
    maxNameYears,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad (void)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Code (BadgeCode, maxNameLength, minNameLength, parseBadgeCode)
import Simplex.Chat.Types.Preferences (ChatBotCommand (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Config (minNameYears)

maxBulk, maxUses, maxMonths, maxNameYears :: Int
maxBulk = 100
maxUses = 1000
maxMonths = 255
maxNameYears = 10

data GroupCmd
  = GCIssue BadgeType Int Int
  | GCBulk BadgeType Int Int
  | GCIssueName Int Int
  | GCBulkName Int Int Int
  | GCRevoke BadgeCode
  deriving (Eq, Show)

data CmdAction
  = RunCmd GroupCmd
  | ReplyText Text
  | IgnoreMsg
  deriving (Eq, Show)

groupCmdAction :: GroupMemberRole -> Text -> CmdAction
groupCmdAction role t = case A.parseOnly cmdActionP (encodeUtf8 (T.strip t)) of
  Right (tag, r)
    | role >= cmdMinRole tag -> either ReplyText RunCmd r
    | Right _ <- r, Just refusal <- cmdRefusal tag -> ReplyText refusal
  _ -> IgnoreMsg

-- | Left is the usage reply for an advertised command whose arguments do not parse.
cmdActionP :: A.Parser (CmdTag, Either Text GroupCmd)
cmdActionP = A.choice (map cmdP [minBound .. maxBound])
  where
    cmdP tag = (tag,) <$> (A.string ("/" <> encodeUtf8 (cmdName tag)) *> (Right <$> fullArgsP tag <|> Left (usage tag) <$ usageEndP))
    fullArgsP tag = A.char ' ' *> cmdArgsP tag <* A.endOfInput
    -- Without the space check "/issued" would get a usage reply.
    usageEndP = void A.space <|> A.endOfInput
    usage tag = "Usage: /" <> cmdName tag <> " " <> cmdParams tag

cmdArgsP :: CmdTag -> A.Parser GroupCmd
cmdArgsP = \case
  CTIssue -> uncurry GCIssueName <$> nameCodeP <|> GCIssue <$> badgeTypeP <*> monthsOpt <*> keyOpt "uses" 1 maxUses
  CTBulk -> uncurry GCBulkName <$> nameCodeP <*> countP <|> GCBulk <$> badgeTypeP <*> monthsOpt <*> countP
  CTRevoke -> GCRevoke <$> codeP
  where
    monthsOpt = keyOpt "months" 1 maxMonths
    countP = A.space *> keyValue "count" 1 maxBulk

-- | The default is the lowest value.
keyOpt :: ByteString -> Int -> Int -> A.Parser Int
keyOpt kw lo hi = fromMaybe lo <$> optional (A.space *> keyValue kw lo hi)

keyValue :: ByteString -> Int -> Int -> A.Parser Int
keyValue kw lo hi = A.string kw *> A.space *> boundedInt kw lo hi

-- | "name <length> [years <Y>]", the length and years of a prepaid name code.
nameCodeP :: A.Parser (Int, Int)
nameCodeP = "name " *> ((,) <$> nameLengthP <*> keyOpt "years" minNameYears maxNameYears)

-- | A code's text carries the length as typed, so a leading zero is refused.
nameLengthP :: A.Parser Int
nameLengthP =
  A.peekChar' >>= \case
    '0' -> fail "leading zero in the name length"
    _ -> boundedInt "length" minNameLength maxNameLength

groupCommands :: [ChatBotCommand]
groupCommands = map command [minBound .. maxBound]
  where
    command tag = CBCCommand (cmdName tag) (cmdLabel tag) (Just (cmdParams tag))

codeP :: A.Parser BadgeCode
codeP = A.takeWhile1 (not . isSpace) >>= maybe (fail "not a badge code") pure . parseBadgeCode . safeDecodeUtf8

-- attoparsec's decimal wraps silently at Int, so the bound is checked on the wider Integer.
boundedInt :: ByteString -> Int -> Int -> A.Parser Int
boundedInt kw lo hi = do
  n <- A.decimal :: A.Parser Integer
  if n >= fromIntegral lo && n <= fromIntegral hi
    then pure (fromInteger n)
    else fail (B.unpack kw <> " out of range")

-- BadgeType decodes anything to BTUnknown, so a typo would issue an unusable code.
badgeTypeP :: A.Parser BadgeType
badgeTypeP =
  textTokenP >>= \case
    BTUnknown t -> fail $ "unknown badge type " <> T.unpack t
    bt -> pure bt

textTokenP :: TextEncoding a => A.Parser a
textTokenP = do
  t <- A.takeWhile1 (not . isSpace)
  maybe (fail "invalid value") pure $ textDecode $ safeDecodeUtf8 t

-- The advertised menu lists the commands in this order.
data CmdTag = CTIssue | CTBulk | CTRevoke
  deriving (Bounded, Enum)

cmdName :: CmdTag -> Text
cmdName = \case
  CTIssue -> "issue"
  CTBulk -> "bulk"
  CTRevoke -> "revoke"

cmdLabel :: CmdTag -> Text
cmdLabel = \case
  CTIssue -> "Generate a badge or name code"
  CTBulk -> "Generate many single-use codes"
  CTRevoke -> "Revoke a code"

-- | The parameters are quoted in the usage reply and advertised to the group, so the two cannot drift apart.
cmdParams :: CmdTag -> Text
cmdParams = \case
  CTIssue -> "<type> [months <M>] [uses <N>], or name <length> [years <Y>]"
  CTBulk -> "<type> [months <M>] count <B>, or name <length> [years <Y>] count <B>"
  CTRevoke -> "<code>"

cmdMinRole :: CmdTag -> GroupMemberRole
cmdMinRole = \case
  CTIssue -> GRModerator
  CTBulk -> GRModerator
  CTRevoke -> GRAdmin

-- | This is the reply to a well-formed command from a sender who may not run it; Nothing means silence.
cmdRefusal :: CmdTag -> Maybe Text
cmdRefusal = \case
  CTIssue -> Nothing
  CTBulk -> Nothing
  -- The sender has just published a code that stays redeemable, so they must learn it was not revoked.
  CTRevoke -> Just "Only admins can revoke codes. This code is now visible to all members."
