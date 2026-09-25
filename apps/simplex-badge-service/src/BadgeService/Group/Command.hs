{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.Group.Command
  ( GroupCmd (..),
    CmdAction (..),
    groupCmdAction,
    groupCommands,
    codeP,
    badgeTypeP,
    textTokenP,
    maxMonths,
    maxUses,
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
import Simplex.Chat.Badges.Code (BadgeCode, parseBadgeCode)
import Simplex.Chat.Types.Preferences (ChatBotCommand (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Util (safeDecodeUtf8)

maxBulk, maxUses, maxMonths :: Int
maxBulk = 100
maxUses = 1000
maxMonths = 255

data GroupCmd
  = GCIssue BadgeType Int Int
  | GCBulk BadgeType Int Int
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
    usage tag = "use: /" <> cmdName tag <> " " <> cmdParams tag

cmdArgsP :: CmdTag -> A.Parser GroupCmd
cmdArgsP = \case
  CTIssue -> GCIssue <$> badgeTypeP <*> monthsOpt <*> keyOpt "uses" maxUses
  CTBulk -> GCBulk <$> badgeTypeP <*> monthsOpt <*> (A.space *> keyValue "count" maxBulk)
  CTRevoke -> GCRevoke <$> codeP
  where
    monthsOpt = keyOpt "months" maxMonths
    keyOpt kw hi = fromMaybe 1 <$> optional (A.space *> keyValue kw hi)
    keyValue kw hi = A.string kw *> A.space *> boundedInt kw hi

groupCommands :: [ChatBotCommand]
groupCommands = map command [minBound .. maxBound]
  where
    command tag = CBCCommand (cmdName tag) (cmdLabel tag) (Just (cmdParams tag))

codeP :: A.Parser BadgeCode
codeP = A.takeWhile1 (not . isSpace) >>= maybe (fail "not a badge code") pure . parseBadgeCode . safeDecodeUtf8

-- attoparsec's decimal wraps silently at Int, so the bound is checked on the wider Integer.
boundedInt :: ByteString -> Int -> A.Parser Int
boundedInt kw hi = do
  n <- A.decimal :: A.Parser Integer
  if n >= 1 && n <= fromIntegral hi
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
  CTIssue -> "Generate a badge code"
  CTBulk -> "Generate many single-use codes"
  CTRevoke -> "Revoke a code"

-- | The parameters are quoted in the usage reply and advertised to the group, so the two cannot drift apart.
cmdParams :: CmdTag -> Text
cmdParams = \case
  CTIssue -> "<type> [months <M>] [uses <N>]"
  CTBulk -> "<type> [months <M>] count <B>"
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
  CTRevoke -> Just "only admins can revoke codes, and this code is now visible to the group - ask an admin to revoke it"
