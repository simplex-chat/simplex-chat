{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
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
    cmdP tag = (tag,) <$> (A.string ("/" <> encodeUtf8 (cmdName tag)) *> (Right <$> fullArgsP tag <|> Left (cmdUsage tag) <$ usageEndP))
    fullArgsP tag = A.char ' ' *> cmdArgsP tag <* A.endOfInput
    -- Without the space check "/issued" would get a usage reply.
    usageEndP = void A.space <|> A.endOfInput

cmdArgsP :: CmdTag -> A.Parser GroupCmd
cmdArgsP = \case
  CTIssue -> uncurry GCIssueName <$> nameCodeP <|> GCIssue <$> badgeTypeP <*> optP monthsOpt <*> optP usesOpt
  CTBulk -> uncurry GCBulkName <$> nameCodeP <*> countP <|> GCBulk <$> badgeTypeP <*> optP monthsOpt <*> countP
  CTRevoke -> GCRevoke <$> codeP
  where
    countP = A.space *> optValueP countOpt

-- | An option's keyword, its placeholder in the usage reply and its bounds; an omitted option takes the lowest value.
data CmdOpt = CmdOpt {optKey :: Text, optVar :: Text, optMin :: Int, optMax :: Int}

lengthOpt, yearsOpt, monthsOpt, usesOpt, countOpt :: CmdOpt
lengthOpt = CmdOpt "length" "<length>" minNameLength maxNameLength
yearsOpt = CmdOpt "years" "<Y>" minNameYears maxNameYears
monthsOpt = CmdOpt "months" "<M>" 1 maxMonths
usesOpt = CmdOpt "uses" "<N>" 1 maxUses
countOpt = CmdOpt "count" "<N>" 1 maxBulk

optP :: CmdOpt -> A.Parser Int
optP opt = fromMaybe (optMin opt) <$> optional (A.space *> optValueP opt)

optValueP :: CmdOpt -> A.Parser Int
optValueP opt = A.string (encodeUtf8 (optKey opt)) *> A.space *> boundedInt opt

-- | "name <length> [years <Y>]", the length and years of a prepaid name code.
nameCodeP :: A.Parser (Int, Int)
nameCodeP = A.string (encodeUtf8 nameKind) *> A.char ' ' *> ((,) <$> nameLengthP <*> optP yearsOpt)

-- | A code's text carries the length as typed, so a leading zero is refused.
nameLengthP :: A.Parser Int
nameLengthP =
  A.peekChar' >>= \case
    '0' -> fail "leading zero in the name length"
    _ -> boundedInt lengthOpt

groupCommands :: [ChatBotCommand]
groupCommands = map command [minBound .. maxBound]
  where
    command tag = CBCCommand (cmdName tag) (cmdLabel tag) (Just (cmdParams tag))

codeP :: A.Parser BadgeCode
codeP = A.takeWhile1 (not . isSpace) >>= maybe (fail "not a badge code") pure . parseBadgeCode . safeDecodeUtf8

-- attoparsec's decimal wraps silently at Int, so the bound is checked on the wider Integer.
boundedInt :: CmdOpt -> A.Parser Int
boundedInt CmdOpt {optKey, optMin, optMax} = do
  n <- A.decimal :: A.Parser Integer
  if n >= fromIntegral optMin && n <= fromIntegral optMax
    then pure (fromInteger n)
    else fail (T.unpack optKey <> " out of range")

-- BadgeType decodes anything to BTUnknown, so a typo would issue an unusable code.
badgeTypeP :: A.Parser BadgeType
badgeTypeP = do
  bt <- textTokenP
  if bt `elem` issuableBadgeTypes then pure bt else fail $ "unknown badge type " <> T.unpack (textEncode bt)

-- BadgeType has no Enum instance because of BTUnknown, so the parser, the usage replies and the menu share this list.
issuableBadgeTypes :: [BadgeType]
issuableBadgeTypes = [BTSupporter, BTLegend, BTInvestor]

nameKind :: Text
nameKind = "name"

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
  CTIssue -> "Generate a code"
  CTBulk -> "Generate many codes"
  CTRevoke -> "Revoke a code"

-- The menu hints and the usage replies are built from the same kinds, options and bounds, so they cannot drift apart.
cmdParams :: CmdTag -> Text
cmdParams = \case
  CTIssue -> kindParams
  CTBulk -> kindParams <> " " <> optArg countOpt
  CTRevoke -> "<code>"
  where
    kindParams = "<" <> T.intercalate "|" (nameArg : badgeKinds) <> "> [" <> optKey yearsOpt <> "|" <> optKey monthsOpt <> " <N>]"

cmdUsage :: CmdTag -> Text
cmdUsage = \case
  CTIssue -> kindUsage CTIssue ("[" <> optArg usesOpt <> "]") [] [usesOpt] ["name 6 years 5", "supporter months 12"]
  CTBulk -> kindUsage CTBulk (optArg countOpt) [countOpt] [countOpt] ["name 6 count 100", "legend months 3 count 10"]
  CTRevoke -> "Usage: " <> cmdText CTRevoke (cmdParams CTRevoke)

-- | nameOpts and badgeOpts are the command's own options that apply to each kind.
kindUsage :: CmdTag -> Text -> [CmdOpt] -> [CmdOpt] -> [Text] -> Text
kindUsage tag cmdOptArg nameOpts badgeOpts examples =
  T.intercalate
    "\n"
    [ "Usage: " <> cmdText tag ("<kind> [" <> optVar lengthOpt <> "] [" <> optArg yearsOpt <> " | " <> optArg monthsOpt <> "] " <> cmdOptArg),
      nameArg <> ": " <> bounds (lengthOpt : yearsOpt : nameOpts),
      T.intercalate ", " badgeKinds <> ": " <> bounds (monthsOpt : badgeOpts),
      "e.g. " <> T.intercalate " · " (map (cmdText tag) examples)
    ]
  where
    bounds = T.intercalate ", " . map optBounds
    optBounds CmdOpt {optKey, optMin, optMax} = optKey <> " " <> tshow optMin <> "–" <> tshow optMax

optArg :: CmdOpt -> Text
optArg opt = optKey opt <> " " <> optVar opt

cmdText :: CmdTag -> Text -> Text
cmdText tag args = "/" <> cmdName tag <> " " <> args

nameArg :: Text
nameArg = nameKind <> " " <> optVar lengthOpt

badgeKinds :: [Text]
badgeKinds = map textEncode issuableBadgeTypes

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
