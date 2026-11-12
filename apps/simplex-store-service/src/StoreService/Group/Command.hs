{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Group.Command
  ( GroupCmd (..),
    MoneyCmd (..),
    CmdAction (..),
    groupCmdAction,
    groupCommands,
    codeP,
    badgeTypeP,
    nameCodeP,
    prepaidCodeP,
    balanceArgsP,
    recordRefundArgsP,
    closeArgsP,
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
import Simplex.Chat.Badges.Code (BadgeCode, NameCode (..), NamePrepaidCode, maxNameLength, minNameLength, parseBadgeCode, parseNameCode)
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
  | GCIssueName Int
  | GCRevoke BadgeCode
  | GCRevokeName NamePrepaidCode
  | GCMoney MoneyCmd
  deriving (Eq, Show)

data MoneyCmd = MCBalance Text | MCRecordRefund Text Int | MCClose Text
  deriving (Eq, Show)

data CmdAction
  = RunCmd GroupCmd
  | ReplyText Text
  | IgnoreMsg
  deriving (Eq, Show)

groupCmdAction :: GroupMemberRole -> Text -> CmdAction
groupCmdAction role t = case A.parseOnly cmdActionP (encodeUtf8 (T.strip t)) of
  Right (tag, r)
    | role >= cmdMinRole tag -> either ReplyText runFor r
    | Right _ <- r, Just refusal <- cmdRefusal tag -> ReplyText refusal
  _ -> IgnoreMsg
  where
    -- a name code spends the sponsor allowance and the gas key's ETH when it registers, so only admins issue them
    runFor cmd
      | nameCode cmd && role < GRAdmin = ReplyText "Only admins can issue name codes."
      | otherwise = RunCmd cmd
    nameCode = \case
      GCIssueName {} -> True
      _ -> False

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
  CTIssue -> GCIssueName <$> nameArgP <|> GCIssue <$> badgeTypeP <*> optP monthsOpt <*> optP usesOpt
  CTBulk -> GCBulk <$> badgeTypeP <*> optP monthsOpt <*> countP
  CTRevoke -> GCRevoke <$> codeP <|> GCRevokeName <$> prepaidCodeP
  CTBalance -> GCMoney <$> balanceArgsP
  CTRecordRefund -> GCMoney <$> recordRefundArgsP
  CTClose -> GCMoney <$> closeArgsP
  where
    countP = A.space *> optValueP countOpt

-- | An option's keyword, its placeholder in the usage reply and its bounds; an omitted option takes the lowest value.
data CmdOpt = CmdOpt {optKey :: Text, optVar :: Text, optMin :: Int, optMax :: Int}

lengthOpt, yearsOpt, monthsOpt, usesOpt, countOpt, centsOpt :: CmdOpt
lengthOpt = CmdOpt "length" "<length>" minNameLength maxNameLength
yearsOpt = CmdOpt "years" "<Y>" minNameYears maxNameYears
monthsOpt = CmdOpt "months" "<M>" 1 maxMonths
usesOpt = CmdOpt "uses" "<N>" 1 maxUses
countOpt = CmdOpt "count" "<N>" 1 maxBulk
centsOpt = CmdOpt "cents" "<cents>" 1 maxBound

optP :: CmdOpt -> A.Parser Int
optP opt = fromMaybe (optMin opt) <$> optional (A.space *> optValueP opt)

optValueP :: CmdOpt -> A.Parser Int
optValueP opt = A.string (encodeUtf8 (optKey opt)) *> A.space *> boundedInt opt

-- | "name <length> [years <Y>]", the length and years of a prepaid name code.
nameCodeP :: A.Parser (Int, Int)
nameCodeP = (,) <$> nameArgP <*> optP yearsOpt

nameArgP :: A.Parser Int
nameArgP = A.string (encodeUtf8 nameKind) *> A.char ' ' *> nameLengthP

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

prepaidCodeP :: A.Parser NamePrepaidCode
prepaidCodeP =
  parseNameCode . safeDecodeUtf8 <$> A.takeWhile1 (not . isSpace) >>= \case
    Just (NCPrepaid code) -> pure code
    _ -> fail "not a prepaid name code"

balanceArgsP, recordRefundArgsP, closeArgsP :: A.Parser MoneyCmd
balanceArgsP = MCBalance <$> referenceP
recordRefundArgsP = MCRecordRefund <$> referenceP <* A.space <*> boundedInt centsOpt
closeArgsP = MCClose <$> referenceP

referenceP :: A.Parser Text
referenceP = safeDecodeUtf8 <$> A.takeWhile1 (not . isSpace)

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
data CmdTag = CTIssue | CTBulk | CTRevoke | CTBalance | CTRecordRefund | CTClose
  deriving (Bounded, Enum)

cmdName :: CmdTag -> Text
cmdName = \case
  CTIssue -> "issue"
  CTBulk -> "bulk"
  CTRevoke -> "revoke"
  CTBalance -> "balance"
  CTRecordRefund -> "record_refund"
  CTClose -> "close"

cmdLabel :: CmdTag -> Text
cmdLabel = \case
  CTIssue -> "Generate a code"
  CTBulk -> "Generate many codes"
  CTRevoke -> "Revoke a code"
  CTBalance -> "Show an order's balance"
  CTRecordRefund -> "Record a balance refund"
  CTClose -> "Close a failed order"

-- The menu hints and the usage replies are built from the same kinds, options and bounds, so they cannot drift apart.
cmdParams :: CmdTag -> Text
cmdParams = \case
  CTIssue -> kindParams (nameArg : badgeKinds) <> " [" <> optArg usesOpt <> "]"
  CTBulk -> kindParams badgeKinds <> " " <> optArg countOpt
  CTRevoke -> "<code>"
  CTBalance -> referenceArg
  CTRecordRefund -> referenceArg <> " " <> optVar centsOpt
  CTClose -> referenceArg
  where
    referenceArg = "<reference>"
    kindParams kinds = "<" <> T.intercalate "|" kinds <> "> [" <> optKey monthsOpt <> " <N>]"

cmdUsage :: CmdTag -> Text
cmdUsage = \case
  CTIssue ->
    kindUsage
      CTIssue
      ("[" <> optVar lengthOpt <> "] [" <> optArg monthsOpt <> "] [" <> optArg usesOpt <> "]")
      [(nameArg, [lengthOpt]), (badgeKindsText, [monthsOpt, usesOpt])]
      ["name 6", "supporter months 12"]
  CTBulk -> kindUsage CTBulk ("[" <> optArg monthsOpt <> "] " <> optArg countOpt) [(badgeKindsText, [monthsOpt, countOpt])] ["legend months 3 count 10"]
  tag -> "Usage: " <> cmdText tag (cmdParams tag)
  where
    badgeKindsText = T.intercalate ", " badgeKinds

kindUsage :: CmdTag -> Text -> [(Text, [CmdOpt])] -> [Text] -> Text
kindUsage tag kindArgs kinds examples =
  T.intercalate "\n" $
    ["Usage: " <> cmdText tag ("<kind> " <> kindArgs)]
      <> map (\(kind, opts) -> kind <> ": " <> bounds opts) kinds
      <> ["e.g. " <> T.intercalate " · " (map (cmdText tag) examples)]
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
  CTBalance -> GROwner
  CTRecordRefund -> GROwner
  CTClose -> GROwner

-- | This is the reply to a well-formed command from a sender who may not run it; Nothing means silence.
cmdRefusal :: CmdTag -> Maybe Text
cmdRefusal = \case
  CTIssue -> Nothing
  CTBulk -> Nothing
  -- The sender has just published a code that stays redeemable, so they must learn it was not revoked.
  CTRevoke -> Just "Only admins can revoke codes. This code is now visible to all members."
  CTBalance -> Nothing
  CTRecordRefund -> Nothing
  CTClose -> Nothing
