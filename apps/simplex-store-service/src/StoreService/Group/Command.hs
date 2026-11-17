{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Group.Command
  ( GroupCmd (..),
    MoneyCmd (..),
    CmdSource (..),
    spelledCmd,
    CmdAction (..),
    groupCmdAction,
    consoleCmd,
    groupCommands,
    maxNameYears,
    maxUses,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad (mfilter, void)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Code (BadgeCode, NameLength, NamePrepaidCode, maxNameLength, minNameLength, minNameYears, nameLength, parseBadgeCode, parseNameCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Types.Preferences (ChatBotCommand (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (boundedDecimal)

maxBulk, maxUses, maxMonths, maxNameYears :: Int
maxBulk = 100
maxUses = 1000
maxMonths = 12
maxNameYears = 10

data GroupCmd
  = GCIssue BadgeType Int Int BadgeCodePaymentStatus
  | GCBulk BadgeType Int Int
  | GCIssueName NameLength Int BadgeCodePaymentStatus
  | GCRevoke BadgeCode
  | GCRevokeName NamePrepaidCode
  | GCMoney MoneyCmd
  deriving (Eq, Show)

data MoneyCmd = MCBalance Text | MCRecordRefund Text USDCents | MCClose Text
  deriving (Eq, Show)

data CmdSource = FromConsole | FromGroup
  deriving (Eq, Show)

spelledCmd :: CmdSource -> Text -> Text
spelledCmd = \case
  FromConsole -> ("//" <>)
  FromGroup -> ("/" <>)

data CmdAction
  = RunCmd GroupCmd
  | ReplyText Text
  | IgnoreMsg
  deriving (Eq, Show)

groupCmdAction :: GroupMemberRole -> Text -> CmdAction
groupCmdAction role t = case parsedCmd FromGroup t of
  Right (tag, r)
    | role >= cmdMinRole tag -> either ReplyText RunCmd r
    | Right _ <- r, Just refusal <- cmdRefusal tag -> ReplyText refusal
  _ -> IgnoreMsg

consoleCmd :: Text -> Either Text GroupCmd
consoleCmd t = either (const $ Left usage) snd $ parsedCmd FromConsole t
  where
    usage = T.intercalate "\n" $ "Usage:" : map (\tag -> cmdText FromConsole tag (cmdParams FromConsole tag)) (sourceTags FromConsole)

parsedCmd :: CmdSource -> Text -> Either String (CmdTag, Either Text GroupCmd)
parsedCmd source = A.parseOnly (cmdActionP source) . encodeUtf8 . T.strip

-- | Left is the usage reply for an advertised command whose arguments do not parse.
cmdActionP :: CmdSource -> A.Parser (CmdTag, Either Text GroupCmd)
cmdActionP source = A.choice (map cmdP (sourceTags source))
  where
    cmdP tag = (tag,) <$> (A.string (encodeUtf8 (cmdPrefix <> cmdName tag)) *> (Right <$> fullArgsP tag <|> Left (cmdUsage source tag) <$ usageEndP))
    cmdPrefix = case source of
      FromConsole -> ""
      FromGroup -> "/"
    fullArgsP tag = A.char ' ' *> cmdArgsP source tag <* A.endOfInput
    -- Without the space check "/issued" would get a usage reply.
    usageEndP = void A.space <|> A.endOfInput

cmdArgsP :: CmdSource -> CmdTag -> A.Parser GroupCmd
cmdArgsP source = \case
  CTIssue ->
    uncurry GCIssueName <$> nameCodeP <*> statusP (nameStatuses source)
      <|> GCIssue <$> badgeTypeP <*> optP monthsOpt <*> maybe (pure $ optMin usesOpt) optP (issueUses source) <*> statusP (badgeStatuses source)
  CTBulk -> GCBulk <$> badgeTypeP <*> optP monthsOpt <*> countP
  CTRevoke -> GCRevoke <$> codeP <|> GCRevokeName <$> prepaidCodeP
  CTBalance -> GCMoney <$> balanceArgsP
  CTRecordRefund -> GCMoney <$> recordRefundArgsP
  CTClose -> GCMoney <$> closeArgsP
  where
    countP = A.space *> optValueP countOpt
    statusP statuses = A.option CPSFree (A.space *> mfilter (`elem` statuses) textTokenP)

issueUses :: CmdSource -> Maybe CmdOpt
issueUses = \case
  FromConsole -> Nothing
  FromGroup -> Just usesOpt

badgeStatuses :: CmdSource -> [BadgeCodePaymentStatus]
badgeStatuses = \case
  FromConsole -> [CPSPaid, CPSUnpaid, CPSFree]
  FromGroup -> []

-- | Nothing marks a name code paid later, so an unpaid one would be refused for good.
nameStatuses :: CmdSource -> [BadgeCodePaymentStatus]
nameStatuses = filter (/= CPSUnpaid) . badgeStatuses

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
nameCodeP :: A.Parser (NameLength, Int)
nameCodeP = A.string (encodeUtf8 nameKind) *> A.char ' ' *> ((,) <$> nameLengthP <*> optP yearsOpt)

-- | A code's text carries the length as typed, so a leading zero is refused.
nameLengthP :: A.Parser NameLength
nameLengthP =
  A.peekChar' >>= \case
    '0' -> fail "leading zero in the name length"
    _ -> boundedDecimal 0 maxBound >>= maybe (fail "length out of range") pure . nameLength

groupCommands :: [ChatBotCommand]
groupCommands = map command [minBound .. maxBound]
  where
    command tag = CBCCommand (cmdName tag) (cmdLabel tag) (Just (cmdParams FromGroup tag))

codeP :: A.Parser BadgeCode
codeP = A.takeWhile1 (not . isSpace) >>= maybe (fail "not a badge code") pure . parseBadgeCode . safeDecodeUtf8

prepaidCodeP :: A.Parser NamePrepaidCode
prepaidCodeP = A.takeWhile1 (not . isSpace) >>= maybe (fail "not a name code") pure . parseNameCode . safeDecodeUtf8

balanceArgsP, recordRefundArgsP, closeArgsP :: A.Parser MoneyCmd
balanceArgsP = MCBalance <$> referenceP
recordRefundArgsP = MCRecordRefund <$> referenceP <* A.space <*> (fromIntegral <$> boundedInt centsOpt)
closeArgsP = MCClose <$> referenceP

referenceP :: A.Parser Text
referenceP = safeDecodeUtf8 <$> A.takeWhile1 (not . isSpace)

boundedInt :: CmdOpt -> A.Parser Int
boundedInt CmdOpt {optMin, optMax} = boundedDecimal optMin optMax

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

sourceTags :: CmdSource -> [CmdTag]
sourceTags = \case
  FromConsole -> [CTIssue, CTRevoke, CTBalance, CTRecordRefund, CTClose]
  FromGroup -> [minBound .. maxBound]

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
cmdParams :: CmdSource -> CmdTag -> Text
cmdParams source = \case
  CTIssue -> kindParams (nameArg : badgeKinds) [yearsOpt, monthsOpt] <> issueExtraArgs source
  CTBulk -> kindParams badgeKinds [monthsOpt] <> " " <> optArg countOpt
  CTRevoke -> "<code>"
  CTBalance -> referenceArg
  CTRecordRefund -> referenceArg <> " " <> optVar centsOpt
  CTClose -> referenceArg
  where
    referenceArg = "<reference>"
    kindParams kinds opts = "<" <> T.intercalate "|" kinds <> "> [" <> T.intercalate "|" (map optKey opts) <> " <N>]"

cmdUsage :: CmdSource -> CmdTag -> Text
cmdUsage source = \case
  CTIssue ->
    kindUsage
      source
      CTIssue
      ("[" <> optVar lengthOpt <> "] [" <> optArg yearsOpt <> " | " <> optArg monthsOpt <> "]" <> issueExtraArgs source)
      [(nameArg, [lengthOpt, yearsOpt], nameStatuses source), (badgeKindsText, monthsOpt : maybeToList (issueUses source), badgeStatuses source)]
      ["name 6 years 5", "supporter months 12"]
  CTBulk -> kindUsage source CTBulk ("[" <> optArg monthsOpt <> "] " <> optArg countOpt) [(badgeKindsText, [monthsOpt, countOpt], [])] ["legend months 3 count 10"]
  CTRevoke -> plainUsage CTRevoke
  CTBalance -> plainUsage CTBalance
  CTRecordRefund -> plainUsage CTRecordRefund
  CTClose -> plainUsage CTClose
  where
    plainUsage tag = "Usage: " <> cmdText source tag (cmdParams source tag)
    badgeKindsText = T.intercalate ", " badgeKinds

kindUsage :: CmdSource -> CmdTag -> Text -> [(Text, [CmdOpt], [BadgeCodePaymentStatus])] -> [Text] -> Text
kindUsage source tag kindArgs kinds examples =
  T.intercalate "\n" $
    ["Usage: " <> cmdText source tag ("<kind> " <> kindArgs)]
      <> map (\(kind, opts, statuses) -> kind <> ": " <> T.intercalate ", " (map optBounds opts <> [statusesText statuses | not (null statuses)])) kinds
      <> ["e.g. " <> T.intercalate " · " (map (cmdText source tag) examples)]
  where
    optBounds CmdOpt {optKey, optMin, optMax} = optKey <> " " <> tshow optMin <> "–" <> tshow optMax

issueExtraArgs :: CmdSource -> Text
issueExtraArgs source = T.concat [" [" <> arg <> "]" | arg <- map optArg (maybeToList $ issueUses source) <> [statusesText statuses | not (null statuses)]]
  where
    statuses = badgeStatuses source

statusesText :: [BadgeCodePaymentStatus] -> Text
statusesText = T.intercalate "|" . map textEncode

optArg :: CmdOpt -> Text
optArg opt = optKey opt <> " " <> optVar opt

cmdText :: CmdSource -> CmdTag -> Text -> Text
cmdText source tag args = spelledCmd source (cmdName tag) <> " " <> args

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
