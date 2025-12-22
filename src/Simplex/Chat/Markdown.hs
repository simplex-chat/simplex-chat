{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Markdown where

import Control.Applicative (optional, (<|>))
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlpha, isAscii, isDigit, isPunctuation, isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (foldl', intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Semigroup (sconcat)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AConnectionLink (..), ConnReqUriData (..), ConnShortLink (..), ConnectionLink (..), ConnectionRequestUri (..), ContactConnType (..), SMPQueue (..), simplexConnReqUri, simplexShortLink)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fstToLower, sumTypeJSON)
import Simplex.Messaging.Protocol (ProtocolServer (..))
import Simplex.Messaging.Util (decodeJSON, safeDecodeUtf8, tshow)
import System.Console.ANSI.Types
import qualified Text.Email.Validate as Email
import qualified URI.ByteString as U

data Markdown = Markdown (Maybe Format) Text | Markdown :|: Markdown
  deriving (Eq, Show)

data Format
  = Bold
  | Italic
  | StrikeThrough
  | Snippet
  | Secret
  | Colored {color :: FormatColor}
  | Uri
  -- showText is Nothing for the usual Uri without text
  | HyperLink {showText :: Maybe Text, linkUri :: Text}
  | SimplexLink {showText :: Maybe Text, linkType :: SimplexLinkType, simplexUri :: AConnectionLink, smpHosts :: NonEmpty Text}
  | Command {commandStr :: Text}
  | Mention {memberName :: Text}
  | Email
  | Phone
  | Unknown {json :: J.Value}
  deriving (Eq, Show)

mentionedNames :: MarkdownList -> [Text]
mentionedNames = mapMaybe (\(FormattedText f _) -> mentionedName =<< f)
  where
    mentionedName = \case
      Mention name -> Just name
      _ -> Nothing

data SimplexLinkType = XLContact | XLInvitation | XLGroup | XLChannel | XLRelay
  deriving (Eq, Show)

colored :: Color -> Format
colored = Colored . FormatColor
{-# INLINE colored #-}

markdown :: Format -> Text -> Markdown
markdown = Markdown . Just
{-# INLINE markdown #-}

instance Semigroup Markdown where
  m <> (Markdown _ "") = m
  (Markdown _ "") <> m = m
  m1@(Markdown f1 s1) <> m2@(Markdown f2 s2)
    | f1 == f2 = Markdown f1 $ s1 <> s2
    | otherwise = m1 :|: m2
  m1@(Markdown f1 s1) <> ms@(Markdown f2 s2 :|: m3)
    | f1 == f2 = Markdown f1 (s1 <> s2) :|: m3
    | otherwise = m1 :|: ms
  ms@(m1 :|: Markdown f2 s2) <> m3@(Markdown f3 s3)
    | f2 == f3 = m1 :|: Markdown f2 (s2 <> s3)
    | otherwise = ms :|: m3
  m1 <> m2 = m1 :|: m2

instance Monoid Markdown where mempty = unmarked ""

instance IsString Markdown where fromString = unmarked . T.pack

newtype FormatColor = FormatColor Color
  deriving (Eq, Show)

instance FromJSON FormatColor where
  parseJSON =
    J.withText "FormatColor" $
      fmap FormatColor . \case
        "red" -> pure Red
        "green" -> pure Green
        "blue" -> pure Blue
        "yellow" -> pure Yellow
        "cyan" -> pure Cyan
        "magenta" -> pure Magenta
        "black" -> pure Black
        "white" -> pure White
        unexpected -> fail $ "unexpected FormatColor: " <> show unexpected

instance ToJSON FormatColor where
  toJSON (FormatColor c) = case c of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Yellow -> "yellow"
    Cyan -> "cyan"
    Magenta -> "magenta"
    Black -> "black"
    White -> "white"

data FormattedText = FormattedText {format :: Maybe Format, text :: Text}
  deriving (Eq, Show)

instance IsString FormattedText where
  fromString = FormattedText Nothing . T.pack

type MarkdownList = [FormattedText]

data ParsedMarkdown = ParsedMarkdown {formattedText :: Maybe MarkdownList}

unmarked :: Text -> Markdown
unmarked = Markdown Nothing

parseMaybeMarkdownList :: Text -> Maybe MarkdownList
parseMaybeMarkdownList s = case ls of
  [] -> Nothing
  [l]
    | T.null cmd -> Nothing
    | isCmd -> Just [FormattedText (Just $ Command cmd) l]
    where
      (isCmd, cmd) = case T.uncons $ T.dropWhile (== ' ') l of
        Just (c, rest) -> (c == '/', rest)
        Nothing -> (False, "")
  _
    | all (isNothing . format) ml -> Nothing
    | otherwise -> Just . reverse $ foldl' acc [] ml
  where
    ls = T.lines s
    ml = intercalate ["\n"] $ map (markdownToList . parseMarkdown) ls
    acc [] m = [m]
    acc ms@(FormattedText f t : ms') ft@(FormattedText f' t')
      | f == f' = FormattedText f (t <> t') : ms'
      | otherwise = ft : ms

parseMarkdownList :: Text -> MarkdownList
parseMarkdownList = markdownToList . parseMarkdown

markdownToList :: Markdown -> MarkdownList
markdownToList (Markdown f s) = [FormattedText f s]
markdownToList (m1 :|: m2) = markdownToList m1 <> markdownToList m2

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP <* A.endOfInput) s

isSimplexLink :: Format -> Bool
isSimplexLink = \case
  SimplexLink {} -> True
  _ -> False

markdownP :: Parser Markdown
markdownP = mconcat <$> A.many' fragmentP
  where
    fragmentP :: Parser Markdown
    fragmentP =
      A.peekChar >>= \case
        Just c -> case c of
          ' ' -> unmarked <$> A.takeWhile (== ' ')
          '+' -> phoneP <|> wordP
          '*' -> formattedP '*' Bold
          '_' -> formattedP '_' Italic
          '~' -> formattedP '~' StrikeThrough
          '`' -> formattedP '`' Snippet
          '#' -> A.char '#' *> secretP
          '!' -> coloredP <|> wordP
          '@' -> mentionP <|> wordP
          '/' -> commandP <|> wordP
          '[' -> sowLinkP <|> wordP
          _
            | isDigit c -> phoneP <|> wordP
            | otherwise -> wordP
        Nothing -> fail ""
    formattedP :: Char -> Format -> Parser Markdown
    formattedP c f = do
      s <- A.char c *> A.takeTill (== c)
      (A.char c $> md c f s) <|> noFormat (c `T.cons` s)
    md :: Char -> Format -> Text -> Markdown
    md c f s
      | T.null s || T.head s == ' ' || T.last s == ' ' =
          unmarked $ c `T.cons` s `T.snoc` c
      | otherwise = markdown f s
    secretP :: Parser Markdown
    secretP = secret <$> A.takeWhile (== '#') <*> A.takeTill (== '#') <*> A.takeWhile (== '#')
    secret :: Text -> Text -> Text -> Markdown
    secret b s a
      | T.null a || T.null s || T.head s == ' ' || T.last s == ' ' =
          unmarked $ '#' `T.cons` ss
      | otherwise = markdown Secret $ T.init ss
      where
        ss = b <> s <> a
    coloredP :: Parser Markdown
    coloredP = do
      clr <- A.char '!' *> colorP <* A.space
      s <- ((<>) <$> A.takeWhile1 (\c -> c /= ' ' && c /= '!') <*> A.takeTill (== '!')) <* A.char '!'
      if T.null s || T.last s == ' '
        then fail "not colored"
        else pure $ markdown (colored clr) s
    mentionP = prefixedStringP '@' displayNameTextP_ Mention
    commandP = prefixedStringP '/' commandTextP Command
    prefixedStringP pfx parser format = do
      c <- A.char pfx *> A.peekChar'
      (str, punct) <- parser
      let origStr = if c == '\'' then '\'' `T.cons` str `T.snoc` '\'' else str
          res = markdown (format str) (pfx `T.cons` origStr)
      pure $ if T.null punct then res else res :|: unmarked punct
    sowLinkP = do
      t <- '[' `inParens` ']'
      l <- '(' `inParens` ')'
      let hasPunct = T.any (\c -> isPunctuation c && c /= '-' && c /= '_') t
      when (hasPunct && t /= l && ("https://" <> t) /= l) $ fail "punctuation in hyperlink text"
      f <- case strDecode $ encodeUtf8 l of
        Right lnk@(ACL _ cLink) -> case cLink of
          CLShort _ -> pure $ simplexUriFormat (Just t) lnk
          CLFull _ -> fail "full SimpleX link in hyperlink"
        Left _ -> case parseUri $ encodeUtf8 l of
          Right _ -> pure $ HyperLink (Just t) l
          Left e -> fail $ "not uri: " <> T.unpack e
      pure $ markdown f $ T.concat ["[", t, "](", l, ")"]
    inParens open close = A.char open *> A.takeWhile1 (/= close) <* A.char close
    colorP =
      A.anyChar >>= \case
        'r' -> optional "ed" $> Red
        'g' -> optional "reen" $> Green
        'b' -> optional "lue" $> Blue
        'y' -> optional "ellow" $> Yellow
        'c' -> optional "yan" $> Cyan
        'm' -> optional "agenta" $> Magenta
        '1' -> pure Red
        '2' -> pure Green
        '3' -> pure Blue
        '4' -> pure Yellow
        '5' -> pure Cyan
        '6' -> pure Magenta
        _ -> fail "not color"
    phoneP = do
      country <- optional $ T.cons <$> A.char '+' <*> A.takeWhile1 isDigit
      code <- optional $ conc4 <$> phoneSep <*> "(" <*> A.takeWhile1 isDigit <*> ")"
      segments <- mconcat <$> A.many' ((<>) <$> phoneSep <*> A.takeWhile1 isDigit)
      let s = fromMaybe "" country <> fromMaybe "" code <> segments
          len = T.length s
      if 7 <= len && len <= 22 then pure $ markdown Phone s else fail "not phone"
    conc4 s1 s2 s3 s4 = s1 <> s2 <> s3 <> s4
    phoneSep = " " <|> "-" <|> "." <|> ""
    wordP :: Parser Markdown
    wordP = wordMD <$> A.takeTill (== ' ')
    wordMD :: Text -> Markdown
    wordMD s
      | T.null s = unmarked s
      | isUri s' = case strDecode $ encodeUtf8 s of
          Right cLink -> res $ markdown (simplexUriFormat Nothing cLink) s'
          Left _ -> case parseUri $ encodeUtf8 s' of
            Right _ -> res $ markdown Uri s'
            Left _ -> unmarked s
      | isDomain s' = res $ markdown Uri s'
      | isEmail s' = res $ markdown Email s'
      | otherwise = unmarked s
      where
        punct = T.takeWhileEnd isPunctuation' s
        s' = T.dropWhileEnd isPunctuation' s
        res md' = if T.null punct then md' else md' :|: unmarked punct
    isPunctuation' = \case
      '/' -> False
      ')' -> False
      c -> isPunctuation c
    isUri s = T.length s >= 10 && any (`T.isPrefixOf` s) ["http://", "https://", "simplex:/"]
    -- matches what is likely to be a domain, not all valid domain names
    isDomain s = case T.splitOn "." s of
      [name, tld] -> isDomain_ name tld
      [sub, name, tld] -> T.length sub >= 3 && T.length sub <= 8 && isDomain_ name tld
      _ -> False
      where
        isDomain_ name tld =
          (let n = T.length name in n >= 1 && n <= 24)
            && (let n = T.length tld in n >= 2 && n <= 8)
            && (let p c = isAscii c && isAlpha c in T.all p name && T.all p tld)
    isEmail s = T.any (== '@') s && Email.isValid (encodeUtf8 s)
    noFormat = pure . unmarked
    simplexUriFormat :: Maybe Text -> AConnectionLink -> Format
    simplexUriFormat showText = \case
      ACL m (CLFull cReq) -> case cReq of
        CRContactUri crData -> SimplexLink showText (linkType' crData) cLink $ uriHosts crData
        CRInvitationUri crData _ -> SimplexLink showText XLInvitation cLink $ uriHosts crData
        where
          cLink = ACL m $ CLFull $ simplexConnReqUri cReq
          uriHosts ConnReqUriData {crSmpQueues} = L.map strEncodeText $ sconcat $ L.map (host . qServer) crSmpQueues
          linkType' ConnReqUriData {crClientData} = case crClientData >>= decodeJSON of
            Just (CRDataGroup _) -> XLGroup
            Nothing -> XLContact
      ACL m (CLShort sLnk) -> case sLnk of
        CSLContact _ ct srv _ -> SimplexLink showText (linkType' ct) cLink $ uriHosts srv
        CSLInvitation _ srv _ _ -> SimplexLink showText XLInvitation cLink $ uriHosts srv
        where
          cLink = ACL m $ CLShort $ simplexShortLink sLnk
          uriHosts srv = L.map strEncodeText $ host srv
          linkType' = \case
            CCTGroup -> XLGroup
            CCTChannel -> XLChannel
            CCTContact -> XLContact
            CCTRelay -> XLRelay
    strEncodeText :: StrEncoding a => a -> Text
    strEncodeText = safeDecodeUtf8 . strEncode

parseUri :: ByteString -> Either Text U.URI
parseUri s = case U.parseURI U.laxURIParserOptions s of
  Left e -> Left $ "Invalid URI: " <> tshow e
  Right uri@U.URI {uriScheme = U.Scheme sch, uriAuthority}
    | sch /= "http" && sch /= "https" -> Left $ "Unsupported URI scheme: " <> safeDecodeUtf8 sch
    | otherwise -> case uriAuthority of
        Nothing -> Left "No URI host"
        Just U.Authority {authorityHost = U.Host h}
          | '.' `B.notElem` h -> Left $ "Invalid URI host: " <> safeDecodeUtf8 h
          | otherwise -> Right uri

-- the heuristic for removing tracking parameters is the following:
-- 1) if the URI path looks like page name* rather than an identifier, allow the first parameter, as long as it is not blacklisted,
-- 2) also allow whitelisted parameters,
-- 3) remove all other parameters.
-- *page name: lowercase latin in snake-case or hyphen-case, allowing for sinlge leading or trailing hyphen or underscore.
sanitizeUri :: Bool -> U.URI -> Maybe U.URI
sanitizeUri safe uri@U.URI {uriAuthority, uriPath, uriQuery = U.Query originalQS} =
  let sanitizedQS
        | safe = filter (not . isSafeBlacklisted . fst) originalQS
        | isNamePath = case originalQS of
            p@(n, _) : ps -> (if isWhitelisted n || not (isBlacklisted n) then (p :) else id) $ filter (isWhitelisted . fst) ps
            [] -> []
        | otherwise = filter (isWhitelisted . fst) originalQS
   in if length sanitizedQS == length originalQS
        then Nothing
        else Just $ uri {U.uriQuery = U.Query sanitizedQS}
  where
    isSafeBlacklisted p = any (`B.isPrefixOf` p) qsSafeBlacklist
    isBlacklisted p = isSafeBlacklisted p || any ($ p) qsBlacklist
    isWhitelisted p = any (\(f, ps) -> f host && p `elem` ps) qsWhitelist
    host = maybe "" (\U.Authority {authorityHost = U.Host h} -> h) uriAuthority
    isNamePath = B.all (\c -> (c >= 'a' && c <= 'z') || c == '_' || c == '-' || c == '/') uriPath
    qsWhitelist :: [(ByteString -> Bool, [ByteString])]
    qsWhitelist =
      [ (const True, ["q", "search", "search_query", "lang", "list", "page", "text", "type"]),
        (dom "aliexpress.com", ["SearchText", "catId", "minPrice", "maxPrice"]),
        (dom "amazon.com", ["i", "rh", "k"]), -- department, filter, keyword
        (dom "baidu.com", ["wd"]), -- search string
        (dom "bing.com", ["mkt"]), -- localized results
        (dom "github.com", ["author", "diff", "ref", "w"]), -- author in search result, PR parameters
        (dom "play.google.com", ["id"]),
        (dom "reddit.com", ["t"]), -- search type, time range
        (dom "wikipedia.com", ["oldid", "uselang"]), -- to show old page revision and chosen user language
        (dom "x.com", ["f"]), -- feed type
        (dom "yahoo.com", ["p"]), -- search string
        (dom "youtube.com", ["v", "t"]), -- video ID and timestamp
        (dom "youtu.be", ["t"])
      ]
    dom d h = d == h || (('.' `B.cons` d) `B.isSuffixOf` h)
    qsBlacklist :: [ByteString -> Bool]
    qsBlacklist =
      [ (B.any (== '_')),
        ("id" `B.isSuffixOf`),
        ("source" `B.isPrefixOf`)
      ]
    qsSafeBlacklist :: [ByteString]
    qsSafeBlacklist =
      [ "ad",
        "af",
        "camp",
        "cmp",
        "dc",
        "dev",
        "ef_",
        "fb",
        "gad_",
        "gc",
        "gdf",
        "hsa_",
        "igsh",
        "li",
        "matomo_",
        "mc_",
        "mkwid",
        "msc",
        "mtm_",
        "pcrid",
        "piwik_",
        "pk_",
        "prom",
        "ref",
        "s_kw",
        "si",
        "src",
        "srs",
        "trk_",
        "tw",
        "utm",
        "ycl"
      ]

markdownText :: FormattedText -> Text
markdownText (FormattedText f_ t) = case f_ of
  Nothing -> t
  Just f -> case f of
    Bold -> around '*'
    Italic -> around '_'
    StrikeThrough -> around '~'
    Snippet -> around '`'
    Secret -> around '#'
    Colored (FormatColor c) -> color c
    Uri -> t
    HyperLink {} -> t
    SimplexLink {} -> t
    Mention _ -> t
    Command _ -> t
    Email -> t
    Phone -> t
    Unknown _ -> t
    where
      around c = c `T.cons` t `T.snoc` c
      color c = case colorStr c of
        Just cStr -> cStr <> t `T.snoc` '!'
        Nothing -> t
      colorStr = \case
        Red -> Just "!1 "
        Green -> Just "!2 "
        Blue -> Just "!3 "
        Yellow -> Just "!4 "
        Cyan -> Just "!5 "
        Magenta -> Just "!6 "
        Black -> Nothing
        White -> Nothing

displayNameTextP :: Parser Text
displayNameTextP = displayNameTextP_ >>= \(t, sfx) -> if T.null sfx then pure t else fail "Name ends with punctuation"
{-# INLINE displayNameTextP #-}

displayNameTextP_ :: Parser (Text, Text)
displayNameTextP_ = (,"") <$> quoted '\'' <|> splitPunctuation <$> takeNameTill isSpace
  where
    takeNameTill p =
      A.peekChar' >>= \c ->
        if refChar c then A.takeTill p else fail "invalid first character in display name"
    splitPunctuation s = (T.dropWhileEnd isPunctuation s, T.takeWhileEnd isPunctuation s)
    quoted c = A.char c *> takeNameTill (== c) <* A.char c
    refChar c = c > ' ' && c /= '#' && c /= '@' && c /= '\''

commandTextP :: Parser (Text, Text)
commandTextP = do
  (cmd, punct) <- displayNameTextP_
  case T.words cmd of
    (keyword : _) | T.all (\c -> isAlpha c || isDigit c || c == '_') keyword -> pure (cmd, punct)
    _ -> fail "invalid command keyword"

-- quotes names that contain spaces or end on punctuation
viewName :: Text -> Text
viewName s = if T.any isSpace s || maybe False (isPunctuation . snd) (T.unsnoc s) then "'" <> s <> "'" else s

$(JQ.deriveJSON (enumJSON $ dropPrefix "XL") ''SimplexLinkType)

$(JQ.deriveToJSON (sumTypeJSON fstToLower) ''Format)

instance FromJSON Format where
  parseJSON v = $(JQ.mkParseJSON (sumTypeJSON fstToLower) ''Format) v <|> pure (Unknown v)

$(JQ.deriveJSON defaultJSON ''FormattedText)

$(JQ.deriveToJSON defaultJSON ''ParsedMarkdown)
