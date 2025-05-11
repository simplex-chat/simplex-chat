{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Markdown where

import Control.Applicative (optional, (<|>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
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
import Simplex.Messaging.Protocol (ProtocolServer (..), sameSrvAddr)
import Simplex.Messaging.Util (decodeJSON, safeDecodeUtf8)
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
  -- linkUri is Nothing when there is no link text or it is the same as URI, in which case the text of the fragment is URI.
  -- sanitizedUri is Nothing when original URI is already sanitized.
  -- spoofed is True when link text is a valid URI, and it is different from link URI.
  | WebLink {scheme :: Text, linkUri :: Maybe Text, sanitizedUri :: Maybe Text, spoofed :: Bool}
  | SimplexLink {linkType :: SimplexLinkType, simplexUri :: AConnectionLink, smpHosts :: NonEmpty Text, spoofed :: Bool}
  | Mention {memberName :: Text}
  | Email
  | Phone
  deriving (Eq, Show)

data SimplexOrWebLink = SOWSimplex AConnectionLink | SOWWeb U.URI

parseLinkUri :: Text -> Either String SimplexOrWebLink
parseLinkUri t = case strDecode s of
  Right cLink -> Right $ SOWSimplex cLink
  Left _ -> case U.parseURI U.laxURIParserOptions s of
    Right uri@U.URI {uriAuthority} -> case uriAuthority of
      Just U.Authority {authorityHost = U.Host h}
        | B.elem '.' h -> Right $ SOWWeb uri
        | otherwise -> Left "Invalid URI host"
      Nothing -> Left "No URI host"
    Left e -> Left $ "Invalid URI: " <> show e
  where
    s = encodeUtf8 t

mentionedNames :: MarkdownList -> [Text]
mentionedNames = mapMaybe (\(FormattedText f _) -> mentionedName =<< f)
  where
    mentionedName = \case
      Mention name -> Just name
      _ -> Nothing

data SimplexLinkType = XLContact | XLInvitation | XLGroup | XLChannel
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
parseMaybeMarkdownList s
  | all (isNothing . format) ml = Nothing
  | otherwise = Just . reverse $ foldl' acc [] ml
  where
    ml = intercalate ["\n"] . map (markdownToList . parseMarkdown) $ T.lines s
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
          '[' -> webLinkP <|> wordP
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
    mentionP = do
      c <- A.char '@' *> A.peekChar'
      name <- displayNameTextP
      let sName = if c == '\'' then '\'' `T.cons` name `T.snoc` '\'' else name
      pure $ markdown (Mention name) ('@' `T.cons` sName)
    webLinkP = do
      t <- A.char '[' *> A.takeWhile1 (/= ']') <* A.char ']'
      uri <- A.char '(' *> A.takeWhile1 (/= ')') <* A.char ')'
      sowLink <- either fail pure $ parseLinkUri uri
      case sowLink of
        SOWSimplex _ -> fail "SimpleX links with link text not supported"
        SOWWeb _ -> pure ()
      let t' = T.dropAround isPunctuation $ T.filter (not . isSpace) t
          (linkUri, spoofed) = case either (\_ -> parseLinkUri ("https://" <> t')) Right $ parseLinkUri t' of
            Right _
              | t == uri -> (Nothing, False)
              | otherwise -> (Just uri, True)
            Left _ -> (Just uri, False)
      pure $ markdown (sowLinkFormat linkUri spoofed sowLink) t
    colorP =
      A.anyChar >>= \case
        'r' -> "ed" $> Red <|> pure Red
        'g' -> "reen" $> Green <|> pure Green
        'b' -> "lue" $> Blue <|> pure Blue
        'y' -> "ellow" $> Yellow <|> pure Yellow
        'c' -> "yan" $> Cyan <|> pure Cyan
        'm' -> "agenta" $> Magenta <|> pure Magenta
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
      | isUri s =
          let t = T.takeWhileEnd isPunctuation' s
              uri = uriMarkdown $ T.dropWhileEnd isPunctuation' s
           in if T.null t then uri else uri :|: unmarked t
      | isDomain s = markdown (WebLink "https" (Just $ "https://" <> s) Nothing False) s
      | isEmail s = markdown Email s
      | otherwise = unmarked s
    isPunctuation' = \case
      '/' -> False
      ')' -> False
      c -> isPunctuation c
    uriMarkdown s = case parseLinkUri s of
      Right sowLink -> markdown (sowLinkFormat Nothing False sowLink) s
      Left _ -> unmarked s
    isUri s = T.length s >= 10 && any (`T.isPrefixOf` s) ["http://", "https://", "simplex:/"]
    isDomain s = case T.splitOn "." s of
      [name, tld] -> validDomain name tld
      [sub, name, tld] -> T.length sub > 0 && T.length sub <= 8 && validDomain name tld
      _ -> False
      where
        validDomain name tld =
          (let n = T.length name in n >= 1 && n <= 24)
            && (let n = T.length tld in n >= 2 && n <= 8)
            && (let p c = isAscii c && isAlpha c in T.all p name && T.all p tld)
    isEmail s = T.any (== '@') s && Email.isValid (encodeUtf8 s)
    noFormat = pure . unmarked
    sowLinkFormat :: Maybe Text -> Bool -> SimplexOrWebLink -> Format
    sowLinkFormat linkUri spoofed = \case
      SOWSimplex cLink -> simplexUriFormat spoofed cLink
      SOWWeb uri@U.URI {uriScheme = U.Scheme sch, uriQuery = U.Query originalQS} ->
        let sanitizedQS = filter (\(p, _) -> p == "q" || p == "search") originalQS
            sanitizedUri
              | length sanitizedQS == length originalQS = Nothing
              | otherwise = Just $ safeDecodeUtf8 $ U.serializeURIRef' uri {U.uriQuery = U.Query sanitizedQS}
          in WebLink {scheme = safeDecodeUtf8 sch, linkUri, sanitizedUri, spoofed}
    simplexUriFormat :: Bool -> AConnectionLink -> Format
    simplexUriFormat spoofed = \case
      ACL m (CLFull cReq) -> case cReq of
        CRContactUri crData -> SimplexLink (linkType' crData) cLink (uriHosts crData) spoofed
        CRInvitationUri crData _ -> SimplexLink XLInvitation cLink (uriHosts crData) spoofed
        where
          cLink = ACL m $ CLFull $ simplexConnReqUri cReq
          uriHosts ConnReqUriData {crSmpQueues} = L.map strEncodeText $ sconcat $ L.map (host . qServer) crSmpQueues
          linkType' ConnReqUriData {crClientData} = case crClientData >>= decodeJSON of
            Just (CRDataGroup _) -> XLGroup
            Nothing -> XLContact
      ACL m (CLShort sLnk) -> case sLnk of
        CSLContact _ ct srv _ -> SimplexLink (linkType' ct) cLink (uriHosts srv) spoofed
        CSLInvitation _ srv _ _ -> SimplexLink XLInvitation cLink (uriHosts srv) spoofed
        where
          cLink = ACL m $ CLShort $ simplexShortLink sLnk
          uriHosts srv = L.map strEncodeText $ host srv
          linkType' = \case
            CCTGroup -> XLGroup
            CCTChannel -> XLChannel
            CCTContact -> XLContact
    strEncodeText :: StrEncoding a => a -> Text
    strEncodeText = safeDecodeUtf8 . strEncode

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
    WebLink {linkUri} -> case linkUri of
      Just uri | uri /= t && uri /= ("https://" <> t) ->
        "[" <> t <> "](" <> uri <> ")"
      _ -> t
    SimplexLink {simplexUri} -> t
    Mention _ -> t
    Email -> t
    Phone -> t
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

-- This parser allows mentions like "hello @name." that are commonly used and currently fail.
-- UI should qoute any names that are terminated by punctuation, such as "@'S.C.'"
-- The alternative would be to quote all names, but it looks ugly.
displayNameTextP :: Parser Text
displayNameTextP = quoted '\'' <|> takeNameTill (\c -> isSpace c || isPunctuation c)
  where
    takeNameTill p =
      A.peekChar' >>= \c ->
        if refChar c then A.takeTill p else fail "invalid first character in display name"
    quoted c = A.char c *> takeNameTill (== c) <* A.char c
    refChar c = c > ' ' && c /= '#' && c /= '@' && c /= '\''

-- quotes names that contain spaces or end on punctuation
viewName :: Text -> Text
viewName s = if T.any isSpace s || maybe False (isPunctuation . snd) (T.unsnoc s) then "'" <> s <> "'" else s

$(JQ.deriveJSON (enumJSON $ dropPrefix "XL") ''SimplexLinkType)

$(JQ.deriveJSON (sumTypeJSON fstToLower) ''Format)

$(JQ.deriveJSON defaultJSON ''FormattedText)

$(JQ.deriveToJSON defaultJSON ''ParsedMarkdown)
