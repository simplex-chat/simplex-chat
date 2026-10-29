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
import qualified Data.Attoparsec.ByteString.Char8 as AB
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlpha, isAscii, isDigit, isPunctuation, isSpace, isSymbol)
import Data.Either (fromRight, isLeft, lefts)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Semigroup (sconcat)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AConnectionLink (..), ConnReqUriData (..), ConnShortLink (..), ConnectionLink (..), ConnectionRequestUri (..), ContactConnType (..), SMPQueue (..), SimplexNameInfo (..), simplexConnReqUri, simplexShortLink)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fstToLower, sumTypeJSON)
import Simplex.Messaging.Protocol (ProtocolServer (..))
import Simplex.Messaging.Util (decodeJSON, safeDecodeUtf8, tshow, (<$?>))
import System.Console.ANSI.Types
import qualified Text.Email.Validate as Email
import qualified URI.ByteString as U

data Markdown = Markdown (Maybe Format) Text | Markdown :|: Markdown
  deriving (Eq, Show)

data Format
  = Bold {singleStar :: BoolDef}
  | Italic
  | StrikeThrough
  | Snippet
  | Secret
  | Small
  | Colored {color :: FormatColor}
  | Uri
  -- showText is Nothing for the usual Uri without text
  | HyperLink {showText :: Maybe Text, linkUri :: Text}
  | SimplexLink {showText :: Maybe Text, linkType :: SimplexLinkType, simplexUri :: AConnectionLink, smpHosts :: NonEmpty Text}
  | SimplexName {nameInfo :: SimplexNameInfo}
  | Command {commandStr :: Text}
  | Mention {memberName :: Text}
  | Email
  | Phone
  | Escape
  | Markup {token :: BlockToken}
  | Unknown {json :: J.Value}
  deriving (Eq, Show)

data BlockToken
  = LineBreak
  | SoftBreak
  | HardBreak
  | ParagraphBreak {lineBreaks :: Int}
  | Indent
  | HeadingMark {level :: Int}
  | HeadingClose
  | HeadingUnderline {level :: Int}
  | ListMark {number :: Maybe Text}
  | QuoteMark
  | FenceMark {language :: Maybe Text}
  | RowMark
  | CellMark
  | DelimiterRow {columns :: [TableAlign]}
  | RuleMark
  deriving (Eq, Show)

data MarkdownBlock
  = Paragraph {content :: SegmentRange}
  | Heading {level :: Int, content :: SegmentRange}
  | CodeBlock {language :: Maybe Text, content :: SegmentRange}
  | Quote {content :: SegmentRange}
  | List {items :: NonEmpty ListEntry}
  | Table {columns :: NonEmpty TableAlign, header :: [SegmentRange], rows :: [[SegmentRange]]}
  | Rule {mark :: SegmentRange}
  | UnknownBlock {json :: J.Value}
  deriving (Eq, Show)

data ListEntry = ListEntry {number :: Maybe Text, content :: SegmentRange, items :: [ListEntry]}
  deriving (Eq, Show)

data SegmentRange = SegmentRange {from :: Int, to :: Int}
  deriving (Eq, Show)

data TableAlign = TANone | TALeft | TACenter | TARight
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

data ParsedMarkdown = ParsedMarkdown {formattedText :: Maybe MarkdownList, markdownBlocks :: Maybe (NonEmpty MarkdownBlock)}

unmarked :: Text -> Markdown
unmarked = Markdown Nothing

parseMaybeMarkdownList :: Text -> Maybe MarkdownList
parseMaybeMarkdownList s = let ParsedMarkdown {formattedText} = parsedMarkdown s in formattedText

parsedMarkdown :: Text -> ParsedMarkdown
parsedMarkdown s = case T.lines s of
  [] -> ParsedMarkdown Nothing Nothing
  [l]
    | T.null cmd -> ParsedMarkdown Nothing Nothing
    | isCmd -> ParsedMarkdown (Just [FormattedText (Just $ Command cmd) l]) Nothing
    where
      (isCmd, cmd) = case T.uncons $ T.dropWhile (== ' ') l of
        Just (c, rest) -> (c == '/', rest)
        Nothing -> (False, "")
  _
    | any isBlockSyntax tokens && length [f | FormattedText f _ <- tokens, isMarkup f] <= maxMarkupTokens -> ParsedMarkdown (Just tokens) (L.nonEmpty $ markdownBlocksOf tokens)
    | all (isNothing . format) inline -> ParsedMarkdown Nothing Nothing
    | otherwise -> ParsedMarkdown (Just inline) Nothing
  where
    s' = fromMaybe s $ T.stripSuffix "\r\n" s <|> T.stripSuffix "\n" s
    tokens = joinSegments $ fromRight [FormattedText Nothing s'] $ A.parseOnly (documentP <* A.endOfInput) s'
    inline = joinSegments $ map breakAsText tokens
    breakAsText ft@(FormattedText f t) = if isMarkup f then FormattedText Nothing t else ft

isBlockSyntax :: FormattedText -> Bool
isBlockSyntax = \case
  FormattedText (Just (Markup t)) _ -> case t of
    LineBreak -> False
    SoftBreak -> False
    HardBreak -> False
    ParagraphBreak _ -> False
    _ -> True
  _ -> False

joinSegments :: MarkdownList -> MarkdownList
joinSegments = map joinGroup . L.groupBy (\(FormattedText f _) (FormattedText f' _) -> f == f' && not (isMarkup f))

joinGroup :: NonEmpty FormattedText -> FormattedText
joinGroup g@(FormattedText f _ :| _) = FormattedText f $ T.concat [t | FormattedText _ t <- L.toList g]

isMarkup :: Maybe Format -> Bool
isMarkup = \case
  Just (Markup _) -> True
  _ -> False

markdownBlocksOf :: MarkdownList -> [MarkdownBlock]
markdownBlocksOf = groupLists . mapMaybe (blockOf <=< L.nonEmpty) . splitBlocks . zip [0 ..]
  where
    splitBlocks ts = case break (isBlockBreak . snd) ts of
      (b, _ : rest) -> b : splitBlocks rest
      (b, []) -> [b]
    isBlockBreak = \case
      FormattedText (Just (Markup LineBreak)) _ -> True
      FormattedText (Just (Markup (ParagraphBreak _))) _ -> True
      _ -> False

blockOf :: NonEmpty (Int, FormattedText) -> Maybe (Either (Int, Maybe Text, SegmentRange) MarkdownBlock)
blockOf ts@((i, FormattedText f t) :| _) = case f of
  Just (Markup tkn) -> case tkn of
    HeadingMark lvl -> Right . Heading lvl <$> contentRange
    ListMark num -> (\r -> Left (indentColumn $ T.takeWhile isIndentChar t, num, r)) <$> contentRange
    QuoteMark -> Right . Quote <$> contentRange
    FenceMark lang -> Just $ Right $ CodeBlock lang $ SegmentRange (i + 2) (max (i + 2) codeEnd)
    RowMark -> Right <$> tableBlock (L.toList ts)
    RuleMark -> Just $ Right $ Rule $ SegmentRange i (i + 1)
    _ -> paragraph
  _ -> paragraph
  where
    contentRange = (\is -> SegmentRange (L.head is) (L.last is + 1)) <$> L.nonEmpty [j | (j, FormattedText f' _) <- L.toList ts, not (isMarkup f')]
    codeEnd = case L.last ts of
      (j, FormattedText (Just (Markup (FenceMark _))) _) -> j - 1
      (j, _) -> j + 1
    paragraph = case L.last ts of
      (_, FormattedText (Just (Markup (HeadingUnderline lvl))) _) -> Right . Heading lvl <$> contentRange
      _ -> Right . Paragraph <$> contentRange

tableBlock :: [(Int, FormattedText)] -> Maybe MarkdownBlock
tableBlock ts = do
  cols <- L.nonEmpty $ concat [aligns | (_, FormattedText (Just (Markup (DelimiterRow aligns))) _) <- ts]
  headerRow : bodyRows <- pure $ tableRows [(i, tkn) | (i, FormattedText (Just (Markup tkn)) _) <- ts, tkn == RowMark || tkn == CellMark]
  pure $ Table cols headerRow bodyRows
  where
    tableRows = \case
      (i, RowMark) : marks ->
        let (cellMarks, marks') = span ((== CellMark) . snd) marks
            js = map fst cellMarks
         in zipWith (\a b -> SegmentRange (a + 1) b) (i : js) js : tableRows marks'
      _ : marks -> tableRows marks
      [] -> []

groupLists :: [Either (Int, Maybe Text, SegmentRange) MarkdownBlock] -> [MarkdownBlock]
groupLists = \case
  Right b : bs -> b : groupLists bs
  bs@(Left _ : _) ->
    let (entries, rest) = span isLeft bs
     in maybe id ((:) . List) (L.nonEmpty $ nestEntries 1 $ lefts entries) (groupLists rest)
  [] -> []

nestEntries :: Int -> [(Int, Maybe Text, SegmentRange)] -> [ListEntry]
nestEntries depth = \case
  (column, num, r) : rest ->
    let (children, siblings) = if depth < maxListDepth then span (\(c, _, _) -> c > column) rest else ([], rest)
     in ListEntry num r (nestEntries (depth + 1) children) : nestEntries depth siblings
  [] -> []

maxListDepth :: Int
maxListDepth = 7

maxMarkupTokens :: Int
maxMarkupTokens = 2000

parseMarkdownList :: Text -> MarkdownList
parseMarkdownList = markdownToList . parseMarkdown

markdownToList :: Markdown -> MarkdownList
markdownToList (Markdown f s) = [FormattedText f s]
markdownToList (m1 :|: m2) = markdownToList m1 <> markdownToList m2

joinMarkdown :: [Markdown] -> Markdown
joinMarkdown ms = case map joinGroup $ L.groupBy sameFormat $ filter (\(FormattedText _ t) -> not $ T.null t) $ concatMap markdownToList ms of
  [] -> mempty
  fts -> foldr1 (:|:) [Markdown f t | FormattedText f t <- fts]
  where
    sameFormat (FormattedText f _) (FormattedText f' _) = f == f'

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP <* A.endOfInput) s

isSimplexLink :: Format -> Bool
isSimplexLink = \case
  SimplexLink {} -> True
  _ -> False

isLink :: Format -> Bool
isLink = \case
  Uri -> True
  HyperLink {} -> True
  SimplexLink {} -> True
  SimplexName {} -> True
  _ -> False

hasLinks :: MarkdownList -> Bool
hasLinks = any $ \(FormattedText f _) -> maybe False isLink f

hasObfuscatedSimplexLink :: Text -> Bool
hasObfuscatedSimplexLink t =
  fromRight False $ AB.parseOnly findLinkP $ encodeUtf8 $ T.filter (not . isSpace) t
  where
    findLinkP = do
      AB.skipWhile (\c -> c /= 's' && c /= 'h') -- links start only with "simplex:" or "https://"
      (True <$ (strP :: AB.Parser AConnectionLink))
        <|> (AB.anyChar *> findLinkP)
        <|> pure False

markdownP :: Parser Markdown
markdownP = joinMarkdown <$> A.many' fragmentP
  where
    fragmentP :: Parser Markdown
    fragmentP =
      A.peekChar >>= \case
        Just c -> case c of
          ' ' -> unmarked <$> A.takeWhile (== ' ')
          '\n' -> unmarked <$> A.string "\n"
          '+' -> phoneP <|> wordP
          '*' -> doubleP '*' (Bold $ BoolDef False) <|> formattedP '*' (Bold $ BoolDef True)
          '_' -> doubleP '_' (Bold $ BoolDef False) <|> formattedP '_' Italic
          '~' -> doubleP '~' StrikeThrough <|> formattedP '~' StrikeThrough
          '`' -> codeSpanP
          '#' -> A.char '#' *> (secretP <|> nameRefP '#' <|> secretFallback)
          '!' -> styledP <|> wordP
          '@' -> (A.char '@' *> nameRefP '@') <|> mentionP <|> wordP
          '/' -> commandP <|> wordP
          '[' -> sowLinkP <|> wordP
          '<' -> autolinkP <|> wordP
          '\\' -> escapeP <|> wordP
          _
            | isDigit c -> phoneP <|> wordP
            | otherwise -> wordP
        Nothing -> fail ""
    formattedP :: Char -> Format -> Parser Markdown
    formattedP c f = do
      s <- A.char c *> A.takeTill (\x -> x == c || x == '\n')
      (A.char c $> md c f s) <|> (markdown f <$> acrossLinesP s) <|> noFormat (c `T.cons` s)
      where
        acrossLinesP s = do
          s' <- ((s <> "\n") <>) <$> (A.char '\n' *> A.takeTill (== c) <* A.char c)
          if T.null s || isSpace (T.head s) || isSpace (T.last s') then fail "not formatted" else pure s'
    md :: Char -> Format -> Text -> Markdown
    md c f s
      | T.null s || T.head s == ' ' || T.last s == ' ' =
          unmarked $ c `T.cons` s `T.snoc` c
      | otherwise = markdown f s
    doubleP :: Char -> Format -> Parser Markdown
    doubleP c f = do
      s <- A.string cc *> A.takeTill (== c) <* A.string cc
      if T.null s || isSpace (T.head s) || isSpace (T.last s)
        then fail "not formatted"
        else pure $ markdown f s
      where
        cc = T.pack [c, c]
    codeSpanP :: Parser Markdown
    codeSpanP = do
      ticks <- A.takeWhile1 (== '`')
      let pieceP = A.takeWhile1 (/= '`') <|> (A.takeWhile1 (== '`') >>= \r -> r <$ guard (r /= ticks))
      (markdown Snippet . stripSpaces . T.concat <$> A.many' pieceP <* A.string ticks) <|> pure (unmarked ticks)
      where
        stripSpaces s
          | T.isPrefixOf " " s && T.isSuffixOf " " s && T.any (/= ' ') s = T.drop 1 $ T.dropEnd 1 s
          | otherwise = s
    escapeP :: Parser Markdown
    escapeP = do
      c <- A.char '\\' *> A.satisfy (\x -> isAscii x && (isPunctuation x || isSymbol x))
      pure $ markdown Escape "\\" <> unmarked (T.singleton c)
    secretP :: Parser Markdown
    secretP = secret <$?> ((,,) <$> A.takeWhile (== '#') <*> A.takeTill (== '#') <*> A.takeWhile1 (== '#'))
    secret :: (Text, Text, Text) -> Either String Markdown
    secret (b, s, a)
      | T.null s || isSpace (T.head s) || isSpace (T.last s) = Left "not secret"
      | otherwise = Right $ markdown Secret $ T.init ss
      where
        ss = b <> s <> a
    secretFallback :: Parser Markdown
    secretFallback = unmarked . ('#' `T.cons`) <$> A.takeTill (\c -> c == ' ' || c == '\n')
    nameRefP :: Char -> Parser Markdown
    nameRefP pfx = nameRef <$?> A.takeTill (\c -> c == ' ' || c == '\n')
      where
        nameRef word
          | pfx == '@' && T.all (/= '.') name = Left "not a name"
          | otherwise = mkMd <$> strDecode (encodeUtf8 full)
          where
            (name, punct) = splitPunctuation word
            full = pfx `T.cons` name
            mkMd ni
              | T.null punct = md'
              | otherwise = md' :|: unmarked punct
              where
                md' = markdown (SimplexName ni) full
    styledP :: Parser Markdown
    styledP = do
      f <- A.char '!' *> ((A.char '-' $> Small) <|> (colored <$> colorP)) <* A.satisfy (\c -> isSpace c && c /= '\n')
      s <- ((<>) <$> A.takeWhile1 (\c -> not (isSpace c) && c /= '!') <*> A.takeTill (== '!')) <* A.char '!'
      if T.null s || isSpace (T.last s)
        then fail "not styled"
        else pure $ markdown f s
    mentionP = prefixedStringP '@' displayNameTextP_ Mention
    commandP = prefixedStringP '/' commandTextP Command
    prefixedStringP pfx parser format = do
      c <- A.char pfx *> A.peekChar'
      (str, punct) <- parser
      let origStr = if c == '\'' then '\'' `T.cons` str `T.snoc` '\'' else str
          res = markdown (format str) (pfx `T.cons` origStr)
      pure $ if T.null punct then res else res :|: unmarked punct
    sowLinkP = do
      t <- enclosedP '[' ']' (\c -> c == '[' || c == '\n')
      l <- enclosedP '(' ')' isSpace
      let hasPunct = T.any (\c -> isPunctuation c && c /= '-' && c /= '_') t
      when (hasPunct && t /= l && ("https://" <> t) /= l) $ fail "punctuation in hyperlink text"
      f <- hyperLinkFormat t l
      pure $ markdown f $ T.concat ["[", t, "](", l, ")"]
    autolinkP = do
      l <- enclosedP '<' '>' (\c -> c == '<' || isSpace c)
      f <-if isEmail l then pure (HyperLink (Just l) ("mailto:" <> l)) else hyperLinkFormat l l
      pure $ markdown f $ "<" <> l <> ">"
    hyperLinkFormat t l = case strDecode $ encodeUtf8 l of
      Right lnk@(ACL _ cLink) -> case cLink of
        CLShort _ -> pure $ simplexUriFormat (Just t) lnk
        CLFull _ -> fail "full SimpleX link in hyperlink"
      Left _ -> case parseUri $ encodeUtf8 l of
        Right _ -> pure $ HyperLink (Just t) l
        Left e -> fail $ "not uri: " <> T.unpack e
    enclosedP open close stop = A.char open *> A.takeWhile1 (\c -> c /= close && not (stop c)) <* A.char close
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
      s <- segmentsP $ fromMaybe "" country <> fromMaybe "" code
      let len = T.length s
      if 7 <= len && len <= 22 then pure $ markdown Phone s else fail "not phone"
    segmentsP s
      | T.length s > 22 = fail "not phone"
      | otherwise = optional ((<>) <$> phoneSep <*> A.takeWhile1 isDigit) >>= maybe (pure s) (segmentsP . (s <>))
    conc4 s1 s2 s3 s4 = s1 <> s2 <> s3 <> s4
    phoneSep = " " <|> "-" <|> "." <|> ""
    wordP :: Parser Markdown
    wordP = wordMD . T.concat <$> A.many' (A.takeWhile1 (\c -> c /= ' ' && c /= '\n' && c /= '\\') <|> (notP escapeP *> A.string "\\"))
    wordMD :: Text -> Markdown
    wordMD s = fromMaybe (unmarked s) $ linkMD s <|> linkAfterParen
      where
        linkAfterParen = case T.breakOn "(" s of
          (before, after)
            | not (T.null after) ->
                let (parens, rest) = T.span (== '(') after
                 in (unmarked (before <> parens) <>) <$> linkMD rest
          _ -> Nothing
    linkMD :: Text -> Maybe Markdown
    linkMD s
      | isUri s' = case strDecode $ encodeUtf8 s' of
          Right cLink -> Just $ res $ markdown (simplexUriFormat Nothing cLink) s'
          Left _ -> case parseUri $ encodeUtf8 s' of
            Right _ -> Just $ res $ markdown Uri s'
            Left _ -> Nothing
      | isDomain s' = Just $ res $ markdown Uri s'
      | isEmail s' = Just $ res $ markdown Email s'
      | otherwise = Nothing
      where
        (s', punct) = T.splitAt (T.length s - linkEndLength s) s
        res md' = if T.null punct then md' else md' :|: unmarked punct
    linkEndLength :: Text -> Int
    linkEndLength s = go (T.count ")" s - T.count "(" s) s 0
      where
        go unmatched t n = case T.unsnoc t of
          Just (t', c)
            | isPunctuation' c -> go unmatched t' (n + 1)
            | c == ')' && unmatched > 0 -> go (unmatched - 1) t' (n + 1)
          _ -> n
    isPunctuation' = \case
      '/' -> False
      ')' -> False
      '_' -> False
      '!' -> False
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
        CRContactUri crData _ -> SimplexLink showText (linkType' crData) cLink $ uriHosts crData
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

documentP :: Parser MarkdownList
documentP = do
  blanks <- blankLinesP
  ([paragraphBreak blanks | not (T.null blanks)] <>) <$> blocksP
  where
    blocksP = (<>) <$> blockP <*> A.option [] ((:) <$> blockBreakP <*> blocksP)

blockBreakP :: Parser FormattedText
blockBreakP = do
  br <- lineEndP
  blanks <- blankLinesP
  pure $ if T.null blanks then markupText LineBreak br else paragraphBreak (br <> blanks)

blankLinesP :: Parser Text
blankLinesP = (<>) <$> (T.concat <$> A.many' ((<>) <$> A.takeWhile isIndentChar <*> lineEndP)) <*> A.option "" (A.takeWhile1 isIndentChar <* A.endOfInput)

paragraphBreak :: Text -> FormattedText
paragraphBreak t = markupText (ParagraphBreak $ T.count "\n" t) t

markupText :: BlockToken -> Text -> FormattedText
markupText = FormattedText . Just . Markup

blockP :: Parser MarkdownList
blockP = tableP <|> fenceP <|> headingP <|> ruleP <|> listItemP <|> quoteP <|> paragraphP

blockStartP :: Parser ()
blockStartP = void tableStartP <|> void fenceStartP <|> void headingMarkP <|> void ruleP <|> void listMarkP <|> quoteStartP

paragraphLineP :: Parser ()
paragraphLineP = hasTextP *> notP blockStartP

hasTextP :: Parser ()
hasTextP = lookAhead lineTextP >>= guard . not . isBlank

notP :: Parser a -> Parser ()
notP p = ((True <$ lookAhead p) <|> pure False) >>= guard . not

tableP :: Parser MarkdownList
tableP = do
  (headerLine, br, delimiter, aligns) <- tableStartP
  let row = tableRowTokens $ length aligns
  bodyRows <- A.many' $ (\br' l -> markupText HardBreak br' : row l) <$> lineEndP <* notP blockStartP <*> (lineTextP >>= \l -> l <$ guard (T.any (== '|') l))
  pure $ row headerLine <> [markupText HardBreak br, markupText (DelimiterRow aligns) delimiter] <> concat bodyRows

tableStartP :: Parser (Text, Text, Text, [TableAlign])
tableStartP = do
  headerLine <- lineTextP
  br <- lineEndP
  delimiter <- lineTextP
  aligns <- maybe mzero pure $ tableStart headerLine delimiter
  pure (headerLine, br, delimiter, aligns)

tableRowTokens :: Int -> Text -> MarkdownList
tableRowTokens n t = case map cellParts cellTexts of
  (before, c, after) : more -> markupText RowMark (lead <> before) : cellTokens c after more
  [] -> [markupText RowMark lead, markupText CellMark trail]
  where
    (lead, allCells, trail) = tableLine t
    cellTexts = case splitAt (n - 1) allCells of
      (ts, rest@(_ : _ : _)) -> ts <> [T.intercalate "|" rest]
      _ -> allCells
    cellTokens c after = \case
      (before', c', after') : more -> cellText c <> [markupText CellMark (after <> "|" <> before')] <> cellTokens c' after' more
      [] -> cellText c <> [markupText CellMark (after <> trail)]
    cellText c = if T.null c then [] else parseMarkdownList c
    cellParts cell =
      let before = T.takeWhile isSpace cell
          rest = T.drop (T.length before) cell
          after = T.takeWhileEnd isSpace rest
       in (before, T.dropEnd (T.length after) rest, after)

fenceP :: Parser MarkdownList
fenceP = do
  (l, (col, n, ch, lang)) <- fenceStartP
  codeLines <- A.many' $ (,) <$> lineEndP <*> (lineTextP >>= \cl -> cl <$ guard (not $ closingFence n ch cl))
  closing <- optional $ (,) <$> lineEndP <*> lineTextP
  pure $ markupText (FenceMark lang) l : concatMap (codeLine col) codeLines <> maybe [] (\(br, cl) -> [markupText HardBreak br, markupText (FenceMark Nothing) cl]) closing
  where
    codeLine col (br, cl) =
      let (ind, code) = dropColumns col cl
       in markupText HardBreak br : [markupText Indent ind | not (T.null ind)] <> [FormattedText Nothing code | not (T.null code)]

fenceStartP :: Parser (Text, (Int, Int, Char, Maybe Text))
fenceStartP = do
  l <- lineTextP
  fence <- maybe mzero pure $ fenceLine l
  (l, fence) <$ lookAhead lineEndP

headingP :: Parser MarkdownList
headingP = do
  (lvl, syntax) <- headingMarkP
  (headingText, closing) <- headingClosing <$> lineTextP
  pure $ markupText (HeadingMark lvl) syntax : parseMarkdownList headingText <> [markupText HeadingClose closing | not (T.null closing)]

headingMarkP :: Parser (Int, Text)
headingMarkP = do
  ind <- A.takeWhile isIndentChar
  hashes <- A.takeWhile1 (== '#')
  guard $ T.length hashes <= 6
  sp <- A.string " "
  hasTextP
  pure (T.length hashes, ind <> hashes <> sp)

ruleP :: Parser MarkdownList
ruleP = lineTextP >>= \l -> [markupText RuleMark l] <$ guard (isRule l)

listItemP :: Parser MarkdownList
listItemP = do
  (num, syntax) <- listMarkP
  l <- lineTextP
  ls <- A.many' $ (\br ind l' -> (br, [markupText Indent ind], l')) <$> lineEndP <* paragraphLineP <*> A.takeWhile1 isIndentChar <*> lineTextP
  pure $ markupText (ListMark num) syntax : inlineLines l ls

listMarkP :: Parser (Maybe Text, Text)
listMarkP = do
  ind <- A.takeWhile isIndentChar
  (num, marker) <- ((Nothing,) . T.singleton <$> A.satisfy (`elem` ("-*+" :: String))) <|> numberedP
  sp <- A.string " "
  hasTextP
  pure (num, ind <> marker <> sp)
  where
    numberedP = do
      digits <- A.takeWhile1 isDigit
      guard $ T.length digits <= 9
      marker <- T.snoc digits <$> A.satisfy (\c -> c == '.' || c == ')')
      pure (Just marker, marker)

quoteP :: Parser MarkdownList
quoteP = do
  leading <- A.many' $ (\l br -> [markupText QuoteMark l, markupText HardBreak br]) <$> blankQuoteLineP <*> lineEndP
  (mark, l) <- quoteTextP
  ls <- nextLinesP
  pure $ concat leading <> (markupText QuoteMark mark : inlineLines l ls)
  where
    quoteTextP = (,) <$> quoteMarkP <* hasTextP <*> lineTextP
    nextLinesP = A.option [] $ do
      br <- lineEndP
      (syntax, l) <- quoteLineP <|> blankLineP
      ((br, syntax, l) :) <$> nextLinesP
    quoteLineP = (\(m, l) -> ([markupText QuoteMark m], l)) <$> quoteTextP
    blankLineP = (\l -> ([markupText QuoteMark l], "")) <$> blankQuoteLineP

quoteMarkP :: Parser Text
quoteMarkP = do
  ind <- A.takeWhile isIndentChar
  sp <- A.char '>' *> A.option "" (T.singleton <$> A.satisfy isIndentChar)
  pure $ ind <> ">" <> sp

blankQuoteLineP :: Parser Text
blankQuoteLineP = (<>) <$> quoteMarkP <*> A.takeWhile isIndentChar <* lookAhead (void lineEndP <|> A.endOfInput)

quoteStartP :: Parser ()
quoteStartP = optional (blankQuoteLineP *> lineEndP) *> quoteMarkP *> hasTextP

paragraphP :: Parser MarkdownList
paragraphP = do
  l <- lineTextP
  ls <- A.many' $ (,[],) <$> lineEndP <* notP underlineP <* paragraphLineP <*> lineTextP
  underline <- A.option [] $ (\br (lvl, u) -> [markupText HardBreak br, markupText (HeadingUnderline lvl) u]) <$> lineEndP <*> underlineP
  pure $ inlineLines l ls <> underline

underlineP :: Parser (Int, Text)
underlineP = lineTextP >>= \l -> maybe mzero (pure . (,l)) (underlineLevel l)

inlineLines :: Text -> [(Text, MarkdownList, Text)] -> MarkdownList
inlineLines l0 nextLines = insertBreaks breaks $ intercalate ["\n"] $ map (parseMarkdownList . T.intercalate "\n") $ runs lineTexts
  where
    (lineTexts, breaks) = go l0 nextLines
    go l = \case
      (br, syntax, l') : more ->
        let (l1, brk) = if isBlank l || isBlank l' then (l, markupText HardBreak br) else breakAfter l br
            (ls, bs) = go l' more
         in (l1 : ls, (brk : syntax) : bs)
      [] -> ([l], [])
    runs = \case
      l : ls | not (isBlank l) -> let (run, ls') = break isBlank ls in (l : run) : runs ls'
      l : ls -> [l] : runs ls
      [] -> []

breakAfter :: Text -> Text -> (Text, FormattedText)
breakAfter l br
  | T.length spaces >= 2 = (T.dropEnd (T.length spaces) l, markupText HardBreak (spaces <> br))
  | odd (T.length $ T.takeWhileEnd (== '\\') l) = (T.dropEnd 1 l, markupText HardBreak ("\\" <> br))
  | otherwise = (l, markupText HardBreak br)
  where
    spaces = T.takeWhileEnd (== ' ') l

insertBreaks :: [MarkdownList] -> MarkdownList -> MarkdownList
insertBreaks breaks = \case
  FormattedText f t : fts -> case (T.breakOn "\n" t, breaks) of
    ((t1, t2), brk : breaks') | not (T.null t2) -> segment f t1 <> brk <> insertBreaks breaks' (FormattedText f (T.drop 1 t2) : fts)
    _ -> segment f t <> insertBreaks breaks fts
  [] -> []
  where
    segment f t = [FormattedText f t | isJust f || not (T.null t)]

lineTextP :: Parser Text
lineTextP = T.concat <$> A.many' (A.takeWhile1 (\c -> c /= '\n' && c /= '\r') <|> (A.string "\r" <* (A.peekChar >>= guard . (/= Just '\n'))))

lineEndP :: Parser Text
lineEndP = A.string "\r\n" <|> A.string "\n"

isIndentChar :: Char -> Bool
isIndentChar c = c == ' ' || c == '\t'

isBlank :: Text -> Bool
isBlank = T.all isSpace

indentColumn :: Text -> Int
indentColumn = T.foldl' (\col c -> if c == '\t' then nextTabStop col else col + 1) 0

nextTabStop :: Int -> Int
nextTabStop col = (col `div` 4 + 1) * 4

dropColumns :: Int -> Text -> (Text, Text)
dropColumns n = go 0
  where
    go col s
      | col >= n = ("", s)
      | otherwise = case T.uncons s of
          Just (' ', s') -> first (T.cons ' ') $ go (col + 1) s'
          Just ('\t', s') -> first (T.cons '\t') $ go (nextTabStop col) s'
          _ -> ("", s)

fenceLine :: Text -> Maybe (Int, Int, Char, Maybe Text)
fenceLine t = do
  let (ind, s) = T.span isIndentChar t
  ch <- T.find (\c -> c == '`' || c == '~') (T.take 1 s)
  let ticks = T.length $ T.takeWhile (== ch) s
      info = T.drop ticks s
  guard $ ticks >= 3 && (ch == '~' || T.all (/= '`') info)
  pure (indentColumn ind, ticks, ch, listToMaybe $ T.words info)

closingFence :: Int -> Char -> Text -> Bool
closingFence n ch t =
  let s = T.dropWhile isIndentChar t
      ticks = T.length $ T.takeWhile (== ch) s
   in ticks >= n && T.all isSpace (T.drop ticks s)

headingClosing :: Text -> (Text, Text)
headingClosing c =
  let trimmed = T.dropWhileEnd isIndentChar c
      hashes = T.takeWhileEnd (== '#') trimmed
      before = T.dropEnd (T.length hashes) trimmed
      c' = T.dropWhileEnd isIndentChar before
   in if not (T.null hashes) && T.any (not . isSpace) c' && T.any isIndentChar (T.takeEnd 1 before)
        then (c', T.drop (T.length c') c)
        else (c, "")

underlineLevel :: Text -> Maybe Int
underlineLevel t = case T.uncons s of
  Just ('=', _) | T.all (== '=') s -> Just 1
  Just ('-', _) | T.all (== '-') s -> Just 2
  _ -> Nothing
  where
    s = T.dropWhileEnd isSpace $ T.dropWhile isIndentChar t

isRule :: Text -> Bool
isRule t = case T.uncons s of
  Just (c, _) -> c `elem` ("-*_" :: String) && T.length s >= 3 && T.all (== c) s
  Nothing -> False
  where
    s = T.filter (not . isIndentChar) t

tableStart :: Text -> Text -> Maybe [TableAlign]
tableStart t d = do
  guard $ T.any (== '|') t
  aligns <- delimiterRow d
  guard $ length (tableCells t) == length aligns
  pure aligns

delimiterRow :: Text -> Maybe [TableAlign]
delimiterRow t
  | T.any (== '|') t = mapM cellAlign $ tableCells t
  | otherwise = Nothing
  where
    cellAlign cell =
      let s = T.strip cell
          (left, s') = maybe (False, s) (True,) $ T.stripPrefix ":" s
          (right, dashes) = maybe (False, s') (True,) $ T.stripSuffix ":" s'
       in if not (T.null dashes) && T.all (== '-') dashes
            then Just $ case (left, right) of
              (True, True) -> TACenter
              (False, True) -> TARight
              (True, False) -> TALeft
              (False, False) -> TANone
            else Nothing

tableCells :: Text -> [Text]
tableCells t = let (_, cells, _) = tableLine t in cells

tableLine :: Text -> (Text, [Text], Text)
tableLine t = (ind <> lead, T.splitOn "|" body, trail)
  where
    (ind, s) = T.span isIndentChar t
    (lead, s') = maybe ("", s) ("|",) $ T.stripPrefix "|" s
    ws = T.takeWhileEnd isSpace s'
    s'' = T.dropEnd (T.length ws) s'
    (body, trail) = maybe (s'', ws) (,"|" <> ws) $ T.stripSuffix "|" s''

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
        | safe = filter (\(n, _) -> isWhitelisted n || not (isSafeBlacklisted n)) originalQS
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
    Bold (BoolDef singleStar) -> if singleStar then around '*' else "**" <> t <> "**"
    Italic -> around '_'
    StrikeThrough -> around '~'
    Snippet -> codeSpan
    Secret -> around '#'
    Small -> "!- " <> t <> "!"
    Colored (FormatColor c) -> color c
    Uri -> t
    HyperLink {} -> t
    SimplexLink {} -> t
    SimplexName {} -> t
    Mention _ -> t
    Command _ -> t
    Email -> t
    Phone -> t
    Escape -> t
    Markup _ -> t
    Unknown _ -> t
    where
      around c = c `T.cons` t `T.snoc` c
      codeSpan =
        let ticks = T.replicate (1 + maximum (0 : map T.length (filter (T.isPrefixOf "`") $ T.group t))) "`"
            pad = T.isPrefixOf "`" t || T.isSuffixOf "`" t || (T.isPrefixOf " " t && T.isSuffixOf " " t && not (T.all (== ' ') t))
            sp = if pad then " " else ""
         in ticks <> sp <> t <> sp <> ticks
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
    quoted c = A.char c *> takeNameTill (\x -> x == c || x == '\n') <* A.char c
    refChar c = c > ' ' && c /= '#' && c /= '@' && c /= '\''

commandTextP :: Parser (Text, Text)
commandTextP = commandText <$> displayNameTextP_
  where
    commandText (cmd, punct)
      | T.null cmd = (punct, "")
      | otherwise = (cmd, punct)

splitPunctuation :: Text -> (Text, Text)
splitPunctuation s = (T.dropWhileEnd isPunctuation s, T.takeWhileEnd isPunctuation s)

-- quotes names that contain spaces or end on punctuation
viewName :: Text -> Text
viewName s = if T.any isSpace s || maybe False (isPunctuation . snd) (T.unsnoc s) then "'" <> s <> "'" else s

$(JQ.deriveJSON (enumJSON $ dropPrefix "XL") ''SimplexLinkType)

$(JQ.deriveJSON (enumJSON $ dropPrefix "TA") ''TableAlign)

$(JQ.deriveJSON (sumTypeJSON fstToLower) ''BlockToken)

$(JQ.deriveToJSON (sumTypeJSON fstToLower) ''Format)

instance FromJSON Format where
  parseJSON v = $(JQ.mkParseJSON (sumTypeJSON fstToLower) ''Format) v <|> pure (Unknown v)

$(JQ.deriveJSON defaultJSON ''SegmentRange)

$(JQ.deriveJSON defaultJSON ''ListEntry)

$(JQ.deriveToJSON (sumTypeJSON fstToLower) ''MarkdownBlock)

instance FromJSON MarkdownBlock where
  parseJSON v = $(JQ.mkParseJSON (sumTypeJSON fstToLower) ''MarkdownBlock) v <|> pure (UnknownBlock v)

$(JQ.deriveJSON defaultJSON ''FormattedText)

$(JQ.deriveToJSON defaultJSON ''ParsedMarkdown)
