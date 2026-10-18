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
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (foldl')
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
  = Bold {singleStar :: Bool}
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
  | Rule
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

data ParsedMarkdown = ParsedMarkdown
  { formattedText :: Maybe MarkdownList,
    markdownBlocks :: Maybe (NonEmpty MarkdownBlock)
  }
  deriving (Eq, Show)

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
    | any isBlockSyntax tokens -> markdownBlocksFold tokens
    | all (isNothing . format) inline -> ParsedMarkdown Nothing Nothing
    | otherwise -> ParsedMarkdown (Just inline) Nothing
  where
    s' = fromMaybe s $ T.stripSuffix "\r\n" s <|> T.stripSuffix "\n" s
    tokens = fromRight [FormattedText Nothing s'] $ A.parseOnly (documentP <* A.endOfInput) s'
    inline = joinSegments $ map breakAsText tokens
    breakAsText ft@(FormattedText f t) = case f of
      Just (Markup _) -> FormattedText Nothing t
      _ -> ft

isBlockSyntax :: FormattedText -> Bool
isBlockSyntax = \case
  FormattedText (Just (Markup t)) _ -> case t of
    LineBreak -> False
    SoftBreak -> False
    HardBreak -> False
    ParagraphBreak _ -> False
    Indent -> False
    _ -> True
  _ -> False

joinSegments :: MarkdownList -> MarkdownList
joinSegments = reverse . foldl' (flip joinSegment) []

joinSegment :: FormattedText -> MarkdownList -> MarkdownList
joinSegment ft@(FormattedText f t) = \case
  FormattedText f' t' : fts | f == f' && not (isMarkup f) -> FormattedText f (t' <> t) : fts
  fts
    | T.null t && not (isMarkup f) -> fts
    | otherwise -> ft : fts

isMarkup :: Maybe Format -> Bool
isMarkup = \case
  Just (Markup _) -> True
  _ -> False

parseMarkdownList :: Text -> MarkdownList
parseMarkdownList = markdownToList . parseMarkdown

markdownToList :: Markdown -> MarkdownList
markdownToList (Markdown f s) = [FormattedText f s]
markdownToList (m1 :|: m2) = markdownToList m1 <> markdownToList m2

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP LKParagraph <* A.endOfInput) s

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

data LineKind
  = LKParagraph
  | LKIndented
  | LKListItem
  | LKQuote
  | LKHeading
  | LKUnderline
  | LKFence
  | LKTable
  | LKRule
  | LKBlank
  deriving (Eq, Show)

continues :: LineKind -> LineKind -> Bool
continues block next = case block of
  LKParagraph -> paragraphLine
  LKIndented -> paragraphLine
  LKListItem -> paragraphLine
  LKQuote -> next == LKQuote || paragraphLine
  LKTable -> next == LKTable
  _ -> False
  where
    paragraphLine = next == LKParagraph || next == LKIndented

documentP :: Parser MarkdownList
documentP = do
  blanks <- blankLinesP
  ([paragraphBreak blanks | not (T.null blanks)] <>) <$> (blocksP <|> ([] <$ A.endOfInput))
  where
    blocksP = do
      b <- blockP
      rest <- A.option [] $ (:) <$> blockBreakP <*> (([] <$ A.endOfInput) <|> blocksP)
      pure $ b <> rest

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

markupMd :: BlockToken -> Text -> Markdown
markupMd = markdown . Markup

blockP :: Parser MarkdownList
blockP =
  lineKindP LKBlank >>= \case
    LKTable -> tableBlockP
    LKFence -> fenceBlockP
    LKHeading -> headingBlockP
    LKRule -> ruleBlockP
    LKListItem -> listItemBlockP
    LKQuote -> quoteBlockP
    LKIndented -> paragraphBlockP LKIndented
    LKParagraph -> paragraphBlockP LKParagraph
    _ -> fail "not a block"

lineKindP :: LineKind -> Parser LineKind
lineKindP block = lookAhead $ do
  l <- lineTextP
  if isBlank l
    then pure LKBlank
    else (LKTable <$ tableStartP l) <|> (LKFence <$ fenceStartP l) <|> otherKindP l
  where
    tableStartP l = lineEndP *> lineTextP >>= guard . isJust . tableStart l
    fenceStartP l = case fenceLine l of
      Just (_, n, ch, _) -> codeLinesP n ch >>= guard . any (not . isBlank . snd)
      Nothing -> mzero
    otherKindP l
      | isJust (headingLine l) = pure LKHeading
      | (block == LKParagraph || block == LKIndented) && isJust (underlineLevel l) = pure LKUnderline
      | isRule l = pure LKRule
      | isJust (listItemLine l) = pure LKListItem
      | isQuoteLine l && (block == LKQuote || not (isBlank $ snd $ quoteParts l)) = pure LKQuote
      | isQuoteLine l = (LKQuote <$ quoteRunTextP) <|> pure (paragraphKind l)
      | otherwise = pure $ paragraphKind l
    quoteRunTextP = A.many' (lineEndP *> lineTextP >>= \l -> l <$ guard (isQuoteLine l)) >>= guard . any (not . isBlank . snd . quoteParts)
    paragraphKind l = if T.any isIndentChar (T.take 1 l) then LKIndented else LKParagraph

lineTextP :: Parser Text
lineTextP = T.concat <$> A.many' (A.takeWhile1 (\c -> c /= '\n' && c /= '\r') <|> (A.string "\r" <* (A.peekChar >>= guard . (/= Just '\n'))))

lineEndP :: Parser Text
lineEndP = A.string "\r\n" <|> A.string "\n"

codeLinesP :: Int -> Char -> Parser [(Text, Text)]
codeLinesP n ch = A.many' $ (,) <$> lineEndP <*> (lineTextP >>= \l -> l <$ guard (not $ closingFence n ch l))

tableBlockP :: Parser MarkdownList
tableBlockP = do
  headerLine <- lineTextP
  br <- lineEndP
  delimiter <- lineTextP
  aligns <- maybe (fail "not a table") pure $ tableStart headerLine delimiter
  let n = length aligns
  rowLines <- A.many' $ do
    k <- lookAhead $ lineEndP *> lineKindP LKTable
    guard $ k == LKParagraph || k == LKIndented
    (\br' l -> markupText LineBreak br' : tableRowTokens n l) <$> lineEndP <*> (lineTextP >>= \l -> l <$ guard (T.any (== '|') l))
  pure $ tableRowTokens n headerLine <> [markupText LineBreak br, markupText (DelimiterRow aligns) delimiter] <> concat rowLines

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
      (before', c', after') : more -> cellContent c <> [markupText CellMark (after <> "|" <> before')] <> cellTokens c' after' more
      [] -> cellContent c <> [markupText CellMark (after <> trail)]
    cellContent c = if T.null c then [] else parseMarkdownList c
    cellParts cell =
      let before = T.takeWhile isSpace cell
          rest = T.drop (T.length before) cell
          after = T.takeWhileEnd isSpace rest
       in (before, T.dropEnd (T.length after) rest, after)

fenceBlockP :: Parser MarkdownList
fenceBlockP = do
  l <- lineTextP
  (col, n, ch, lang) <- maybe (fail "not a fence") pure $ fenceLine l
  code <- codeLinesP n ch
  guard $ any (not . isBlank . snd) code
  closing <- optional $ (,) <$> lineEndP <*> lineTextP
  pure $ markupText (FenceMark lang) l : concatMap (codeLine col) code <> maybe [] (\(br, cl) -> [markupText LineBreak br, markupText (FenceMark Nothing) cl]) closing
  where
    codeLine col (br, cl) =
      let (dropped, code) = dropColumns col cl
       in markupText LineBreak br : [markupText Indent dropped | not (T.null dropped)] <> [FormattedText Nothing code | not (T.null code)]

headingBlockP :: Parser MarkdownList
headingBlockP = do
  l <- lineTextP
  (lvl, syntax, c) <- maybe (fail "not a heading") pure $ headingLine l
  let (c', closing) = headingClosing c
  pure $ markupText (HeadingMark lvl) syntax : parseMarkdownList c' <> [markupText HeadingClose closing | not (T.null closing)]

ruleBlockP :: Parser MarkdownList
ruleBlockP = do
  l <- lineTextP
  guard $ isRule l
  pure [markupText RuleMark l]

listItemBlockP :: Parser MarkdownList
listItemBlockP = do
  (_, num, syntax, _) <- lookAhead lineTextP >>= maybe (fail "not a list item") pure . listItemLine
  (markupText (ListMark num) syntax :) . markdownToList <$> (A.string syntax *> markdownP LKListItem)

quoteBlockP :: Parser MarkdownList
quoteBlockP = do
  leading <- A.many' $ (\l br -> [markupText QuoteMark l, markupText LineBreak br]) <$> blankQuoteLineP <*> lineEndP
  syntax <- fst . quoteParts <$> lookAhead lineTextP
  start <- markupMd QuoteMark <$> A.string syntax
  quoteContent <- markdownP LKQuote
  trailing <- A.many' $ (\br l -> [markupText LineBreak br, markupText QuoteMark l]) <$> lineEndP <*> blankQuoteLineP
  pure $ concat leading <> markdownToList (start <> quoteContent) <> concat trailing

blankQuoteLineP :: Parser Text
blankQuoteLineP = lineTextP >>= \l -> l <$ guard (isQuoteLine l && isBlank (snd $ quoteParts l))

paragraphBlockP :: LineKind -> Parser MarkdownList
paragraphBlockP kind = do
  indent <- A.takeWhile isIndentChar
  paragraphContent <- markdownP kind
  underline <- A.option [] $ do
    k <- lookAhead $ lineEndP *> lineKindP kind
    guard $ k == LKUnderline
    br <- lineEndP
    l <- lineTextP
    pure [markupText LineBreak br, markupText (HeadingUnderline $ fromMaybe 1 $ underlineLevel l) l]
  pure $ [markupText Indent indent | not (T.null indent)] <> markdownToList paragraphContent <> underline

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

headingLine :: Text -> Maybe (Int, Text, Text)
headingLine t = do
  let (ind, s) = T.span isIndentChar t
      hashes = T.takeWhile (== '#') s
      lvl = T.length hashes
  c <- T.stripPrefix " " $ T.drop lvl s
  guard $ lvl >= 1 && lvl <= 6 && T.any (not . isSpace) c
  pure (lvl, ind <> hashes <> " ", c)

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

listItemLine :: Text -> Maybe (Int, Maybe Text, Text, Text)
listItemLine t = do
  let (ind, s) = T.span isIndentChar t
  (num, marker) <- bullet s <|> numbered s
  c <- T.stripPrefix " " $ T.drop (T.length marker) s
  guard $ T.any (not . isSpace) c
  pure (indentColumn ind, num, ind <> marker <> " ", c)
  where
    bullet s = case T.uncons s of
      Just (c, _) | c `elem` ("-*+" :: String) -> Just (Nothing, T.singleton c)
      _ -> Nothing
    numbered s =
      let digits = T.takeWhile isDigit s
       in case T.uncons $ T.drop (T.length digits) s of
            Just (c, _) | not (T.null digits) && T.length digits <= 9 && (c == '.' || c == ')') -> let marker = digits `T.snoc` c in Just (Just marker, marker)
            _ -> Nothing

isQuoteLine :: Text -> Bool
isQuoteLine = T.isPrefixOf ">" . T.dropWhile isIndentChar

quoteParts :: Text -> (Text, Text)
quoteParts t =
  let (ind, s) = T.span isIndentChar t
      s' = T.drop 1 s
      sp = T.take 1 $ T.takeWhile isIndentChar s'
   in (ind <> ">" <> sp, T.drop (T.length sp) s')

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

data BlocksFold = BlocksFold
  { revTokens :: [FormattedText],
    tokenCount :: Int,
    blockKind :: LineKind,
    atLineStart :: Bool,
    afterBlankQuoteLine :: Bool,
    openLeaf :: OpenLeaf,
    openEntries :: [OpenEntry],
    listEntries :: [ListEntry],
    revBlocks :: [MarkdownBlock]
  }

data OpenLeaf
  = NoLeaf
  | ParagraphLeaf (Maybe SegmentRange)
  | HeadingLeaf Int (Maybe SegmentRange)
  | QuoteLeaf (Maybe SegmentRange)
  | EntryLeaf
  | CodeLeaf (Maybe Text) (Maybe Int)
  | TableLeaf [TableAlign] [[SegmentRange]] (Maybe [SegmentRange]) Int

data OpenEntry = OpenEntry Int (Maybe Text) (Maybe SegmentRange) [ListEntry]

markdownBlocksFold :: MarkdownList -> ParsedMarkdown
markdownBlocksFold ts =
  let BlocksFold {revTokens, revBlocks} = closeList $ closeLeaf $ foldl' blockStep (BlocksFold [] 0 LKBlank True False NoLeaf [] [] []) ts
   in ParsedMarkdown (Just $ reverse revTokens) (L.nonEmpty $ reverse revBlocks)

blockStep :: BlocksFold -> FormattedText -> BlocksFold
blockStep st@BlocksFold {openLeaf, atLineStart} ft@(FormattedText f _) = case (openLeaf, f) of
  (CodeLeaf lang start, Just (Markup (FenceMark _))) ->
    (appendToken ft st) {openLeaf = NoLeaf, revBlocks = CodeBlock lang (SegmentRange (fromMaybe (tokenCount st) start) (tokenCount st - 1)) : revBlocks st}
  (CodeLeaf lang start, Just (Markup LineBreak)) ->
    let st' = appendToken ft st
     in st' {openLeaf = CodeLeaf lang $ start <|> Just (tokenCount st')}
  (CodeLeaf {}, _) -> appendToken ft st
  (_, Just (Markup (ParagraphBreak _))) -> (appendToken ft st) {atLineStart = True, blockKind = LKBlank}
  (_, Just (Markup tkn)) | tkn == LineBreak || tkn == SoftBreak || tkn == HardBreak -> (appendToken ft st) {atLineStart = True, afterBlankQuoteLine = afterQuoteMark}
  (ParagraphLeaf (Just r), Just (Markup (HeadingUnderline lvl))) ->
    (appendToken ft st) {atLineStart = False, blockKind = LKUnderline, openLeaf = NoLeaf, revBlocks = Heading lvl r : revBlocks st}
  _
    | atLineStart -> tokenStep ft $ lineStart (tokenLineKind ft) st {atLineStart = False}
    | otherwise -> tokenStep ft st
  where
    afterQuoteMark = case revTokens st of
      FormattedText (Just (Markup QuoteMark)) _ : _ -> True
      _ -> False
    lineStart kind st'
      | continues (blockKind st') kind && (kind == LKQuote || not (afterBlankQuoteLine st')) = st' {afterBlankQuoteLine = False}
      | otherwise = openBlock kind ft $ closeLeaf st' {blockKind = kind, afterBlankQuoteLine = False}

tokenLineKind :: FormattedText -> LineKind
tokenLineKind (FormattedText f _) = case f of
  Just (Markup tkn) -> case tkn of
    Indent -> LKIndented
    ListMark _ -> LKListItem
    QuoteMark -> LKQuote
    HeadingMark _ -> LKHeading
    HeadingUnderline _ -> LKUnderline
    FenceMark _ -> LKFence
    RowMark -> LKTable
    DelimiterRow _ -> LKTable
    RuleMark -> LKRule
    _ -> LKParagraph
  _ -> LKParagraph

openBlock :: LineKind -> FormattedText -> BlocksFold -> BlocksFold
openBlock kind (FormattedText f t) st = case kind of
  LKListItem -> openEntry st
  LKRule -> let st' = closeList st in st' {revBlocks = Rule : revBlocks st'}
  _ -> (closeList st) {openLeaf = leaf}
  where
    leaf = case (kind, f) of
      (LKQuote, _) -> QuoteLeaf Nothing
      (LKHeading, Just (Markup (HeadingMark lvl))) -> HeadingLeaf lvl Nothing
      (LKFence, Just (Markup (FenceMark lang))) -> CodeLeaf lang Nothing
      (LKTable, _) -> TableLeaf [] [] Nothing 0
      _ -> ParagraphLeaf Nothing
    openEntry st' =
      let column = indentColumn $ T.takeWhile isIndentChar t
          num = case f of
            Just (Markup (ListMark n)) -> n
            _ -> Nothing
          st'' = closeEntriesAbove column st'
          st''' = case openEntries st'' of
            OpenEntry c _ _ _ : _ | c == column || length (openEntries st'') >= maxListDepth -> closeEntry st''
            _ -> st''
       in st''' {openLeaf = EntryLeaf, openEntries = OpenEntry column num Nothing [] : openEntries st'''}

maxListDepth :: Int
maxListDepth = 7

closeEntriesAbove :: Int -> BlocksFold -> BlocksFold
closeEntriesAbove column st = case openEntries st of
  OpenEntry c _ _ _ : _ | c > column -> closeEntriesAbove column $ closeEntry st
  _ -> st

closeEntry :: BlocksFold -> BlocksFold
closeEntry st = case openEntries st of
  OpenEntry _ num r children : parents ->
    let entry = ListEntry num (fromMaybe (SegmentRange (tokenCount st) (tokenCount st)) r) (reverse children)
     in case parents of
          OpenEntry c n r' siblings : rest -> st {openEntries = OpenEntry c n r' (entry : siblings) : rest}
          [] -> st {openEntries = [], listEntries = entry : listEntries st}
  [] -> st

closeList :: BlocksFold -> BlocksFold
closeList st = case openEntries st of
  _ : _ -> closeList $ closeEntry st
  [] -> case L.nonEmpty (reverse $ listEntries st) of
    Just entries -> st {listEntries = [], revBlocks = List entries : revBlocks st}
    Nothing -> st

closeLeaf :: BlocksFold -> BlocksFold
closeLeaf st = case openLeaf st of
  ParagraphLeaf (Just r) -> closed $ Paragraph r
  HeadingLeaf lvl (Just r) -> closed $ Heading lvl r
  QuoteLeaf (Just r) -> closed $ Quote r
  CodeLeaf lang start -> closed $ CodeBlock lang $ SegmentRange (fromMaybe (tokenCount st) start) (tokenCount st)
  TableLeaf aligns tableRows row _ -> case (L.nonEmpty aligns, reverse $ withRow row tableRows) of
    (Just cols, headerRow : bodyRows) -> closed $ Table cols headerRow bodyRows
    _ -> st {openLeaf = NoLeaf}
  _ -> st {openLeaf = NoLeaf}
  where
    closed b = st {openLeaf = NoLeaf, revBlocks = b : revBlocks st}

tokenStep :: FormattedText -> BlocksFold -> BlocksFold
tokenStep ft@(FormattedText f t) st = case (f, openLeaf st) of
  (Just (Markup Indent), EntryLeaf) -> appendToken ft st
  (Just (Markup Indent), _) -> contentStep (FormattedText Nothing t) st
  (Just (Markup RowMark), TableLeaf aligns tableRows row _) ->
    let st' = appendToken ft st
     in st' {openLeaf = TableLeaf aligns (withRow row tableRows) (Just []) (tokenCount st')}
  (Just (Markup CellMark), TableLeaf aligns tableRows row cellStart) ->
    let st' = appendToken ft st
     in st' {openLeaf = TableLeaf aligns tableRows (Just $ SegmentRange cellStart (tokenCount st) : fromMaybe [] row) (tokenCount st')}
  (Just (Markup (DelimiterRow aligns)), TableLeaf _ tableRows row cellStart) ->
    (appendToken ft st) {openLeaf = TableLeaf aligns (withRow row tableRows) Nothing cellStart}
  (Just (Markup _), _) -> appendToken ft st
  _ -> contentStep ft st

withRow :: Maybe [SegmentRange] -> [[SegmentRange]] -> [[SegmentRange]]
withRow row tableRows = maybe tableRows ((: tableRows) . reverse) row

contentStep :: FormattedText -> BlocksFold -> BlocksFold
contentStep ft@(FormattedText _ t) st
  | T.null t = st
  | otherwise =
      let st' = joinToken ft st
          i = tokenCount st' - 1
          extend = Just . maybe (SegmentRange i (tokenCount st')) (\(SegmentRange a _) -> SegmentRange a (tokenCount st'))
       in case openLeaf st' of
            ParagraphLeaf r -> st' {openLeaf = ParagraphLeaf $ extend r}
            HeadingLeaf lvl r -> st' {openLeaf = HeadingLeaf lvl $ extend r}
            QuoteLeaf r -> st' {openLeaf = QuoteLeaf $ extend r}
            EntryLeaf -> case openEntries st' of
              OpenEntry c num r children : parents -> st' {openEntries = OpenEntry c num (extend r) children : parents}
              [] -> st'
            _ -> st'

appendToken :: FormattedText -> BlocksFold -> BlocksFold
appendToken ft st = st {revTokens = ft : revTokens st, tokenCount = tokenCount st + 1}

joinToken :: FormattedText -> BlocksFold -> BlocksFold
joinToken ft@(FormattedText f t) st = case revTokens st of
  FormattedText f' t' : fts | f == f' -> st {revTokens = FormattedText f (t' <> t) : fts}
  _ -> appendToken ft st

markdownP :: LineKind -> Parser Markdown
markdownP block = mconcat <$> A.many' fragmentP
  where
    fragmentP :: Parser Markdown
    fragmentP =
      A.peekChar >>= \case
        Just c -> case c of
          ' ' -> hardBreakP <|> unmarked <$> A.takeWhile (== ' ')
          '\n' -> lineBreakP ""
          '\r' -> lineBreakP "" <|> unmarked <$> A.string "\r"
          '+' -> phoneP <|> wordP
          '*' -> doubleP '*' (Bold False) <|> formattedP '*' (Bold True)
          '_' -> doubleP '_' (Bold False) <|> formattedP '_' Italic
          '~' -> doubleP '~' StrikeThrough <|> formattedP '~' StrikeThrough
          '`' -> codeSpanP
          '#' -> A.char '#' *> (secretP <|> nameRefP '#' <|> secretFallback)
          '!' -> styledP <|> wordP
          '@' -> (A.char '@' *> nameRefP '@') <|> mentionP <|> wordP
          '/' -> commandP <|> wordP
          '[' -> sowLinkP <|> wordP
          '<' -> autolinkP <|> wordP
          '\\' -> escapeP <|> (A.char '\\' *> lineBreakP "\\") <|> unmarked <$> A.string "\\"
          _
            | isDigit c -> phoneP <|> wordP
            | otherwise -> wordP
        Nothing -> fail ""
    hardBreakP :: Parser Markdown
    hardBreakP = do
      spaces <- A.takeWhile1 (== ' ')
      guard $ T.length spaces >= 2
      lineBreakP spaces
    lineBreakP :: Text -> Parser Markdown
    lineBreakP hard = breakMd hard <$> continuationP
    breakMd :: Text -> (Text, [Markdown], Markdown) -> Markdown
    breakMd hard (br, blanks, start) = markupMd brk (hard <> br) <> mconcat blanks <> start
      where
        brk
          | not (null blanks) = LineBreak
          | T.null hard = SoftBreak
          | otherwise = HardBreak
    continuationP :: Parser (Text, [Markdown], Markdown)
    continuationP = do
      br <- lineEndP
      blanks <- if block == LKQuote then A.many' ((\l br' -> markupMd QuoteMark l <> markupMd LineBreak br') <$> blankQuoteLineP <*> lineEndP) else pure []
      kind <- lineKindP block
      guard $ continues block kind && (null blanks || kind == LKQuote)
      start <- case kind of
        LKQuote -> do
          (syntax, s) <- quoteParts <$> lookAhead lineTextP
          markupMd QuoteMark <$> A.string (if isBlank s then syntax <> s else syntax)
        LKIndented -> markupMd Indent <$> A.takeWhile1 isIndentChar
        _ -> pure mempty
      pure (br, blanks, start)
    formatLines :: Format -> Text -> [((Text, [Markdown], Markdown), Text)] -> Parser Markdown
    formatLines f s0 more = do
      let lastPiece = maybe s0 snd $ listToMaybe $ reverse more
      guard $ not (T.null s0) && T.head s0 /= ' ' && not (T.null lastPiece) && T.last lastPiece /= ' '
      pure $ pieces s0 more
      where
        pieces s = \case
          (continuation, s') : rest ->
            let hard = hardBreakSuffix s
             in markdown f (T.dropEnd (T.length hard) s) :|: breakMd hard continuation :|: pieces s' rest
          [] -> markdown f s
        hardBreakSuffix s
          | T.isSuffixOf "\\" s = "\\"
          | T.length spaces >= 2 = spaces
          | otherwise = ""
          where
            spaces = T.takeWhileEnd (== ' ') s
    acrossLinesP :: Format -> (Char -> Bool) -> Parser a -> Text -> Parser Markdown
    acrossLinesP f isClose closeP s0 = do
      more <- A.many1 ((,) <$> continuationP <*> A.takeTill (\x -> isClose x || isLineEnd x))
      _ <- closeP
      formatLines f s0 more
    formattedP :: Char -> Format -> Parser Markdown
    formattedP c f = do
      s <- A.char c *> A.takeTill (\x -> x == c || isLineEnd x)
      (A.char c $> md c f s) <|> acrossLinesP f (== c) (A.char c) s <|> noFormat (c `T.cons` s)
    md :: Char -> Format -> Text -> Markdown
    md c f s
      | T.null s || T.head s == ' ' || T.last s == ' ' =
          unmarked $ c `T.cons` s `T.snoc` c
      | otherwise = markdown f s
    doubleP :: Char -> Format -> Parser Markdown
    doubleP c f = do
      let delimiter = T.pack [c, c]
      s <- A.string delimiter *> A.takeTill (\x -> x == c || isLineEnd x)
      (A.string delimiter *> checkedP s) <|> acrossLinesP f (== c) (A.string delimiter) s
      where
        checkedP s
          | T.null s || T.head s == ' ' || T.last s == ' ' = fail "not formatted"
          | otherwise = pure $ markdown f s
    codeSpanP :: Parser Markdown
    codeSpanP = codeP <|> (unmarked <$> A.takeWhile1 (== '`'))
      where
        codeP = do
          n <- T.length <$> A.takeWhile1 (== '`')
          let pieceP = T.concat <$> A.many' (A.takeWhile1 (\x -> x /= '`' && not (isLineEnd x)) <|> (A.takeWhile1 (== '`') >>= \r -> r <$ guard (T.length r /= n)))
          s0 <- pieceP
          more <- A.many' ((,) <$> continuationP <*> pieceP)
          A.takeWhile1 (== '`') >>= guard . (== n) . T.length
          let s0' :| pieces' = stripCodeSpaces $ s0 :| map snd more
          pure $ foldl' (\m (continuation, p) -> m :|: breakMd "" continuation :|: markdown Snippet p) (markdown Snippet s0') (zip (map fst more) pieces')
    escapeP :: Parser Markdown
    escapeP = do
      c <- A.char '\\' *> A.satisfy (\x -> isAscii x && (isPunctuation x || isSymbol x))
      pure $ markdown Escape "\\" :|: unmarked (T.singleton c)
    autolinkP :: Parser Markdown
    autolinkP = do
      l <- '<' `inParens` '>'
      guard $ not (T.any isSpace l)
      f <- if isEmail l then pure (HyperLink (Just l) ("mailto:" <> l)) else hyperLinkFormat l l
      pure $ markdown f $ "<" <> l <> ">"
    secretP :: Parser Markdown
    secretP = (secret <$?> ((,,) <$> A.takeWhile (== '#') <*> A.takeTill (\c -> c == '#' || isLineEnd c) <*> A.takeWhile1 (== '#'))) <|> secretLinesP
    secretLinesP = do
      s <- A.takeTill (\c -> c == '#' || isLineEnd c)
      acrossLinesP Secret (== '#') (A.char '#') s
    secret :: (Text, Text, Text) -> Either String Markdown
    secret (b, s, a)
      | T.null s || T.head s == ' ' || T.last s == ' ' = Left "not secret"
      | otherwise = Right $ markdown Secret $ T.init ss
      where
        ss = b <> s <> a
    secretFallback :: Parser Markdown
    secretFallback = unmarked . ('#' `T.cons`) <$> A.takeTill (\c -> c == ' ' || isLineEnd c)
    nameRefP :: Char -> Parser Markdown
    nameRefP pfx = nameRef <$?> A.takeTill (\c -> c == ' ' || isLineEnd c)
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
      f <- A.char '!' *> ((A.char '-' $> Small) <|> (colored <$> colorP)) <* A.satisfy (\c -> isSpace c && not (isLineEnd c))
      s <- (<>) <$> A.takeWhile1 (\c -> c /= ' ' && c /= '!' && not (isLineEnd c)) <*> A.takeTill (\c -> c == '!' || isLineEnd c)
      (A.char '!' *> styled f s) <|> acrossLinesP f (== '!') (A.char '!') s
      where
        styled f s
          | T.null s || T.last s == ' ' = fail "not styled"
          | otherwise = pure $ markdown f s
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
      f <- hyperLinkFormat t l
      pure $ markdown f $ T.concat ["[", t, "](", l, ")"]
    hyperLinkFormat t l = case strDecode $ encodeUtf8 l of
      Right lnk@(ACL _ cLink) -> case cLink of
        CLShort _ -> pure $ simplexUriFormat (Just t) lnk
        CLFull _ -> fail "full SimpleX link in hyperlink"
      Left _ -> case parseUri $ encodeUtf8 l of
        Right _ -> pure $ HyperLink (Just t) l
        Left e -> fail $ "not uri: " <> T.unpack e
    inParens open close = A.char open *> A.takeWhile1 (\c -> c /= close && not (isLineEnd c)) <* A.char close
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
    wordP = wordMD <$> A.takeTill (\c -> c == ' ' || isLineEnd c || c == '\\')
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

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

stripCodeSpaces :: NonEmpty Text -> NonEmpty Text
stripCodeSpaces ts@(t0 :| rest)
  | T.isPrefixOf " " t0 && T.isSuffixOf " " (L.last ts) && not (all (T.all (== ' ')) ts) = case L.nonEmpty rest of
      Nothing -> T.dropEnd 1 (T.drop 1 t0) :| []
      Just rest' -> T.drop 1 t0 :| (L.init rest' <> [T.dropEnd 1 (L.last rest')])
  | otherwise = ts

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
    Bold singleStar -> if singleStar then around '*' else "**" <> t <> "**"
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
    quoted c = A.char c *> takeNameTill (\x -> x == c || isLineEnd x) <* A.char c
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
