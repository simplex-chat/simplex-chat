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
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor (bimap, first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlpha, isAscii, isDigit, isPunctuation, isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (foldl', intercalate, mapAccumL, tails)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
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
  = Bold
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
  | BlockStart {block :: MarkdownBlock}
  | BlockEnd
  | LineBreak
  | CellBreak
  | Unknown {json :: J.Value}
  deriving (Eq, Show)

data MarkdownBlock
  = Paragraph
  | Heading {level :: Int}
  | CodeBlock {language :: Maybe Text}
  | Quote
  | ListItem {number :: Maybe Text, indent :: Int}
  | Table {columns :: [TableAlign]}
  | Rule
  | UnknownBlock {json :: J.Value}
  deriving (Eq, Show)

data TableAlign = TANone | TALeft | TACenter | TARight
  deriving (Eq, Show)

isMarker :: Format -> Bool
isMarker = \case
  BlockStart _ -> True
  BlockEnd -> True
  LineBreak -> True
  CellBreak -> True
  _ -> False

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
    | any hasBlock groups -> Just $ joinSegments $ intercalate ["\n"] $ map groupSegments groups
    | all (isNothing . format) ml -> Nothing
    | otherwise -> Just $ joinSegments ml
  where
    ls = T.lines s
    groups = markdownGroups ls
    ml = intercalate ["\n"] $ map (markdownToList . parseMarkdown) ls
    hasBlock = \case
      MdBlock b _ -> b /= Paragraph
      MdBlank _ -> False

joinSegments :: MarkdownList -> MarkdownList
joinSegments = reverse . foldl' acc []
  where
    acc ms ft@(FormattedText f t)
      | maybe False isMarker f = ft : ms
      | T.null t = ms
    acc (FormattedText f t : ms) (FormattedText f' t')
      | f == f' = FormattedText f (t <> t') : ms
    acc ms ft = ft : ms

parseMarkdownList :: Text -> MarkdownList
parseMarkdownList = markdownToList . parseMarkdown

markdownToList :: Markdown -> MarkdownList
markdownToList (Markdown f s) = [FormattedText f s]
markdownToList (m1 :|: m2) = markdownToList m1 <> markdownToList m2

maxListIndent :: Int
maxListIndent = 6

data MdGroup = MdBlock MarkdownBlock MarkdownList | MdBlank Text

data MdBlockLine = MdBlockLine Text MarkdownList Text

mdBlock :: MarkdownBlock -> Text -> [MdBlockLine] -> Text -> MdGroup
mdBlock b prefix bLines suffix = MdBlock b $ case bLines of
  MdBlockLine start content end : more ->
    let (lastEnd, nextLines) = mapAccumL (\prevEnd (MdBlockLine start' content' end') -> (end', markerSegment LineBreak (prevEnd <> "\n" <> start') : content')) end more
     in markerSegment (BlockStart b) (prefix <> start) : content <> concat nextLines <> [markerSegment BlockEnd (lastEnd <> suffix)]
  [] -> [markerSegment (BlockStart b) prefix, markerSegment BlockEnd suffix]

groupSegments :: MdGroup -> MarkdownList
groupSegments = \case
  MdBlock _ segments -> segments
  MdBlank t -> [FormattedText Nothing t]

markerSegment :: Format -> Text -> FormattedText
markerSegment = FormattedText . Just

type MdLine = (Text, Text)

data LineKind
  = KTable [TableAlign]
  | KFence Int Int (Maybe Text)
  | KHeading Int Text Text
  | KRule
  | KListItem Int (Maybe Text) Text Text
  | KQuote Int
  | KBlank
  | KParagraph Int

markdownGroups :: [Text] -> [MdGroup]
markdownGroups = groups [] . withKinds . map splitCR
  where
    splitCR l = maybe (l, "") (,"\r") $ T.stripSuffix "\r" l
    withKinds lns = let ts = map fst lns in zip lns $ zipWith lineKind ts $ drop 1 $ tails ts
    groups :: [Int] -> [(MdLine, LineKind)] -> [MdGroup]
    groups _ [] = []
    groups items kls@(((t, cr), kind) : ls) = case kind of
      KTable aligns ->
        let n = length aligns
            (delimiter, rest) = splitAt 1 ls
            (bodyLines, rest') = tableRows rest
            delimiterText = T.concat $ map (("\n" <>) . lineText . fst) delimiter
            header = tableRow n t (cr <> delimiterText)
         in mdBlock (Table aligns) "" (header : map (uncurry $ tableRow n) bodyLines) "" : groups [] rest'
      KFence col n lang ->
        let (codeLines, rest) = first (map fst) $ break (closingFence n . fst . fst) ls
            (closingLine, rest') = first (map fst) $ splitAt 1 rest
            codeLine (ct, ccr) = let (dropped, code) = dropColumns col ct in MdBlockLine dropped [FormattedText Nothing code] ccr
            fenceEnd = T.concat $ map (("\n" <>) . lineText) closingLine
         in mdBlock (CodeBlock lang) (t <> cr <> "\n") (map codeLine codeLines) fenceEnd : groups [] rest'
      KHeading lvl syntax c -> mdBlock (Heading lvl) "" [MdBlockLine syntax (parseMarkdownList c) cr] "" : groups [] ls
      KRule -> mdBlock Rule "" [MdBlockLine t [] cr] "" : groups [] ls
      KListItem col num syntax c ->
        let items' = case dropWhile (> col) items of
              ks@(k : _) | k == col -> ks
              ks -> col : ks
            (continuation, rest) = paragraphChunks ls
            continuationLine (ct, ccr)
              | isBlank ct = MdBlockLine ct [] ccr
              | otherwise = let (ind, txt) = T.span isIndentChar ct in MdBlockLine ind (parseMarkdownList txt) ccr
            item = ListItem num (min maxListIndent $ length items' - 1)
         in mdBlock item "" (MdBlockLine syntax (parseMarkdownList c) cr : map continuationLine continuation) "" : groups items' rest
      KQuote n ->
        let (qLines, rest) = first (map fst) $ splitAt n kls
            textless = T.all isSpace . snd . quoteParts . fst
            (before, qLines') = span textless qLines
            (after, inside) = bimap reverse reverse $ span textless $ reverse qLines'
            quoteLine (qt, qcr) = case quoteParts qt of
              (syntax, txt)
                | T.all isSpace txt -> MdBlockLine (syntax <> txt) [] qcr
                | otherwise -> MdBlockLine syntax (parseMarkdownList txt) qcr
            prefix = T.concat $ map ((<> "\n") . lineText) before
            suffix = T.concat $ map (("\n" <>) . lineText) after
         in mdBlock Quote prefix (map quoteLine inside) suffix : groups [] rest
      KBlank ->
        let (blanks, rest) = first (map fst) $ span (isBlank . fst . fst) kls
         in MdBlank (T.intercalate "\n" $ map lineText blanks) : groups items rest
      KParagraph _ ->
        let (paragraph, rest) = paragraphChunks kls
            paragraphLine (pt, pcr)
              | isBlank pt = MdBlockLine pt [] pcr
              | otherwise = MdBlockLine "" (parseMarkdownList pt) pcr
         in mdBlock Paragraph "" (map paragraphLine paragraph) "" : groups [] rest
    tableRows :: [(MdLine, LineKind)] -> ([MdLine], [(MdLine, LineKind)])
    tableRows = \case
      (bl@(bt, _), KParagraph 1) : more | T.any (== '|') bt -> first (bl :) $ tableRows more
      more -> ([], more)
    paragraphChunks :: [(MdLine, LineKind)] -> ([MdLine], [(MdLine, LineKind)])
    paragraphChunks = \case
      kls'@((_, KParagraph m) : _) -> let (chunk, rest) = splitAt m kls' in first (map fst chunk <>) $ paragraphChunks rest
      kls' -> ([], kls')
    lineText (lt, lcr) = lt <> lcr

lineKind :: Text -> [Text] -> LineKind
lineKind t rest
  | Just aligns <- tableStart t rest = KTable aligns
  | Just (col, n, lang) <- fenceLine t =
      if any (not . isBlank) (takeWhile (not . closingFence n) rest)
        then KFence col n lang
        else KParagraph $ 1 + fenceChunk n
  | Just (lvl, syntax, c) <- headingLine t = KHeading lvl syntax c
  | isRule t = KRule
  | Just (col, num, syntax, c) <- listItemLine t = KListItem col num syntax c
  | isQuoteLine t =
      let run = t : quoteRun rest
       in if any (T.any (not . isSpace) . snd . quoteParts) run then KQuote (length run) else KParagraph (length run)
  | isBlank t = KBlank
  | otherwise = KParagraph 1
  where
    fenceChunk n = case span isBlank rest of
      (blanks, c : _) | closingFence n c -> length blanks + 1
      _ -> 0
    quoteRun = \case
      l : ls | isQuoteLine l && isNothing (tableStart l ls) -> l : quoteRun ls
      _ -> []

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

fenceLine :: Text -> Maybe (Int, Int, Maybe Text)
fenceLine t = do
  let (ind, s) = T.span isIndentChar t
      ticks = T.length $ T.takeWhile (== '`') s
      info = T.drop ticks s
  guard $ ticks >= 3 && T.all (/= '`') info
  pure (indentColumn ind, ticks, listToMaybe $ T.words info)

closingFence :: Int -> Text -> Bool
closingFence n t =
  let s = T.dropWhile isIndentChar t
      ticks = T.length $ T.takeWhile (== '`') s
   in ticks >= n && T.all isSpace (T.drop ticks s)

headingLine :: Text -> Maybe (Int, Text, Text)
headingLine t = do
  let (ind, s) = T.span isIndentChar t
      hashes = T.takeWhile (== '#') s
      lvl = T.length hashes
  c <- T.stripPrefix " " $ T.drop lvl s
  guard $ lvl >= 1 && lvl <= 6 && T.any (not . isSpace) c
  pure (lvl, ind <> hashes <> " ", c)

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

tableStart :: Text -> [Text] -> Maybe [TableAlign]
tableStart t = \case
  d : _ | T.any (== '|') t, Just aligns <- delimiterRow d, length (tableCells t) == length aligns -> Just aligns
  _ -> Nothing

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

tableRow :: Int -> Text -> Text -> MdBlockLine
tableRow n t end = case map cellParts texts of
  (before, c, after) : more ->
    let (lastAfter, cells) = mapAccumL (\prevAfter (before', c', after') -> (after', markerSegment CellBreak (prevAfter <> "|" <> before') : parseMarkdownList c')) after more
        missing = replicate (n - 1 - length more) $ markerSegment CellBreak ""
     in MdBlockLine (lead <> before) (parseMarkdownList c <> concat cells <> missing) (lastAfter <> trail <> end)
  [] -> MdBlockLine lead [] (trail <> end)
  where
    (lead, cellTexts, trail) = tableLine t
    texts = case splitAt (n - 1) cellTexts of
      (ts, rest@(_ : _ : _)) -> ts <> [T.intercalate "|" rest]
      _ -> cellTexts
    cellParts cell =
      let before = T.takeWhile isSpace cell
          rest = T.drop (T.length before) cell
          after = T.takeWhileEnd isSpace rest
       in (before, T.dropEnd (T.length after) rest, after)

parseMarkdown :: Text -> Markdown
parseMarkdown s = fromRight (unmarked s) $ A.parseOnly (markdownP <* A.endOfInput) s

isSimplexLink :: Format -> Bool
isSimplexLink = \case
  SimplexLink {} -> True
  _ -> False

ftIsSimplexLink :: FormattedText -> Bool
ftIsSimplexLink FormattedText {format} = maybe False isSimplexLink format

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
  fromRight False (AB.parseOnly findLinkP $ encodeUtf8 $ T.filter (not . isSpace) t)
    || any (any ftIsSimplexLink . parseMarkdownList) (T.lines t)
  where
    findLinkP = do
      AB.skipWhile (\c -> c /= 's' && c /= 'h') -- links start only with "simplex:" or "https://"
      (True <$ (strP :: AB.Parser AConnectionLink))
        <|> (AB.anyChar *> findLinkP)
        <|> pure False

markdownP :: Parser Markdown
markdownP = mconcat <$> A.many' fragmentP
  where
    fragmentP :: Parser Markdown
    fragmentP =
      A.peekChar >>= \case
        Just c -> case c of
          ' ' -> unmarked <$> A.takeWhile (== ' ')
          '+' -> phoneP <|> wordP
          '*' -> boldP <|> formattedP '*' Bold
          '_' -> formattedP '_' Italic
          '~' -> formattedP '~' StrikeThrough
          '`' -> formattedP '`' Snippet
          '#' -> A.char '#' *> (secretP <|> nameRefP '#' <|> secretFallback)
          '!' -> styledP <|> wordP
          '@' -> (A.char '@' *> nameRefP '@') <|> mentionP <|> wordP
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
    boldP :: Parser Markdown
    boldP = do
      s <- A.string "**" *> A.takeTill (== '*') <* A.string "**"
      if T.null s || T.head s == ' ' || T.last s == ' '
        then fail "not bold"
        else pure $ markdown Bold s
    secretP :: Parser Markdown
    secretP = secret <$?> ((,,) <$> A.takeWhile (== '#') <*> A.takeTill (== '#') <*> A.takeWhile1 (== '#'))
    secret :: (Text, Text, Text) -> Either String Markdown
    secret (b, s, a)
      | T.null s || T.head s == ' ' || T.last s == ' ' = Left "not secret"
      | otherwise = Right $ markdown Secret $ T.init ss
      where
        ss = b <> s <> a
    secretFallback :: Parser Markdown
    secretFallback = unmarked . ('#' `T.cons`) <$> A.takeTill (== ' ')
    nameRefP :: Char -> Parser Markdown
    nameRefP pfx = nameRef <$?> A.takeTill (== ' ')
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
      f <- A.char '!' *> ((A.char '-' $> Small) <|> (colored <$> colorP)) <* A.space
      s <- ((<>) <$> A.takeWhile1 (\c -> c /= ' ' && c /= '!') <*> A.takeTill (== '!')) <* A.char '!'
      if T.null s || T.last s == ' '
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
    Bold -> around '*'
    Italic -> around '_'
    StrikeThrough -> around '~'
    Snippet -> around '`'
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
    BlockStart _ -> t
    BlockEnd -> t
    LineBreak -> t
    CellBreak -> t
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
    quoted c = A.char c *> takeNameTill (== c) <* A.char c
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

$(JQ.deriveToJSON (sumTypeJSON fstToLower) ''MarkdownBlock)

instance FromJSON MarkdownBlock where
  parseJSON v = $(JQ.mkParseJSON (sumTypeJSON fstToLower) ''MarkdownBlock) v <|> pure (UnknownBlock v)

$(JQ.deriveToJSON (sumTypeJSON fstToLower) ''Format)

instance FromJSON Format where
  parseJSON v = $(JQ.mkParseJSON (sumTypeJSON fstToLower) ''Format) v <|> pure (Unknown v)

$(JQ.deriveJSON defaultJSON ''FormattedText)

$(JQ.deriveToJSON defaultJSON ''ParsedMarkdown)
