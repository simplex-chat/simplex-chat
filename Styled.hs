module Styled where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Markdown
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types

data StyledString = Styled [SGR] String | StyledString :<>: StyledString

instance Semigroup StyledString where (<>) = (:<>:)

instance Monoid StyledString where mempty = plain ""

instance IsString StyledString where fromString = plain

plain :: String -> StyledString
plain = Styled []

bPlain :: ByteString -> StyledString
bPlain = Styled [] . B.unpack

styleMarkdownText :: Text -> StyledString
styleMarkdownText = styleMarkdown . parseMarkdown

styleMarkdown :: Markdown -> StyledString
styleMarkdown (s1 :|: s2) = styleMarkdown s1 <> styleMarkdown s2
styleMarkdown (Markdown Snippet s) = plain . T.unpack $ '`' `T.cons` s `T.snoc` '`'
styleMarkdown (Markdown f s) = Styled sgr $ T.unpack s
  where
    sgr = case f of
      Bold -> [SetConsoleIntensity BoldIntensity]
      Italic -> [SetUnderlining SingleUnderline, SetItalicized True]
      Underline -> [SetUnderlining SingleUnderline]
      StrikeThrough -> [SetSwapForegroundBackground True]
      Colored c -> [SetColor Foreground Vivid c]
      Snippet -> []
      NoFormat -> []

styledToANSITerm :: StyledString -> String
styledToANSITerm (Styled [] s) = s
styledToANSITerm (Styled sgr s) = setSGRCode sgr <> s <> setSGRCode [Reset]
styledToANSITerm (s1 :<>: s2) = styledToANSITerm s1 <> styledToANSITerm s2

styledToPlain :: StyledString -> String
styledToPlain (Styled _ s) = s
styledToPlain (s1 :<>: s2) = styledToPlain s1 <> styledToPlain s2

sLength :: StyledString -> Int
sLength (Styled _ s) = length s
sLength (s1 :<>: s2) = sLength s1 + sLength s2
