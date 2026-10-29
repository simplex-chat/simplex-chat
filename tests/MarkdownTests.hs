{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownTests where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Markdown
import Simplex.Chat.Types (BoolDef (..))
import Simplex.Messaging.Agent.Protocol (SimplexDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util ((<$$>))
import System.Console.ANSI.Types
import Test.Hspec
import qualified URI.ByteString as U

markdownTests :: Spec
markdownTests = do
  textFormat
  secretText
  textSmall
  textColor
  textWithUri
  textWithHyperlink
  obfuscatedSimplexLinks
  textWithEmail
  textWithPhone
  textWithMentions
  textWithCommands
  textWithSimplexNames
  multilineMarkdownList
  inlineMarkdownGFM
  backslashEscapes
  textsWithoutBlocks
  blockMarkdown
  breaksInBlocks
  headings
  thematicBreaks
  lists
  quotes
  codeBlocks
  tables
  formatsAcrossLines
  liveMessages
  markdownConsumers
  linearTimeParsing
  testSanitizeUri

infixr 1 ==>, <==, <==>, ==>>, <<==, <<==>>

(==>) :: Text -> Markdown -> Expectation
s ==> m = parseMarkdown s `shouldBe` m

(<==) :: Text -> Markdown -> Expectation
s <== m = s <<== markdownToList m

(<==>) :: Text -> Markdown -> Expectation
s <==> m = (s ==> m) >> (s <== m)

(==>>) :: Text -> MarkdownList -> Expectation
s ==>> ft = parseMaybeMarkdownList s `shouldBe` Just ft

(<<==) :: Text -> MarkdownList -> Expectation
s <<== ft = T.concat (map markdownText ft) `shouldBe` s

(<<==>>) :: Text -> MarkdownList -> Expectation
s <<==>> ft = (s ==>> ft) >> (s <<== ft)

boldF :: Format
boldF = Bold $ BoolDef True

bold2F :: Format
bold2F = Bold $ BoolDef False

bold :: Text -> Markdown
bold = markdown boldF

bold2 :: Text -> Markdown
bold2 = markdown bold2F

textFormat :: Spec
textFormat = describe "text format (bold)" do
  it "correct markdown" do
    "this is *bold formatted* text"
      <==> "this is " <> bold "bold formatted" <> " text"
    "*bold formatted* text"
      <==> bold "bold formatted" <> " text"
    "this is *bold*"
      <==> "this is " <> bold "bold"
    " *bold* text"
      <==> " " <> bold "bold" <> " text"
    "   *bold* text"
      <==> "   " <> bold "bold" <> " text"
    "this is *bold* "
      <==> "this is " <> bold "bold" <> " "
    "this is *bold*   "
      <==> "this is " <> bold "bold" <> "   "
  it "correct markdown with double asterisk" do
    "this is **bold formatted** text"
      <==> "this is " <> bold2 "bold formatted" <> " text"
    "**bold formatted** text"
      <==> bold2 "bold formatted" <> " text"
    "this is **bold**"
      <==> "this is " <> bold2 "bold"
    " **bold** text"
      <==> " " <> bold2 "bold" <> " text"
    "this is **bold** "
      <==> "this is " <> bold2 "bold" <> " "
    "this is **bold**   "
      <==> "this is " <> bold2 "bold" <> "   "
  it "ignored as markdown" do
    "this is * unformatted * text"
      <==> "this is * unformatted * text"
    "this is *unformatted * text"
      <==> "this is *unformatted * text"
    "this is * unformatted* text"
      <==> "this is * unformatted* text"
    "this is ** unformatted ** text"
      <==> "this is ** unformatted ** text"
    "this is **unformatted ** text"
      <==> "this is **unformatted ** text"
    "this is ** unformatted** text"
      <==> "this is ** unformatted** text"
    "this is **unformatted text"
      <==> "this is **unformatted text"
    "this is **unformatted* text"
      <==> "this is **unformatted* text"
    "this is*unformatted* text"
      <==> "this is*unformatted* text"
    "this is *unformatted text"
      <==> "this is *unformatted text"
    "*this* is *unformatted text"
      <==> bold "this" <> " is *unformatted text"
  it "ignored internal markdown" do
    "this is *long _bold_ (not italic)* text"
      <==> "this is " <> bold "long _bold_ (not italic)" <> " text"
    "snippet: `this is *bold text*`"
      <==> "snippet: " <> markdown Snippet "this is *bold text*"

secretText :: Spec
secretText = describe "secret text" do
  it "correct markdown" do
    "this is #black_secret# text"
      <==> "this is " <> markdown Secret "black_secret" <> " text"
    "##black_secret### text"
      <==> markdown Secret "#black_secret##" <> " text"
    "this is #black secret# text"
      <==> "this is " <> markdown Secret "black secret" <> " text"
    "##black secret### text"
      <==> markdown Secret "#black secret##" <> " text"
    "this is #secret#"
      <==> "this is " <> markdown Secret "secret"
    " #secret# text"
      <==> " " <> markdown Secret "secret" <> " text"
    "   #secret# text"
      <==> "   " <> markdown Secret "secret" <> " text"
    "this is #secret# "
      <==> "this is " <> markdown Secret "secret" <> " "
    "this is #secret#   "
      <==> "this is " <> markdown Secret "secret" <> "   "
  it "ignored as markdown" do
    "this is # unformatted # text"
      <==> "this is # unformatted # text"
    "this is #unformatted # text"
      <==> "this is " <> sname NTPublicGroup TLDSimplex "unformatted" [] "unformatted" <> " # text"
    "this is # unformatted# text"
      <==> "this is # unformatted# text"
    "this is ## unformatted ## text"
      <==> "this is ## unformatted ## text"
    "this is#unformatted# text"
      <==> "this is#unformatted# text"
    "this is #unformatted text"
      <==> "this is " <> sname NTPublicGroup TLDSimplex "unformatted" [] "unformatted" <> " text"
    "*this* is #unformatted text"
      <==> bold "this" <> " is " <> sname NTPublicGroup TLDSimplex "unformatted" [] "unformatted" <> " text"
  it "ignored internal markdown" do
    "snippet: `this is #secret_text#`"
      <==> "snippet: " <> markdown Snippet "this is #secret_text#"

small :: Text -> Markdown
small = markdown Small

textSmall :: Spec
textSmall = describe "text small" do
  it "correct markdown" do
    "this is !- small! text"
      <==> "this is " <> small "small" <> " text"
    "!- small! text"
      <==> small "small" <> " text"
    "this is !- small!"
      <==> "this is " <> small "small"
    " !- small! text"
      <==> " " <> small "small" <> " text"
    "this is !- small! "
      <==> "this is " <> small "small" <> " "
  it "ignored as markdown" do
    "this is !- unformatted ! text"
      <==> "this is !- unformatted ! text"
    "this is !-  unformatted! text"
      <==> "this is !-  unformatted! text"
    "this is!- unformatted! text"
      <==> "this is!- unformatted! text"
    "this is !- unformatted text"
      <==> "this is !- unformatted text"
  it "ignored internal markdown" do
    "this is !- long *small* (not bold)! text"
      <==> "this is " <> small "long *small* (not bold)" <> " text"

red :: Text -> Markdown
red = markdown (colored Red)

textColor :: Spec
textColor = describe "text color (red)" do
  it "correct markdown" do
    "this is !1 red color! text"
      <==> "this is " <> red "red color" <> " text"
    "!1 red! text"
      <==> red "red" <> " text"
    "this is !1 red!"
      <==> "this is " <> red "red"
    " !1 red! text"
      <==> " " <> red "red" <> " text"
    "   !1 red! text"
      <==> "   " <> red "red" <> " text"
    "this is !1 red! "
      <==> "this is " <> red "red" <> " "
    "this is !1 red!   "
      <==> "this is " <> red "red" <> "   "
  it "ignored as markdown" do
    "this is !1 unformatted ! text"
      <==> "this is !1 unformatted ! text"
    "this is !1 unformatted ! text"
      <==> "this is !1 unformatted ! text"
    "this is !1  unformatted! text"
      <==> "this is !1  unformatted! text"
    -- "this is !!1 unformatted!! text"
    --   <==> "this is " <> "!!1" <> "unformatted!! text"
    "this is!1 unformatted! text"
      <==> "this is!1 unformatted! text"
    "this is !1 unformatted text"
      <==> "this is !1 unformatted text"
    "*this* is !1 unformatted text"
      <==> bold "this" <> " is !1 unformatted text"
  it "ignored internal markdown" do
    "this is !1 long *red* (not bold)! text"
      <==> "this is " <> red "long *red* (not bold)" <> " text"
    "snippet: `this is !1 red text!`"
      <==> "snippet: " <> markdown Snippet "this is !1 red text!"

uri :: Text -> Markdown
uri = Markdown $ Just Uri

simplexLink :: SimplexLinkType -> Text -> NonEmpty Text -> Text -> Markdown
simplexLink linkType uriText smpHosts t = Markdown (simplexLinkFormat linkType uriText smpHosts Nothing) t

simplexLinkFormat :: SimplexLinkType -> Text -> NonEmpty Text -> Maybe Text -> Maybe Format
simplexLinkFormat linkType uriText smpHosts showText = case strDecode $ encodeUtf8 uriText of
  Right simplexUri -> Just SimplexLink {linkType, simplexUri, smpHosts, showText}
  Left e -> error e

textWithUri :: Spec
textWithUri = describe "text with Uri" do
  it "correct markdown" do
    "https://simplex.chat" <==> uri "https://simplex.chat"
    "https://simplex.chat." <==> uri "https://simplex.chat" <> "."
    "https://simplex.chat, hello" <==> uri "https://simplex.chat" <> ", hello"
    "http://simplex.chat" <==> uri "http://simplex.chat"
    "this is https://simplex.chat" <==> "this is " <> uri "https://simplex.chat"
    "https://simplex.chat site" <==> uri "https://simplex.chat" <> " site"
    "SimpleX on GitHub: https://github.com/simplex-chat/" <==> "SimpleX on GitHub: " <> uri "https://github.com/simplex-chat/"
    "SimpleX on GitHub: https://github.com/simplex-chat." <==> "SimpleX on GitHub: " <> uri "https://github.com/simplex-chat" <> "."
    "https://github.com/simplex-chat/ - SimpleX on GitHub" <==> uri "https://github.com/simplex-chat/" <> " - SimpleX on GitHub"
    "SimpleX on GitHub (https://github.com/simplex-chat/)" <==> "SimpleX on GitHub (" <> uri "https://github.com/simplex-chat/" <> ")"
    "https://en.m.wikipedia.org/wiki/Servo_(software)" <==> uri "https://en.m.wikipedia.org/wiki/Servo_(software)"
    "https://simplex.chat/page_name_" <==> uri "https://simplex.chat/page_name_"
    "https://simplex.chat/page_name_, hello" <==> uri "https://simplex.chat/page_name_" <> ", hello"
    "https://simplex.chat/page!" <==> uri "https://simplex.chat/page!"
    "https://simplex.chat/page!, hello" <==> uri "https://simplex.chat/page!" <> ", hello"
    "example.com" <==> uri "example.com"
    "example.com." <==> uri "example.com" <> "."
    "example.com..." <==> uri "example.com" <> "..."
    "www.example.com" <==> uri "www.example.com"
    "example.academy" <==> uri "example.academy"
    "this is example.com" <==> "this is " <> uri "example.com"
    "x.com" <==> uri "x.com"
  it "links in parentheses" do
    "(https://simplex.chat)" <==> "(" <> uri "https://simplex.chat" <> ")"
    "see (https://simplex.chat)." <==> "see (" <> uri "https://simplex.chat" <> ")."
    "(see https://simplex.chat)" <==> "(see " <> uri "https://simplex.chat" <> ")"
    "(https://en.m.wikipedia.org/wiki/Servo_(software))" <==> "(" <> uri "https://en.m.wikipedia.org/wiki/Servo_(software)" <> ")"
    "https://simplex.chat/page))" <==> uri "https://simplex.chat/page" <> "))"
    "(example.com)" <==> "(" <> uri "example.com" <> ")"
    "(chat@simplex.chat)" <==> "(" <> email "chat@simplex.chat" <> ")"
  it "ignored as markdown" do
    "_https://simplex.chat" <==> "_https://simplex.chat"
    "this is _https://simplex.chat" <==> "this is _https://simplex.chat"
    "this is https://" <==> "this is https://"
    "example.c" <==> "example.c"
    "www.www.example.com" <==> "www.www.example.com"
    "www.example1.com" <==> "www.example1.com"
    "www." <==> "www."
    ".com" <==> ".com"
    "example.academytoolong" <==> "example.academytoolong"
    "simplex:/example" <==> "simplex:/example"
    "end of sentence.Next sentence" <==> "end of sentence.Next sentence"
    "Visit Example.Com today" <==> "Visit Example.Com today"
  it "SimpleX links" do
    let inv = "/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D"
    ("https://simplex.chat" <> inv) <==> simplexLink XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] ("https://simplex.chat" <> inv)
    ("simplex:" <> inv) <==> simplexLink XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] ("simplex:" <> inv)
    ("https://example.com" <> inv) <==> simplexLink XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] ("https://example.com" <> inv)
    let ct = "/contact#/?v=2&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D"
    ("https://simplex.chat" <> ct) <==> simplexLink XLContact ("simplex:" <> ct) ["smp.simplex.im"] ("https://simplex.chat" <> ct)
    ("simplex:" <> ct) <==> simplexLink XLContact ("simplex:" <> ct) ["smp.simplex.im"] ("simplex:" <> ct)
    let gr = "/contact#/?v=2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FWHV0YU1sYlU7NqiEHkHDB6gxO1ofTync%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAWbebOqVYuBXaiqHcXYjEHCpYi6VzDlu6CVaijDTmsQU%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22mL-7Divb94GGmGmRBef5Dg%3D%3D%22%7D"
    ("https://simplex.chat" <> gr) <==> simplexLink XLGroup ("simplex:" <> gr) ["smp4.simplex.im", "o5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"] ("https://simplex.chat" <> gr)
    ("simplex:" <> gr) <==> simplexLink XLGroup ("simplex:" <> gr) ["smp4.simplex.im", "o5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"] ("simplex:" <> gr)

web :: Text -> Text -> Text -> Markdown
web t u = Markdown $ Just HyperLink {showText = Just t, linkUri = u}

textWithHyperlink :: Spec
textWithHyperlink = describe "text with HyperLink without link text" do
  let addr = "https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw"
      addr' = "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
  it "correct markdown" do
    "[click here](https://example.com)" <==> web "click here" "https://example.com" "[click here](https://example.com)"
    "For details [click here](https://example.com)" <==> "For details " <> web "click here" "https://example.com" "[click here](https://example.com)"
    "[example.com](https://example.com)" <==> web "example.com" "https://example.com" "[example.com](https://example.com)"
    "[example.com/page](https://example.com/page)" <==> web "example.com/page" "https://example.com/page" "[example.com/page](https://example.com/page)"
    ("[Connect to me](" <> addr <> ")") <==> Markdown (simplexLinkFormat XLContact addr' ["smp6.simplex.im"] (Just "Connect to me")) ("[Connect to me](" <> addr <> ")")
  it "potentially spoofed link" do
    "[https://example.com](https://another.com)" <==> "[https://example.com](" <> uri "https://another.com" <> ")"
    "[example.com/page](https://another.com/page)" <==> "[example.com/page](" <> uri "https://another.com/page" <> ")"
    ("[Connect.to.me](" <> addr <> ")") <==> "[Connect.to.me](" <> Markdown (simplexLinkFormat XLContact addr' ["smp6.simplex.im"] Nothing) addr <> ")"
    "[What's new](https://simplex.chat)" <==> "[What's new](" <> uri "https://simplex.chat" <> ")"
  it "ignored as markdown" do
    "[click here](example.com)" <==> "[click here](" <> uri "example.com" <> ")"
    "[click here](https://example.com )" <==> "[click here](" <> uri "https://example.com" <> " )"

obfuscatedSimplexLinks :: Spec
obfuscatedSimplexLinks = describe "SimpleX links obfuscated with whitespace" do
  let addr = "https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw"
      inv = "/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D"
  let spaced s = T.replace "://" ":// " s -- insert a space right after the scheme
  it "detects links split with spaces or newlines" do
    hasObfuscatedSimplexLink addr `shouldBe` True
    hasObfuscatedSimplexLink (spaced addr) `shouldBe` True
    hasObfuscatedSimplexLink (T.intercalate "\n" $ T.chunksOf 8 addr) `shouldBe` True
    hasObfuscatedSimplexLink ("connect with me: " <> spaced addr) `shouldBe` True
    hasObfuscatedSimplexLink (T.intercalate " " $ T.chunksOf 8 $ "https://simplex.chat" <> inv) `shouldBe` True
  it "detects a split link followed by other text" do
    hasObfuscatedSimplexLink (spaced addr <> "\nplease connect") `shouldBe` True
  it "ignores text without a SimpleX link" do
    hasObfuscatedSimplexLink "" `shouldBe` False
    hasObfuscatedSimplexLink "hello there, this is a normal message" `shouldBe` False
    hasObfuscatedSimplexLink "see https://example.com/page?ref=123 for details" `shouldBe` False

email :: Text -> Markdown
email = Markdown $ Just Email

textWithEmail :: Spec
textWithEmail = describe "text with Email" do
  it "correct markdown" do
    "chat@simplex.chat" <==> email "chat@simplex.chat"
    "test chat@simplex.chat" <==> "test " <> email "chat@simplex.chat"
    "test chat+123@simplex.chat" <==> "test " <> email "chat+123@simplex.chat"
    "test chat.chat+123@simplex.chat" <==> "test " <> email "chat.chat+123@simplex.chat"
    "chat@simplex.chat test" <==> email "chat@simplex.chat" <> " test"
    "test1 chat@simplex.chat test2" <==> "test1 " <> email "chat@simplex.chat" <> " test2"
    "test chat@simplex.chat." <==> "test " <> email "chat@simplex.chat" <> "."
    "test chat@simplex.chat..." <==> "test " <> email "chat@simplex.chat" <> "..."
  it "ignored as email markdown" do
    "chat @simplex.chat" <==> "chat " <> sname NTContact TLDWeb "simplex.chat" [] "simplex.chat"
    "this is chat @simplex.chat" <==> "this is chat " <> sname NTContact TLDWeb "simplex.chat" [] "simplex.chat"
    "this is chat@ simplex.chat" <==> "this is chat@ " <> uri "simplex.chat"
    "this is chat @ simplex.chat" <==> "this is chat @ " <> uri "simplex.chat"
    "*this* is chat @ simplex.chat" <==> bold "this" <> " is chat @ " <> uri "simplex.chat"

phone :: Text -> Markdown
phone = Markdown $ Just Phone

textWithPhone :: Spec
textWithPhone = describe "text with Phone" do
  it "correct markdown" do
    "07777777777" <==> phone "07777777777"
    "test 07777777777" <==> "test " <> phone "07777777777"
    "07777777777 test" <==> phone "07777777777" <> " test"
    "test1 07777777777 test2" <==> "test1 " <> phone "07777777777" <> " test2"
    "test 07777 777 777 test" <==> "test " <> phone "07777 777 777" <> " test"
    "test +447777777777 test" <==> "test " <> phone "+447777777777" <> " test"
    "test +44 (0) 7777 777 777 test" <==> "test " <> phone "+44 (0) 7777 777 777" <> " test"
    "test +44-7777-777-777 test" <==> "test " <> phone "+44-7777-777-777" <> " test"
    "test +44 (0) 7777.777.777 https://simplex.chat test"
      <==> "test " <> phone "+44 (0) 7777.777.777" <> " " <> uri "https://simplex.chat" <> " test"
  it "ignored as markdown (too short)" $
    "test 077777 test" <==> "test 077777 test"
  it "ignored as markdown (double spaces)" $ do
    "test 07777  777  777 test" <==> "test 07777  777  777 test"
    "*test* 07777  777  777 test" <==> bold "test" <> " 07777  777  777 test"
  it "ignored as markdown (not starting with + or 0)" $ do
    "order 12345678 shipped" <==> "order 12345678 shipped"
    "on 2024-10-29" <==> "on 2024-10-29"
    "server 192.168.1.1" <==> "server 192.168.1.1"
    "version 7.1.0.14" <==> "version 7.1.0.14"
    "total 1 000 000" <==> "total 1 000 000"

mention :: Text -> Text -> Markdown
mention = Markdown . Just . Mention

textWithMentions :: Spec
textWithMentions = describe "text with mentions" do
  it "correct markdown" do
    "@alice" <==> mention "alice" "@alice"
    "hello @alice" <==> "hello " <> mention "alice" "@alice"
    "hello @alice !" <==> "hello " <> mention "alice" "@alice" <> " !"
    "hello @alice!" <==> "hello " <> mention "alice" "@alice" <> "!"
    "hello @alice..." <==> "hello " <> mention "alice" "@alice" <> "..."
    "hello @alice@example.com" <==> "hello " <> mention "alice@example.com" "@alice@example.com"
    "hello @'alice @ example.com'" <==> "hello " <> mention "alice @ example.com" "@'alice @ example.com'"
    "@'alice jones'" <==> mention "alice jones" "@'alice jones'"
    "hello @'alice jones'!" <==> "hello " <> mention "alice jones" "@'alice jones'" <> "!"
    "hello @'a.j.'!" <==> "hello " <> mention "a.j." "@'a.j.'" <> "!"
  it "ignored as markdown" $ do
    "hello @'alice jones!" <==> "hello @'alice jones!"
    "hello @bob @'alice jones!" <==> "hello " <> mention "bob" "@bob" <> " @'alice jones!"
    "hello @ alice!" <==> "hello @ alice!"
    "hello @bob @ alice!" <==> "hello " <> mention "bob" "@bob" <> " @ alice!"
    "hello @bob @" <==> "hello " <> mention "bob" "@bob" <> " @"

command :: Text -> Text -> Markdown
command = Markdown . Just . Command

textWithCommands :: Spec
textWithCommands = describe "text with commands" do
  it "correct markdown" do
    "/start" <==> command "start" "/start"
    "send /help" <==> "send " <> command "help" "/help"
    "send /help !" <==> "send " <> command "help" "/help" <> " !"
    "send /help!" <==> "send " <> command "help" "/help" <> "!"
    "send /help..." <==> "send " <> command "help" "/help" <> "..."
    "send /'filter 1'" <==> "send " <> command "filter 1" "/'filter 1'"
    "/'filter 1'" <==> command "filter 1" "/'filter 1'"
    "/filter 1" <==> command "filter" "/filter" <> " 1" -- this is parsed as full command by parseMaybeMarkdownList
    "send /'filter 1'." <==> "send " <> command "filter 1" "/'filter 1'" <> "."
    "send /'filter 1.'!" <==> "send " <> command "filter 1." "/'filter 1.'" <> "!"
    "send /he?lp" <==> "send " <> command "he?lp" "/he?lp"
    "/-" <==> command "-" "/-"
    "send /+." <==> "send " <> command "+" "/+" <> "."
    "/'+'" <==> command "+" "/'+'"
  it "calculator keys" do
    "/C   /±   /%   /÷" <==> command "C" "/C" <> "   " <> command "±" "/±" <> "   " <> command "%" "/%" <> "   " <> command "÷" "/÷"
    "/√   /0   /.   /=" <==> command "√" "/√" <> "   " <> command "0" "/0" <> "   " <> command "." "/." <> "   " <> command "=" "/="
    "/C `\160\160\160\160`/neg `\160\160`" <==> command "C" "/C" <> " " <> markdown Snippet "\160\160\160\160" <> command "neg" "/neg" <> " " <> markdown Snippet "\160\160"
  it "ignored as markdown" $ do
    "send /'filter 1" <==> "send /'filter 1"
    "send /help /'filter 1" <==> "send " <> command "help" "/help" <> " /'filter 1"
    "send / help!" <==> "send / help!"
    "send /help / filter" <==> "send " <> command "help" "/help" <> " / filter"
    "send /help /" <==> "send " <> command "help" "/help" <> " /"

uri' :: Text -> FormattedText
uri' = FormattedText $ Just Uri

command' :: Text -> Text -> FormattedText
command' = FormattedText . Just . Command

sname :: SimplexNameType -> SimplexTLD -> Text -> [Text] -> Text -> Markdown
sname nt ns dom sub txt = markdown (SimplexName $ SimplexNameInfo nt (SimplexDomain ns dom sub)) (pfx <> txt)
  where
    pfx = case nt of NTPublicGroup -> "#"; NTContact -> "@"

textWithSimplexNames :: Spec
textWithSimplexNames = describe "text with SimpleX names" do
  it "channel names - simplex namespace" do
    "#privacy" <==> sname NTPublicGroup TLDSimplex "privacy" [] "privacy"
    "#privacy.simplex" <==> sname NTPublicGroup TLDSimplex "privacy" [] "privacy.simplex"
    "#my-channel.simplex" <==> sname NTPublicGroup TLDSimplex "my-channel" [] "my-channel.simplex"
    "hello #privacy!" <==> "hello " <> sname NTPublicGroup TLDSimplex "privacy" [] "privacy" <> "!"
    "see #privacy.simplex now" <==> "see " <> sname NTPublicGroup TLDSimplex "privacy" [] "privacy.simplex" <> " now"
  it "channel names - subdomains" do
    "#support.acme.simplex" <==> sname NTPublicGroup TLDSimplex "acme" ["support"] "support.acme.simplex"
    "#a.b.acme.simplex" <==> sname NTPublicGroup TLDSimplex "acme" ["b", "a"] "a.b.acme.simplex"
  it "channel names - testing namespace" do
    "#test.testing" <==> sname NTPublicGroup TLDTesting "test" [] "test.testing"
    "#sub.test.testing" <==> sname NTPublicGroup TLDTesting "test" ["sub"] "sub.test.testing"
  it "channel names - web domains" do
    "#example.com" <==> sname NTPublicGroup TLDWeb "example.com" [] "example.com"
    "#news.bbc.co.uk" <==> sname NTPublicGroup TLDWeb "news.bbc.co.uk" [] "news.bbc.co.uk"
    "#123.com" <==> sname NTPublicGroup TLDWeb "123.com" [] "123.com"
  it "contact names" do
    "@privacy.simplex" <==> sname NTContact TLDSimplex "privacy" [] "privacy.simplex"
    "@my-name.simplex" <==> sname NTContact TLDSimplex "my-name" [] "my-name.simplex"
    "@alice.example.com" <==> sname NTContact TLDWeb "alice.example.com" [] "alice.example.com"
  it "not parsed as names" do
    "#123" <==> "#123"
    "issue #42 is fixed" <==> "issue #42 is fixed"
    "I'm #1!" <==> "I'm #1!"
    "#secret#" <==> markdown Secret "secret"
    "##double secret##" <==> markdown Secret "#double secret#"
    "#" <==> "#"

multilineMarkdownList :: Spec
multilineMarkdownList = describe "multiline markdown" do
  it "correct markdown" do
    "http://simplex.chat\nhttp://app.simplex.chat" <<==>> [uri' "http://simplex.chat", "\n", uri' "http://app.simplex.chat"]
  it "combines the same formats" do
    "http://simplex.chat\ntext 1\ntext 2\nhttp://app.simplex.chat" <<==>> [uri' "http://simplex.chat", "\ntext 1\ntext 2\n", uri' "http://app.simplex.chat"]
  it "no markdown" do
    parseMaybeMarkdownList "not a\nmarkdown" `shouldBe` Nothing
  let inv = "/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D"
  it "multiline with simplex link" do
    ("https://simplex.chat" <> inv <> "\ntext")
      <<==>>
        [ FormattedText (simplexLinkFormat XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] Nothing) ("https://simplex.chat" <> inv),
          "\ntext"
        ]
  it "command markdown" do
    "/link 1" <<==>> [command' "link 1" "/link 1"]
    " /link 1" <<==>> [command' "link 1" " /link 1"]
    "*0*\n/7   /+" <<==>> [FormattedText (Just boldF) "0", "\n", command' "7" "/7", "   ", command' "+" "/+"]

inlineMarkdownGFM :: Spec
inlineMarkdownGFM = describe "inline markdown of GFM" do
  it "strike-through and bold with double delimiters" do
    "~~struck~~ text" ==> markdown StrikeThrough "struck" <> " text"
    "~struck~ text" <==> markdown StrikeThrough "struck" <> " text"
    "__bold__ text" ==> bold2 "bold" <> " text"
    "**a**b" <==> bold2 "a" <> "b"
    "~~ a~~" ==> "~~ a~~"
    "__ a__" ==> "__ a__"
    "a__b__" ==> "a__b__"
    markdownText (fmt boldF "a") `shouldBe` "*a*"
    markdownText (fmt bold2F "a") `shouldBe` "**a**"
  it "underscores inside words, as CommonMark" do
    "_private_var = 1" <==> "_private_var = 1"
    "use _snake_case_ here" <==> "use " <> markdown Italic "snake_case" <> " here"
    "_foo_bar_baz_" <==> markdown Italic "foo_bar_baz"
    "__foo__bar" <==> "__foo__bar"
    "__init__.py" ==> bold2 "init" <> ".py"
  it "a closing delimiter followed by the same character is text" do
    "*a**b*" <==> bold "a**b"
    "*foo**bar**baz*" <==> bold "foo**bar**baz"
    "**a****b**" <==> "**a****b**"
    "*a**" <==> "*a**"
  it "code spans inside a format" do
    "*a `*`*" <==> bold "a `*`"
    "**a `**` b**" <==> bold2 "a `**` b"
  it "code spans" do
    "`` x ` y ``" ==> markdown Snippet "x ` y"
    "``x ` y``" <== markdown Snippet "x ` y"
    "` a `" ==> markdown Snippet "a"
    "` a`" <==> markdown Snippet " a"
    "` `" <==> markdown Snippet " "
    "`  a  `" <==> markdown Snippet " a "
    "``a``" ==> markdown Snippet "a"
    "```a```" ==> markdown Snippet "a"
    "``` a ```" ==> markdown Snippet "a"
    "`` a`" <==> "`` a`"
    "`a``b`" ==> markdown Snippet "a``b"
    "```a``b```" <== markdown Snippet "a``b"
    markdownText (fmt Snippet "`a") `shouldBe` "`` `a ``"
  it "autolinks" do
    "<https://simplex.chat>" <==> web "https://simplex.chat" "https://simplex.chat" "<https://simplex.chat>"
    "mail <chat@simplex.chat>" <==> "mail " <> web "chat@simplex.chat" "mailto:chat@simplex.chat" "<chat@simplex.chat>"
    "<not a link>" <==> "<not a link>"
    "<https://simplex.chat" <==> "<https://simplex.chat"
    "<ftp://simplex.chat>" <==> "<ftp://simplex.chat>"

backslashEscapes :: Spec
backslashEscapes = describe "backslash escapes" do
  it "an escaped ASCII punctuation character is text" do
    "\\*not bold*" <==> markdown Escape "\\" <> "*not bold*"
    "a\\_b_" <==> "a" <> markdown Escape "\\" <> "_b_"
    "\\\\" <==> markdown Escape "\\" <> "\\"
    "\\`a`" <==> markdown Escape "\\" <> "`a`"
  it "a backslash before other characters is text" do
    "\\a" <==> "\\a"
    "\\ a" <==> "\\ a"
    "a\\" <==> "a\\"
    "C:\\dir\\file.txt" <==> "C:\\dir\\file.txt"
  it "escapes are text in code" do
    "`\\*`" <==> markdown Snippet "\\*"
    "```\n\\*a\n```" <===> ([fence "```", hardBr, "\\*a", hardBr, fence "```"], [CodeBlock Nothing (rng 2 3)])
  it "escaped block syntax is text" do
    "\\# a\n- b" <===> ([fmt Escape "\\", "# a", lineBr, bullet "- ", "b"], [Paragraph (rng 0 2), List [entry 4 5 []]])
    "\\- a\n- b" <===> ([fmt Escape "\\", "- a", lineBr, bullet "- ", "b"], [Paragraph (rng 0 2), List [entry 4 5 []]])
    "\\> a\n- b" <===> ([fmt Escape "\\", "> a", lineBr, bullet "- ", "b"], [Paragraph (rng 0 2), List [entry 4 5 []]])
    "1\\. a\n- b" <===> (["1", fmt Escape "\\", ". a", lineBr, bullet "- ", "b"], [Paragraph (rng 0 3), List [entry 5 6 []]])
  it "escapes are kept in texts without blocks" do
    "\\*a*" <<==>> [fmt Escape "\\", "*a*"]
    "\\*a*\nb" <<==>> [fmt Escape "\\", "*a*\nb"]

textsWithoutBlocks :: Spec
textsWithoutBlocks = describe "texts without blocks, as on master" do
  it "no format" do
    parseMaybeMarkdownList "a\nb" `shouldBe` Nothing
    parseMaybeMarkdownList "a\n\nb" `shouldBe` Nothing
    parseMaybeMarkdownList "a\n" `shouldBe` Nothing
    parseMaybeMarkdownList "a  \nb" `shouldBe` Nothing
    parseMaybeMarkdownList "\n\na" `shouldBe` Nothing
  it "line breaks are text" do
    "*a*\n\nb" <<==>> [fmt boldF "a", "\n\nb"]
    "*a*\nb" <<==>> [fmt boldF "a", "\nb"]
    "*a*\r\nb" <<==>> [fmt boldF "a", "\r\nb"]
    "*a*  \nb" <<==>> [fmt boldF "a", "  \nb"]
    "*a*\\\nb" <<==>> [fmt boldF "a", "\\\nb"]
    "  a\n*b*" <<==>> ["  a\n", fmt boldF "b"]
    "\n\n*a*" <<==>> ["\n\n", fmt boldF "a"]
  it "the final line break is removed" do
    "*a*\n" ==>> [fmt boldF "a"]
    "*a*\r\n" ==>> [fmt boldF "a"]
    "*a*\n\n" ==>> [fmt boldF "a", "\n"]

infix 1 ===>, <===>

(===>) :: Text -> (MarkdownList, [MarkdownBlock]) -> Expectation
s ===> (ft, blocks) = do
  let ParsedMarkdown ft' blocks' = parsedMarkdown s
  (ft', blocks') `shouldBe` (Just ft, L.nonEmpty blocks)
  rangesCoverContent ft blocks

(<===>) :: Text -> (MarkdownList, [MarkdownBlock]) -> Expectation
s <===> parsed@(ft, _) = (s ===> parsed) >> (withoutFinalBreak <<== ft)
  where
    withoutFinalBreak = fromMaybe s $ T.stripSuffix "\r\n" s <|> T.stripSuffix "\n" s

noBlocks :: Text -> Expectation
noBlocks s = let ParsedMarkdown _ blocks = parsedMarkdown s in blocks `shouldBe` Nothing

rangesCoverContent :: MarkdownList -> [MarkdownBlock] -> Expectation
rangesCoverContent ft blocks = do
  ranges `shouldSatisfy` all (\(SegmentRange a b) -> 0 <= a && a <= b && b <= length ft)
  zip ranges (drop 1 ranges) `shouldSatisfy` all (\(SegmentRange _ b, SegmentRange a _) -> b <= a)
  [i | SegmentRange a b <- ranges, i <- [a .. b - 1], i `elem` contentIndices] `shouldBe` contentIndices
  where
    ranges = concatMap blockRanges blocks
    contentIndices = [i | (i, FormattedText f _) <- zip [0 ..] ft, not (isMarkup f)]
    blockRanges = \case
      Paragraph r -> [r]
      Heading _ r -> [r]
      CodeBlock _ r -> [r]
      Quote r -> [r]
      List entries -> concatMap entryRanges entries
      Table _ headerCells bodyRows -> headerCells <> concat bodyRows
      Rule r -> [r]
      UnknownBlock _ -> []
    entryRanges (ListEntry _ r children) = r : concatMap entryRanges children

fmt :: Format -> Text -> FormattedText
fmt = FormattedText . Just

mk :: BlockToken -> Text -> FormattedText
mk = markupText

lineBr :: FormattedText
lineBr = mk LineBreak "\n"

hardBr :: FormattedText
hardBr = mk HardBreak "\n"

heading1 :: FormattedText
heading1 = mk (HeadingMark 1) "# "

bullet :: Text -> FormattedText
bullet = mk (ListMark Nothing)

numbered :: Text -> FormattedText
numbered n = mk (ListMark $ Just n) (n <> " ")

quoteMark :: Text -> FormattedText
quoteMark = mk QuoteMark

fence :: Text -> FormattedText
fence = mk (FenceMark Nothing)

rng :: Int -> Int -> SegmentRange
rng = SegmentRange

entry :: Int -> Int -> [ListEntry] -> ListEntry
entry a b = ListEntry Nothing (rng a b)

blockMarkdown :: Spec
blockMarkdown = describe "block markdown" do
  it "blocks and their ranges" do
    "# Plan\n- buy *milk*\n  and eggs\n  - nested\n\n> a\n> b"
      <===> ( [ mk (HeadingMark 1) "# ",
                "Plan",
                mk LineBreak "\n",
                mk (ListMark Nothing) "- ",
                "buy ",
                FormattedText (Just boldF) "milk",
                mk HardBreak "\n",
                mk Indent "  ",
                "and eggs",
                mk LineBreak "\n",
                mk (ListMark Nothing) "  - ",
                "nested",
                mk (ParagraphBreak 2) "\n\n",
                mk QuoteMark "> ",
                "a",
                mk HardBreak "\n",
                mk QuoteMark "> ",
                "b"
              ],
              [Heading 1 (rng 1 2), List [entry 4 9 [entry 11 12 []]], Quote (rng 14 18)]
            )

breaksInBlocks :: Spec
breaksInBlocks = describe "line breaks in texts with blocks" do
  it "line breaks in paragraphs, list items and quotes" do
    "a\nb\nc\n# d" <===> (["a", hardBr, "b", hardBr, "c", lineBr, heading1, "d"], [Paragraph (rng 0 5), Heading 1 (rng 7 8)])
    "  a\n  b\n# c" <===> (["  a", hardBr, "  b", lineBr, heading1, "c"], [Paragraph (rng 0 3), Heading 1 (rng 5 6)])
    "- a\n  b" <===> ([bullet "- ", "a", hardBr, mk Indent "  ", "b"], [List [entry 1 5 []]])
    "> a\n> b" <===> ([quoteMark "> ", "a", hardBr, quoteMark "> ", "b"], [Quote (rng 1 5)])
  it "hard breaks by two or more spaces and by a backslash" do
    "a  \nb\n- c" <===> (["a", mk HardBreak "  \n", "b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
    "a   \nb\n- c" <===> (["a", mk HardBreak "   \n", "b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
    "a\\\nb\n- c" <===> (["a", mk HardBreak "\\\n", "b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
    "a\\\\\\\nb\n- c" <===> (["a", fmt Escape "\\", "\\", mk HardBreak "\\\n", "b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 5), List [entry 7 8 []]])
    "- a  \n  b" <===> ([bullet "- ", "a", mk HardBreak "  \n", mk Indent "  ", "b"], [List [entry 1 5 []]])
    "- a\\\n  b" <===> ([bullet "- ", "a", mk HardBreak "\\\n", mk Indent "  ", "b"], [List [entry 1 5 []]])
    "> a  \n> b" <===> ([quoteMark "> ", "a", mk HardBreak "  \n", quoteMark "> ", "b"], [Quote (rng 1 5)])
    "> a\\\n> b" <===> ([quoteMark "> ", "a", mk HardBreak "\\\n", quoteMark "> ", "b"], [Quote (rng 1 5)])
  it "spaces and backslashes that are content" do
    "a \nb\n- c" <===> (["a ", hardBr, "b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
    "a\\\\\nb\n- c" <===> (["a", fmt Escape "\\", "\\", hardBr, "b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 5), List [entry 7 8 []]])
    "a  \n\n- c" <===> (["a  ", mk (ParagraphBreak 2) "\n\n", bullet "- ", "c"], [Paragraph (rng 0 1), List [entry 3 4 []]])
    "a\\\n\n- c" <===> (["a\\", mk (ParagraphBreak 2) "\n\n", bullet "- ", "c"], [Paragraph (rng 0 1), List [entry 3 4 []]])
    "a  \n- c" <===> (["a  ", lineBr, bullet "- ", "c"], [Paragraph (rng 0 1), List [entry 3 4 []]])
    "# h\na  " <===> ([heading1, "h", lineBr, "a  "], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
    "# h\na\\" <===> ([heading1, "h", lineBr, "a\\"], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
  it "line and paragraph breaks between blocks" do
    "## a\nb" <===> ([mk (HeadingMark 2) "## ", "a", lineBr, "b"], [Heading 2 (rng 1 2), Paragraph (rng 3 4)])
    "# a\n\nb" <===> ([heading1, "a", mk (ParagraphBreak 2) "\n\n", "b"], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
    "# a\n\n\nb" <===> ([heading1, "a", mk (ParagraphBreak 3) "\n\n\n", "b"], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
    "# a\n  \n\t\nb" <===> ([heading1, "a", mk (ParagraphBreak 3) "\n  \n\t\n", "b"], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
  it "blank lines at the text start and end" do
    "\n\n# a" <===> ([mk (ParagraphBreak 2) "\n\n", heading1, "a"], [Heading 1 (rng 2 3)])
    "\n \n# a" <===> ([mk (ParagraphBreak 2) "\n \n", heading1, "a"], [Heading 1 (rng 2 3)])
    "# a\n\n" <===> ([heading1, "a", lineBr], [Heading 1 (rng 1 2)])
    "# a\n\n\n" <===> ([heading1, "a", mk (ParagraphBreak 2) "\n\n"], [Heading 1 (rng 1 2)])
    "# a\n  " <===> ([heading1, "a", mk (ParagraphBreak 1) "\n  "], [Heading 1 (rng 1 2)])
  it "CR before line breaks" do
    "a\r\nb\r\n- c" <===> (["a", mk HardBreak "\r\n", "b", mk LineBreak "\r\n", bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
    "a  \r\nb\r\n- c" <===> (["a", mk HardBreak "  \r\n", "b", mk LineBreak "\r\n", bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
    "# a\r\n- b" <===> ([heading1, "a", mk LineBreak "\r\n", bullet "- ", "b"], [Heading 1 (rng 1 2), List [entry 4 5 []]])
    "# a\r\n\r\nb" <===> ([heading1, "a", mk (ParagraphBreak 2) "\r\n\r\n", "b"], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
    "- a\r\n  b" <===> ([bullet "- ", "a", mk HardBreak "\r\n", mk Indent "  ", "b"], [List [entry 1 5 []]])
    "> a\r\n> b" <===> ([quoteMark "> ", "a", mk HardBreak "\r\n", quoteMark "> ", "b"], [Quote (rng 1 5)])
    "a\r\n===" <===> (["a", mk HardBreak "\r\n", mk (HeadingUnderline 1) "==="], [Heading 1 (rng 0 1)])
    "```\r\na\r\n```" <===> ([fence "```", mk HardBreak "\r\n", "a", mk HardBreak "\r\n", fence "```"], [CodeBlock Nothing (rng 2 3)])
    "|a|\r\n|-|\r\n|b|" <===> ([mk RowMark "|", "a", mk CellMark "|", mk HardBreak "\r\n", mk (DelimiterRow [TANone]) "|-|", mk HardBreak "\r\n", mk RowMark "|", "b", mk CellMark "|"], [Table [TANone] [rng 1 2] [[rng 7 8]]])

headings :: Spec
headings = describe "headings" do
  it "levels 1 to 6" do
    forM_ ([1 .. 6] :: [Int]) $ \n -> do
      let hashes = T.replicate n "#"
      (hashes <> " a") <===> ([mk (HeadingMark n) (hashes <> " "), "a"], [Heading n (rng 1 2)])
    noBlocks "####### a"
  it "a space and text after the marker" do
    noBlocks "#"
    noBlocks "# "
    noBlocks "#a"
    noBlocks "#\ta"
    noBlocks "#\tx#"
    "#\n- b" <===> (["#", lineBr, bullet "- ", "b"], [Paragraph (rng 0 1), List [entry 3 4 []]])
  it "indentation and inline content" do
    "  # a" <===> ([mk (HeadingMark 1) "  # ", "a"], [Heading 1 (rng 1 2)])
    "#   a" <===> ([mk (HeadingMark 1) "#   ", "a"], [Heading 1 (rng 1 2)])
    "# *a* b" <===> ([heading1, fmt boldF "a", " b"], [Heading 1 (rng 1 3)])
  it "closing sequences" do
    "# a #" <===> ([heading1, "a", mk HeadingClose " #"], [Heading 1 (rng 1 2)])
    "# a ##  " <===> ([heading1, "a", mk HeadingClose " ##  "], [Heading 1 (rng 1 2)])
    "# a #\nb" <===> ([heading1, "a", mk HeadingClose " #", lineBr, "b"], [Heading 1 (rng 1 2), Paragraph (rng 4 5)])
    "# a#" <===> ([heading1, "a#"], [Heading 1 (rng 1 2)])
    "# a \\#" <===> ([heading1, "a ", fmt Escape "\\", "#"], [Heading 1 (rng 1 4)])
  it "one line, after and before other blocks" do
    "a\n# b" <===> (["a", lineBr, heading1, "b"], [Paragraph (rng 0 1), Heading 1 (rng 3 4)])
    "# a\n# b" <===> ([heading1, "a", lineBr, heading1, "b"], [Heading 1 (rng 1 2), Heading 1 (rng 4 5)])
  it "setext headings" do
    "a\n===" <===> (["a", hardBr, mk (HeadingUnderline 1) "==="], [Heading 1 (rng 0 1)])
    "a\nb\n=" <===> (["a", hardBr, "b", hardBr, mk (HeadingUnderline 1) "="], [Heading 1 (rng 0 3)])
    "a\n  ===  " <===> (["a", hardBr, mk (HeadingUnderline 1) "  ===  "], [Heading 1 (rng 0 1)])
  it "dashes under text are a rule, not an underline" do
    "a\n---" <===> (["a", lineBr, mk RuleMark "---"], [Paragraph (rng 0 1), Rule (rng 2 3)])
    "a\nb\n---" <===> (["a", hardBr, "b", lineBr, mk RuleMark "---"], [Paragraph (rng 0 3), Rule (rng 4 5)])
    "a\n---\nb" <===> (["a", lineBr, mk RuleMark "---", lineBr, "b"], [Paragraph (rng 0 1), Rule (rng 2 3), Paragraph (rng 4 5)])
    noBlocks "a\n-"
  it "underlines only under paragraphs" do
    "a\n- - -" <===> (["a", lineBr, mk RuleMark "- - -"], [Paragraph (rng 0 1), Rule (rng 2 3)])
    "a\n***" <===> (["a", lineBr, mk RuleMark "***"], [Paragraph (rng 0 1), Rule (rng 2 3)])
    "# a\n===" <===> ([heading1, "a", lineBr, "==="], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
    "> a\n===" <===> ([quoteMark "> ", "a", lineBr, "==="], [Quote (rng 1 2), Paragraph (rng 3 4)])
    "- a\n===" <===> ([bullet "- ", "a", lineBr, "==="], [List [entry 1 2 []], Paragraph (rng 3 4)])
    noBlocks "a\n= ="
    noBlocks "a\n\n==="
    noBlocks "==="
    noBlocks "===\na"

thematicBreaks :: Spec
thematicBreaks = describe "thematic breaks" do
  it "3 or more of one character, with spaces between and around" do
    forM_ (["---", "***", "___", "- - -", "* * *", "_ _ _", "-- -", "  ***", "***  ", "----------"] :: [Text]) $ \r ->
      r <===> ([mk RuleMark r], [Rule (rng 0 1)])
  it "other lines are text" do
    noBlocks "--"
    noBlocks "*-*"
    noBlocks "---x"
    noBlocks "-- a"
  it "between other blocks" do
    "a\n***\nb" <===> (["a", lineBr, mk RuleMark "***", lineBr, "b"], [Paragraph (rng 0 1), Rule (rng 2 3), Paragraph (rng 4 5)])
    "- a\n---" <===> ([bullet "- ", "a", lineBr, mk RuleMark "---"], [List [entry 1 2 []], Rule (rng 3 4)])

lists :: Spec
lists = describe "lists" do
  it "bullet markers" do
    forM_ (["- ", "* ", "+ "] :: [Text]) $ \m -> (m <> "a") <===> ([bullet m, "a"], [List [entry 1 2 []]])
    "- a\n* b\n+ c" <===> ([bullet "- ", "a", lineBr, bullet "* ", "b", lineBr, bullet "+ ", "c"], [List [entry 1 2 [], entry 4 5 [], entry 7 8 []]])
  it "numbers as written" do
    "1. a\n2) b" <===> ([numbered "1.", "a", lineBr, numbered "2)", "b"], [List [ListEntry (Just "1.") (rng 1 2) [], ListEntry (Just "2)") (rng 4 5) []]])
    "3. a\n7. b" <===> ([numbered "3.", "a", lineBr, numbered "7.", "b"], [List [ListEntry (Just "3.") (rng 1 2) [], ListEntry (Just "7.") (rng 4 5) []]])
    "123456789. a" <===> ([numbered "123456789.", "a"], [List [ListEntry (Just "123456789.") (rng 1 2) []]])
  it "markers without a space and text are text" do
    noBlocks "-a"
    noBlocks "- "
    noBlocks "1.a"
    noBlocks "1. "
    noBlocks "1234567890. a"
    noBlocks "a. b"
  it "after a paragraph line, only a list numbered 1 starts" do
    "a\n1. b" <===> (["a", lineBr, numbered "1.", "b"], [Paragraph (rng 0 1), List [ListEntry (Just "1.") (rng 3 4) []]])
    "a\n1) b" <===> (["a", lineBr, numbered "1)", "b"], [Paragraph (rng 0 1), List [ListEntry (Just "1)") (rng 3 4) []]])
    noBlocks "a\n14. b"
    "a\n14. b\n- c" <===> (["a", hardBr, "14. b", lineBr, bullet "- ", "c"], [Paragraph (rng 0 3), List [entry 5 6 []]])
  it "nesting by the column of the marker" do
    "- a\n  - b" <===> ([bullet "- ", "a", lineBr, bullet "  - ", "b"], [List [entry 1 2 [entry 4 5 []]]])
    "1. a\n   - b" <===> ([numbered "1.", "a", lineBr, bullet "   - ", "b"], [List [ListEntry (Just "1.") (rng 1 2) [entry 4 5 []]]])
    "- a\n    - b" <===> ([bullet "- ", "a", lineBr, bullet "    - ", "b"], [List [entry 1 2 [entry 4 5 []]]])
    "- a\n\t- b" <===> ([bullet "- ", "a", lineBr, bullet "\t- ", "b"], [List [entry 1 2 [entry 4 5 []]]])
    "- a\n  - b\n- c" <===> ([bullet "- ", "a", lineBr, bullet "  - ", "b", lineBr, bullet "- ", "c"], [List [entry 1 2 [entry 4 5 []], entry 7 8 []]])
    "- a\n    - b\n  - c" <===> ([bullet "- ", "a", lineBr, bullet "    - ", "b", lineBr, bullet "  - ", "c"], [List [entry 1 2 [entry 4 5 [], entry 7 8 []]]])
    "  - a\n- b" <===> ([bullet "  - ", "a", lineBr, bullet "- ", "b"], [List [entry 1 2 [], entry 4 5 []]])
  it "depth at most 7" do
    let ks = [1 .. 8] :: [Int]
        marker k = T.replicate (2 * (k - 1)) " " <> "- "
        e k = entry (3 * k - 2) (3 * k - 1)
    T.intercalate "\n" [marker k <> T.pack (show k) | k <- ks]
      <===> ( intercalate [lineBr] [[bullet (marker k), FormattedText Nothing (T.pack $ show k)] | k <- ks],
              [List [e 1 [e 2 [e 3 [e 4 [e 5 [e 6 [e 7 [], e 8 []]]]]]]]]
            )
  it "continuation lines" do
    "- a\n  b" <===> ([bullet "- ", "a", hardBr, mk Indent "  ", "b"], [List [entry 1 5 []]])
    "- a\n    b" <===> ([bullet "- ", "a", hardBr, mk Indent "    ", "b"], [List [entry 1 5 []]])
    "- a\n\tb" <===> ([bullet "- ", "a", hardBr, mk Indent "\t", "b"], [List [entry 1 5 []]])
    "- a\n  b\n  - c" <===> ([bullet "- ", "a", hardBr, mk Indent "  ", "b", lineBr, bullet "  - ", "c"], [List [entry 1 5 [entry 7 8 []]]])
  it "a line without indentation after a list item is a paragraph" do
    "- a\nb" <===> ([bullet "- ", "a", lineBr, "b"], [List [entry 1 2 []], Paragraph (rng 3 4)])
    "- a\nb\n- c" <===> ([bullet "- ", "a", lineBr, "b", lineBr, bullet "- ", "c"], [List [entry 1 2 []], Paragraph (rng 3 4), List [entry 6 7 []]])
    "- *a* b" <===> ([bullet "- ", fmt boldF "a", " b"], [List [entry 1 3 []]])
  it "links and formats in list items" do
    "- https://simplex.chat" <===> ([bullet "- ", uri' "https://simplex.chat"], [List [entry 1 2 []]])
    "1. [click here](https://simplex.chat)" <===> ([numbered "1.", fmt (HyperLink (Just "click here") "https://simplex.chat") "[click here](https://simplex.chat)"], [List [ListEntry (Just "1.") (rng 1 2) []]])
    "- *a* _b_ ~c~ `d` #e# @alice" <===> ([bullet "- ", fmt boldF "a", " ", fmt Italic "b", " ", fmt StrikeThrough "c", " ", fmt Snippet "d", " ", fmt Secret "e", " ", fmt (Mention "alice") "@alice"], [List [entry 1 12 []]])
    "- SimpleX (https://simplex.chat)" <===> ([bullet "- ", "SimpleX (", uri' "https://simplex.chat", ")"], [List [entry 1 4 []]])
    "- [What's new](https://simplex.chat)" <===> ([bullet "- ", "[What's new](", uri' "https://simplex.chat", ")"], [List [entry 1 4 []]])
  it "blank lines between items" do
    "- a\n\n- b" <===> ([bullet "- ", "a", mk (ParagraphBreak 2) "\n\n", bullet "- ", "b"], [List [entry 1 2 [], entry 4 5 []]])
    "- a\n\n\n- b" <===> ([bullet "- ", "a", mk (ParagraphBreak 3) "\n\n\n", bullet "- ", "b"], [List [entry 1 2 [], entry 4 5 []]])
  it "lists and other blocks" do
    "- a\n\nb" <===> ([bullet "- ", "a", mk (ParagraphBreak 2) "\n\n", "b"], [List [entry 1 2 []], Paragraph (rng 3 4)])
    "a\n- b" <===> (["a", lineBr, bullet "- ", "b"], [Paragraph (rng 0 1), List [entry 3 4 []]])
    "# a\n- b" <===> ([heading1, "a", lineBr, bullet "- ", "b"], [Heading 1 (rng 1 2), List [entry 4 5 []]])
    "- a\n# b" <===> ([bullet "- ", "a", lineBr, heading1, "b"], [List [entry 1 2 []], Heading 1 (rng 4 5)])
    "- a\n> b" <===> ([bullet "- ", "a", lineBr, quoteMark "> ", "b"], [List [entry 1 2 []], Quote (rng 4 5)])
    "- a\n# h\n- b" <===> ([bullet "- ", "a", lineBr, heading1, "h", lineBr, bullet "- ", "b"], [List [entry 1 2 []], Heading 1 (rng 4 5), List [entry 7 8 []]])

quotes :: Spec
quotes = describe "quotes" do
  it "quote marks" do
    "> a" <===> ([quoteMark "> ", "a"], [Quote (rng 1 2)])
    ">\ta" <===> ([quoteMark ">\t", "a"], [Quote (rng 1 2)])
    ">  a" <===> ([quoteMark "> ", " a"], [Quote (rng 1 2)])
    "  > a" <===> ([quoteMark "  > ", "a"], [Quote (rng 1 2)])
  it "a quote mark without a space or a tab after it is text" do
    noBlocks ">a"
    noBlocks ">_< sorry"
    ">a\n> b" <===> ([">a", lineBr, quoteMark "> ", "b"], [Paragraph (rng 0 1), Quote (rng 3 4)])
  it "quote marks without text are text" do
    noBlocks ">"
    noBlocks "> "
    noBlocks ">\n>"
  it "block syntax inside a quote is text" do
    "> > b\nc" <===> ([quoteMark "> ", "> b", lineBr, "c"], [Quote (rng 1 2), Paragraph (rng 3 4)])
    "> # a" <===> ([quoteMark "> ", "# a"], [Quote (rng 1 2)])
    "> - a" <===> ([quoteMark "> ", "- a"], [Quote (rng 1 2)])
  it "blank quote lines" do
    "> a\n>\n> b" <===> ([quoteMark "> ", "a", hardBr, quoteMark ">", hardBr, quoteMark "> ", "b"], [Quote (rng 1 7)])
    "> a\n>\n>\n> b" <===> ([quoteMark "> ", "a", hardBr, quoteMark ">", hardBr, quoteMark ">", hardBr, quoteMark "> ", "b"], [Quote (rng 1 9)])
    "> a\n>  \n> b" <===> ([quoteMark "> ", "a", hardBr, quoteMark ">  ", hardBr, quoteMark "> ", "b"], [Quote (rng 1 7)])
    ">\n> a\n>" <===> ([quoteMark ">", hardBr, quoteMark "> ", "a", hardBr, quoteMark ">"], [Quote (rng 3 4)])
  it "a line without a quote mark after a quote line is a paragraph" do
    "> a\nb" <===> ([quoteMark "> ", "a", lineBr, "b"], [Quote (rng 1 2), Paragraph (rng 3 4)])
    "> a\nb\n> c" <===> ([quoteMark "> ", "a", lineBr, "b", lineBr, quoteMark "> ", "c"], [Quote (rng 1 2), Paragraph (rng 3 4), Quote (rng 6 7)])
    "> a\nb\n>\n> c" <===> ([quoteMark "> ", "a", lineBr, "b", lineBr, quoteMark ">", hardBr, quoteMark "> ", "c"], [Quote (rng 1 2), Paragraph (rng 3 4), Quote (rng 8 9)])
    "> a\n>\nb" <===> ([quoteMark "> ", "a", hardBr, quoteMark ">", lineBr, "b"], [Quote (rng 1 2), Paragraph (rng 5 6)])
  it "quotes and other blocks" do
    "> a\n\nb" <===> ([quoteMark "> ", "a", mk (ParagraphBreak 2) "\n\n", "b"], [Quote (rng 1 2), Paragraph (rng 3 4)])
    "> a\n\n> b" <===> ([quoteMark "> ", "a", mk (ParagraphBreak 2) "\n\n", quoteMark "> ", "b"], [Quote (rng 1 2), Quote (rng 4 5)])
    "> a\n- b" <===> ([quoteMark "> ", "a", lineBr, bullet "- ", "b"], [Quote (rng 1 2), List [entry 4 5 []]])
    "> a\n# b" <===> ([quoteMark "> ", "a", lineBr, heading1, "b"], [Quote (rng 1 2), Heading 1 (rng 4 5)])
    "> a\nb | c\n-|-"
      <===> ( [quoteMark "> ", "a", lineBr, mk RowMark "", "b", mk CellMark " | ", "c", mk CellMark "", hardBr, mk (DelimiterRow [TANone, TANone]) "-|-"],
              [Quote (rng 1 2), Table [TANone, TANone] [rng 4 5, rng 6 7] []]
            )
  it "quotes after paragraph lines" do
    "a\n> b" <===> (["a", lineBr, quoteMark "> ", "b"], [Paragraph (rng 0 1), Quote (rng 3 4)])
    "a\n>\n> b" <===> (["a", lineBr, quoteMark ">", hardBr, quoteMark "> ", "b"], [Paragraph (rng 0 1), Quote (rng 5 6)])
    "a\n>\n>\n> b" <===> (["a", hardBr, ">", lineBr, quoteMark ">", hardBr, quoteMark "> ", "b"], [Paragraph (rng 0 3), Quote (rng 7 8)])
    parseMaybeMarkdownList (T.intercalate "\n" $ "a" : replicate 7000 ">") `shouldBe` Nothing

codeBlocks :: Spec
codeBlocks = describe "code blocks" do
  it "fences and languages" do
    "```\nx *y*\n```" <===> ([fence "```", hardBr, "x *y*", hardBr, fence "```"], [CodeBlock Nothing (rng 2 3)])
    "~~~\na\n\nb\n~~~" <===> ([fence "~~~", hardBr, "a", hardBr, hardBr, "b", hardBr, fence "~~~"], [CodeBlock Nothing (rng 2 6)])
    "```bash\necho 1" <===> ([mk (FenceMark $ Just "bash") "```bash", hardBr, "echo 1"], [CodeBlock (Just "bash") (rng 2 3)])
    "```bash script\nx\n```" <===> ([mk (FenceMark $ Just "bash") "```bash script", hardBr, "x", hardBr, fence "```"], [CodeBlock (Just "bash") (rng 2 3)])
    "``` bash\nx\n```" <===> ([mk (FenceMark $ Just "bash") "``` bash", hardBr, "x", hardBr, fence "```"], [CodeBlock (Just "bash") (rng 2 3)])
    "~~~ a`b\nc\n~~~" <===> ([mk (FenceMark $ Just "a`b") "~~~ a`b", hardBr, "c", hardBr, fence "~~~"], [CodeBlock (Just "a`b") (rng 2 3)])
    noBlocks "``` a`b\nc\n```"
  it "closing fences" do
    "```\na\n~~~\nb\n```" <===> ([fence "```", hardBr, "a", hardBr, "~~~", hardBr, "b", hardBr, fence "```"], [CodeBlock Nothing (rng 2 7)])
    "```\na\n```  " <===> ([fence "```", hardBr, "a", hardBr, fence "```  "], [CodeBlock Nothing (rng 2 3)])
    "```\na\n`````" <===> ([fence "```", hardBr, "a", hardBr, fence "`````"], [CodeBlock Nothing (rng 2 3)])
    "````\na\n```\n````" <===> ([fence "````", hardBr, "a", hardBr, "```", hardBr, fence "````"], [CodeBlock Nothing (rng 2 5)])
    "```\na\n``` b" <===> ([fence "```", hardBr, "a", hardBr, "``` b"], [CodeBlock Nothing (rng 2 5)])
  it "blank and indented code lines" do
    "```\n\na\n\n```" <===> ([fence "```", hardBr, hardBr, "a", hardBr, hardBr, fence "```"], [CodeBlock Nothing (rng 2 5)])
    "```\na\n\n```" <===> ([fence "```", hardBr, "a", hardBr, hardBr, fence "```"], [CodeBlock Nothing (rng 2 4)])
    "  ```\n    a\n  ```" <===> ([fence "  ```", hardBr, mk Indent "  ", "  a", hardBr, fence "  ```"], [CodeBlock Nothing (rng 2 4)])
    "  ```\n\ta\n  ```" <===> ([fence "  ```", hardBr, mk Indent "\t", "a", hardBr, fence "  ```"], [CodeBlock Nothing (rng 2 4)])
    "   ```\n a\n```" <===> ([fence "   ```", hardBr, mk Indent " ", "a", hardBr, fence "```"], [CodeBlock Nothing (rng 2 4)])
  it "code is text" do
    "```\n*a* `b` @c https://simplex.chat\n```" <===> ([fence "```", hardBr, "*a* `b` @c https://simplex.chat", hardBr, fence "```"], [CodeBlock Nothing (rng 2 3)])
    "```\na  \nb\\\n```" <===> ([fence "```", hardBr, "a  ", hardBr, "b\\", hardBr, fence "```"], [CodeBlock Nothing (rng 2 5)])
  it "a fence followed by a line is a code block" do
    "```\n```" <===> ([fence "```", hardBr, fence "```"], [CodeBlock Nothing (rng 2 2)])
    "```\n```\nx" <===> ([fence "```", hardBr, fence "```", lineBr, "x"], [CodeBlock Nothing (rng 2 2), Paragraph (rng 4 5)])
    "```\n\n```" <===> ([fence "```", hardBr, hardBr, fence "```"], [CodeBlock Nothing (rng 2 2)])
    "```\n   \n```" <===> ([fence "```", hardBr, "   ", hardBr, fence "```"], [CodeBlock Nothing (rng 2 3)])
    "```\n\n" <===> ([fence "```", hardBr], [CodeBlock Nothing (rng 2 2)])
  it "a fence line at the text end is text" do
    noBlocks "```"
    noBlocks "```\n"
    noBlocks "a\n```"
    "a\n```\nb" <===> (["a", lineBr, fence "```", hardBr, "b"], [Paragraph (rng 0 1), CodeBlock Nothing (rng 4 5)])
  it "code blocks and other blocks" do
    "a\n```\nb\n```" <===> (["a", lineBr, fence "```", hardBr, "b", hardBr, fence "```"], [Paragraph (rng 0 1), CodeBlock Nothing (rng 4 5)])
    "```\na\n```\nb" <===> ([fence "```", hardBr, "a", hardBr, fence "```", lineBr, "b"], [CodeBlock Nothing (rng 2 3), Paragraph (rng 6 7)])
    "- a\n```\nb\n```" <===> ([bullet "- ", "a", lineBr, fence "```", hardBr, "b", hardBr, fence "```"], [List [entry 1 2 []], CodeBlock Nothing (rng 5 6)])

tables :: Spec
tables = describe "tables" do
  it "alignments" do
    "| a | b | c | d |\n|---|:--|--:|:-:|"
      <===> ( [mk RowMark "| ", "a", mk CellMark " | ", "b", mk CellMark " | ", "c", mk CellMark " | ", "d", mk CellMark " |", hardBr, mk (DelimiterRow [TANone, TALeft, TARight, TACenter]) "|---|:--|--:|:-:|"],
              [Table [TANone, TALeft, TARight, TACenter] [rng 1 2, rng 3 4, rng 5 6, rng 7 8] []]
            )
    "a | b\n:- | -:" <===> ([mk RowMark "", "a", mk CellMark " | ", "b", mk CellMark "", hardBr, mk (DelimiterRow [TALeft, TARight]) ":- | -:"], [Table [TALeft, TARight] [rng 1 2, rng 3 4] []])
  it "rows and cells" do
    "| a | b |\n|---|:-:|\n| 1 | 2 |"
      <===> ( [mk RowMark "| ", "a", mk CellMark " | ", "b", mk CellMark " |", hardBr, mk (DelimiterRow [TANone, TACenter]) "|---|:-:|", hardBr, mk RowMark "| ", "1", mk CellMark " | ", "2", mk CellMark " |"],
              [Table [TANone, TACenter] [rng 1 2, rng 3 4] [[rng 9 10, rng 11 12]]]
            )
    "|a|b|\n|-|-|\n|c|" <===> (header2 <> [hardBr, mk RowMark "|", "c", mk CellMark "|"], [Table [TANone, TANone] [rng 1 2, rng 3 4] [[rng 9 10]]])
    "|a|b|\n|-|-|\n|c|d|e|" <===> (header2 <> [hardBr, mk RowMark "|", "c", mk CellMark "|", "d|e", mk CellMark "|"], [Table [TANone, TANone] [rng 1 2, rng 3 4] [[rng 9 10, rng 11 12]]])
    "|a|b|\n|-|-|\n|c|d|e" <===> (header2 <> [hardBr, mk RowMark "|", "c", mk CellMark "|", "d|e", mk CellMark ""], [Table [TANone, TANone] [rng 1 2, rng 3 4] [[rng 9 10, rng 11 12]]])
    "|a|b|\n|-|-|\n||c|" <===> (header2 <> [hardBr, mk RowMark "|", mk CellMark "|", "c", mk CellMark "|"], [Table [TANone, TANone] [rng 1 2, rng 3 4] [[rng 9 9, rng 10 11]]])
    "|a|b|\n|-|-|\n|  |c|" <===> (header2 <> [hardBr, mk RowMark "|  ", mk CellMark "|", "c", mk CellMark "|"], [Table [TANone, TANone] [rng 1 2, rng 3 4] [[rng 9 9, rng 10 11]]])
    "|a|\n|-|\n||" <===> ([mk RowMark "|", "a", mk CellMark "|", hardBr, mk (DelimiterRow [TANone]) "|-|", hardBr, mk RowMark "|", mk CellMark "|"], [Table [TANone] [rng 1 2] [[rng 7 7]]])
    "  | a |\n  |---|\n  | b |" <===> ([mk RowMark "  | ", "a", mk CellMark " |", hardBr, mk (DelimiterRow [TANone]) "  |---|", hardBr, mk RowMark "  | ", "b", mk CellMark " |"], [Table [TANone] [rng 1 2] [[rng 7 8]]])
  it "inline content in cells" do
    "| *a* | @b |\n|---|---|" <===> ([mk RowMark "| ", fmt boldF "a", mk CellMark " | ", fmt (Mention "b") "@b", mk CellMark " |", hardBr, mk (DelimiterRow [TANone, TANone]) "|---|---|"], [Table [TANone, TANone] [rng 1 2, rng 3 4] []])
  it "lines that are not tables" do
    noBlocks "| a |\n| b |"
    noBlocks "| a | b |\n|---|"
    noBlocks "| a |\n|---|x"
    noBlocks "a\n-|-"
  it "tables and other blocks" do
    "a | b\n- | -\nc" <===> ([mk RowMark "", "a", mk CellMark " | ", "b", mk CellMark "", hardBr, mk (DelimiterRow [TANone, TANone]) "- | -", lineBr, "c"], [Table [TANone, TANone] [rng 1 2, rng 3 4] [], Paragraph (rng 8 9)])
    "| a |\n|---|\n- b | c" <===> ([mk RowMark "| ", "a", mk CellMark " |", hardBr, mk (DelimiterRow [TANone]) "|---|", lineBr, bullet "- ", "b | c"], [Table [TANone] [rng 1 2] [], List [entry 7 8 []]])
    "x\n| a |\n|---|" <===> (["x", lineBr, mk RowMark "| ", "a", mk CellMark " |", hardBr, mk (DelimiterRow [TANone]) "|---|"], [Paragraph (rng 0 1), Table [TANone] [rng 3 4] []])
  where
    header2 = [mk RowMark "|", "a", mk CellMark "|", "b", mk CellMark "|", hardBr, mk (DelimiterRow [TANone, TANone]) "|-|-|"]

formatsAcrossLines :: Spec
formatsAcrossLines = describe "formats across line breaks" do
  it "each delimited format, in a paragraph" do
    "*a\nb*" ==>> [fmt boldF "a", "\n", fmt boldF "b"]
    "**a\nb**" ==>> [fmt bold2F "a", "\n", fmt bold2F "b"]
    "__a\nb__" ==>> [fmt bold2F "a", "\n", fmt bold2F "b"]
    "_a\nb_" ==>> [fmt Italic "a", "\n", fmt Italic "b"]
    "~a\nb~" ==>> [fmt StrikeThrough "a", "\n", fmt StrikeThrough "b"]
    "~~a\nb~~" ==>> [fmt StrikeThrough "a", "\n", fmt StrikeThrough "b"]
    "`a\nb`" ==>> [fmt Snippet "a", "\n", fmt Snippet "b"]
    "#a\nb#" ==>> [fmt Secret "a", "\n", fmt Secret "b"]
    "!1 a\nb!" ==>> [fmt (colored Red) "a", "\n", fmt (colored Red) "b"]
    "!- a\nb!" ==>> [fmt Small "a", "\n", fmt Small "b"]
    "*a\nb\nc*" ==>> [fmt boldF "a", "\n", fmt boldF "b", "\n", fmt boldF "c"]
    "*a  \nb*" ==>> [fmt boldF "a", "  \n", fmt boldF "b"]
    "# h\n*a\nb*" ===> ([heading1, "h", lineBr, fmt boldF "a", hardBr, fmt boldF "b"], [Heading 1 (rng 1 2), Paragraph (rng 3 6)])
  it "in a list item and a quote" do
    "- *a\n  b*" ===> ([bullet "- ", fmt boldF "a", hardBr, mk Indent "  ", fmt boldF "b"], [List [entry 1 5 []]])
    "> *a\n> b*" ===> (quoted, [Quote (rng 1 5)])
    "> *a*\n> *b*" <===> (quoted, [Quote (rng 1 5)])
  it "not across blocks, blank lines and cells" do
    parseMaybeMarkdownList "*a\n\nb*" `shouldBe` Nothing
    "# *a\nb*" <===> ([heading1, "*a", lineBr, "b*"], [Heading 1 (rng 1 2), Paragraph (rng 3 4)])
    "- *a\n- b*" <===> ([bullet "- ", "*a", lineBr, bullet "- ", "b*"], [List [entry 1 2 [], entry 4 5 []]])
    "- *a\nb*" <===> ([bullet "- ", "*a", lineBr, "b*"], [List [entry 1 2 []], Paragraph (rng 3 4)])
    "> *a\nb*" <===> ([quoteMark "> ", "*a", lineBr, "b*"], [Quote (rng 1 2), Paragraph (rng 3 4)])
    "> *a\n>\n> b*" <===> ([quoteMark "> ", "*a", hardBr, quoteMark ">", hardBr, quoteMark "> ", "b*"], [Quote (rng 1 7)])
    "| *a | b* |\n|---|---|" <===> ([mk RowMark "| ", "*a", mk CellMark " | ", "b*", mk CellMark " |", hardBr, mk (DelimiterRow [TANone, TANone]) "|---|---|"], [Table [TANone, TANone] [rng 1 2, rng 3 4] []])
  it "a format starting or ending at a line break keeps its empty part" do
    "# h\n`\na`" ===> ([heading1, "h", lineBr, fmt Snippet "", hardBr, fmt Snippet "a"], [Heading 1 (rng 1 2), Paragraph (rng 3 6)])
    "# h\n``\n``" <===> ([heading1, "h", lineBr, fmt Snippet "", hardBr, fmt Snippet ""], [Heading 1 (rng 1 2), Paragraph (rng 3 6)])
    T.concat (map markdownText $ fromMaybe [] $ parseMaybeMarkdownList "# h\n`\na`") `shouldBe` "# h\n``\n`a`"
  it "an unclosed delimiter is text to the line end" do
    "*a\n_b_" ==>> ["*a\n", fmt Italic "b"]
    parseMaybeMarkdownList "*a\nb" `shouldBe` Nothing
  it "white space at the content ends: as on master, within the line" do
    parseMaybeMarkdownList "*a\n*" `shouldBe` Nothing
    parseMaybeMarkdownList "*a\nb *" `shouldBe` Nothing
    parseMaybeMarkdownList "_ a\nb_" `shouldBe` Nothing
    parseMaybeMarkdownList "**a\n**" `shouldBe` Nothing
    "5 * 3\n*b*" ==>> ["5 * 3\n", fmt boldF "b"]
  it "links, mentions and names within one line" do
    "[a\nb](https://simplex.chat)" ==>> ["[a\nb](", uri' "https://simplex.chat", ")"]
    parseMaybeMarkdownList "@'a\nb'" `shouldBe` Nothing
    "https://simplex.chat\nb" ==>> [uri' "https://simplex.chat", "\nb"]
    "@alice\nb" ==>> [fmt (Mention "alice") "@alice", "\nb"]
  where
    quoted = [quoteMark "> ", fmt boldF "a", hardBr, quoteMark "> ", fmt boldF "b"]

liveMessages :: Spec
liveMessages = describe "incomplete syntax of live messages" do
  it "a code block once a line follows the fence" do
    noBlocks "```"
    noBlocks "```js"
    "```\nls" <===> ([fence "```", hardBr, "ls"], [CodeBlock Nothing (rng 2 3)])
    "``` a ```" ==>> [fmt Snippet "a"]
  it "a rule until another character is added" do
    "---" <===> ([mk RuleMark "---"], [Rule (rng 0 1)])
    noBlocks "---x"
  it "a table once the delimiter row has a cell" do
    noBlocks "| a |"
    noBlocks "| a |\n|"
    "| a |\n|-" <===> ([mk RowMark "| ", "a", mk CellMark " |", hardBr, mk (DelimiterRow [TANone]) "|-"], [Table [TANone] [rng 1 2] []])
    noBlocks "| a |\n|---|x"
  it "block marks before their text" do
    noBlocks "-"
    noBlocks "1."
    noBlocks ">"
    "# a" <===> ([heading1, "a"], [Heading 1 (rng 1 2)])
    "- a" <===> ([bullet "- ", "a"], [List [entry 1 2 []]])
    "1. a" <===> ([numbered "1.", "a"], [List [ListEntry (Just "1.") (rng 1 2) []]])
    "> a" <===> ([quoteMark "> ", "a"], [Quote (rng 1 2)])

linearTimeParsing :: Spec
linearTimeParsing = describe "bounded scans for linear-time parsing" do
  it "phone numbers: at most 22 characters, as on master" do
    "0 0 0 0 0 0 0 0 0 0 0 0" <==> "0 " <> phone "0 0 0 0 0 0 0 0 0 0 0"
  it "link texts stop at \"[\", link targets and autolinks at white space" do
    "[a [b](https://simplex.chat)" <==> "[a " <> web "b" "https://simplex.chat" "[b](https://simplex.chat)"
    "[a](https://simplex.chat x)" <==> "[a](" <> uri "https://simplex.chat" <> " x)"
    "<https://simplex.chat x>" <==> "<https://simplex.chat x>"
  it "a text with more than 2000 markup tokens has no blocks" do
    let ParsedMarkdown _ blocks = parsedMarkdown $ T.replicate 1000 "- a\n"
    length <$> blocks `shouldBe` Just 1
    noBlocks $ T.replicate 1001 "- a\n"
    length . filter (\(FormattedText f _) -> f == Just boldF) <$> parseMaybeMarkdownList (T.replicate 1001 "- *a*\n") `shouldBe` Just 1001
  it "large adversarial texts" do
    forM_ (["1 ", "[ ", "[a]( ", "< ", "a\n", ">\n", "- a\n", "|\n", "|-|\n", "*a\n"] :: [Text]) $ \p ->
      length (show $ parseMaybeMarkdownList $ "|a|\n|-|\n" <> T.replicate 4000 p) `shouldSatisfy` (> 0)

markdownConsumers :: Spec
markdownConsumers = describe "consumers of parsed Markdown" do
  it "mentionedNames: mentions in blocks, not in code" do
    mentions "- @alice" `shouldBe` ["alice"]
    mentions "> @alice" `shouldBe` ["alice"]
    mentions "# @alice" `shouldBe` ["alice"]
    mentions "| @alice |\n|---|" `shouldBe` ["alice"]
    mentions "```\n@alice\n```" `shouldBe` []
    mentions "`@alice`" `shouldBe` []
  it "hasLinks: links in blocks, not in code" do
    links "- https://simplex.chat" `shouldBe` True
    links "| https://simplex.chat |\n|---|" `shouldBe` True
    links "```\nhttps://simplex.chat\n```" `shouldBe` False
  it "SimpleX links in blocks and in code" do
    simplexLinks ("- " <> addr) `shouldBe` True
    simplexLinks ("| " <> addr <> " |\n|---|") `shouldBe` True
    simplexLinks ("```\n" <> addr <> "\n```") `shouldBe` False
    hasObfuscatedSimplexLink ("```\n" <> addr <> "\n```") `shouldBe` True
    textWithoutSimplexLinks ("- check [this link](" <> addr <> ") out") `shouldBe` "- check  out"
  it "markdownText of markup tokens is their text" do
    forM_ ([LineBreak, SoftBreak, HardBreak, ParagraphBreak 2, Indent, HeadingMark 1, HeadingClose, HeadingUnderline 1, ListMark Nothing, QuoteMark, FenceMark Nothing, RowMark, CellMark, DelimiterRow [TANone], RuleMark] :: [BlockToken]) $ \tkn ->
      markdownText (mk tkn "x") `shouldBe` "x"
    markdownText (fmt Escape "\\") `shouldBe` "\\"
  where
    parsed = fromMaybe [] . parseMaybeMarkdownList
    mentions = mentionedNames . parsed
    links = hasLinks . parsed
    isSimplexLinkFormat (FormattedText f _) = maybe False isSimplexLink f
    simplexLinks = any isSimplexLinkFormat . parsed
    textWithoutSimplexLinks = T.concat . map (\(FormattedText _ t) -> t) . filter (not . isSimplexLinkFormat) . parsed
    addr = "https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw"

testSanitizeUri :: Spec
testSanitizeUri = describe "sanitizeUri" $ do
  it "should allow the first parameter and whitelisted parameters on pages without IDs" $ do
    "https://example.com/page?ref=123" `sanitized` Just "https://example.com/page"
    "https://example.com/page?name" `sanitized` Nothing
    "https://example.com/page?name=abc" `sanitized` Nothing
    "https://example.com/page?name=abc&ref=123" `sanitized` Just "https://example.com/page?name=abc"
    "https://example.com/page?search=query" `sanitized` Nothing
    "https://example.com/page?q=query" `sanitized` Nothing
    "https://example.com/page?ref=123&q=query" `sanitized` Just "https://example.com/page?q=query"
    "https://youtube.com/watch?v=abc&t=123" `sanitized` Nothing
    "https://www.youtube.com/watch?v=abc" `sanitized` Nothing
    "https://www.youtube.com/watch?v=abc&t=123" `sanitized` Nothing
    "https://www.youtube.com/watch?ref=456&v=abc&t=123" `sanitized` Just "https://www.youtube.com/watch?v=abc&t=123"
  it "should only allow whitelisted parameters if path contains IDs" $ do
    "https://youtu.be/a123?si=456" `sanitized` Just "https://youtu.be/a123"
    "https://youtu.be/a123?t=456" `sanitized` Nothing
    "https://youtu.be/a123?si=456&t=789" `sanitized` Just "https://youtu.be/a123?t=789"
  it "should allow some parameters in safe mode, but sanitize in unsafe" $ do
    "https://example.com/page/a123?source=abc" `eagerSanitized` Just "https://example.com/page/a123"
    "https://example.com/page/a123?source=abc" `safeSanitized` Nothing -- source is in unsafe blacklist
    "https://example.com/page/a123?name=abc" `eagerSanitized` Just "https://example.com/page/a123"
    "https://example.com/page/a123?name=abc" `safeSanitized` Nothing -- name is not in a whitelist
  it "should keep whitelisted parameters in safe mode even if they match a blacklist prefix" $ do
    "https://example.com/playlist?list=abc" `sanitized` Nothing -- "list" is whitelisted, "li" is blacklisted
    "https://example.com/playlist?list=abc&si=def" `sanitized` Just "https://example.com/playlist?list=abc"
    "https://github.com/owner/repo?ref=main" `sanitized` Nothing -- "ref" is whitelisted for github.com
  where
    s `eagerSanitized` res = sanitized_ False s res
    s `safeSanitized` res = sanitized_ True s res
    s `sanitized` res = do
      s `eagerSanitized` res
      s `safeSanitized` res
    sanitized_ safe s res = (U.serializeURIRef' <$$> (sanitizeUri safe <$> parseUri s)) `shouldBe` Right res
