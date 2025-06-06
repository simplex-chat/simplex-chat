{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownTests where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Markdown
import Simplex.Messaging.Encoding.String
import System.Console.ANSI.Types
import Test.Hspec

markdownTests :: Spec
markdownTests = do
  textFormat
  secretText
  textColor
  textWithUri
  textWithEmail
  textWithPhone
  textWithMentions
  multilineMarkdownList

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

bold :: Text -> Markdown
bold = markdown Bold

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
  it "ignored as markdown" do
    "this is * unformatted * text"
      <==> "this is * unformatted * text"
    "this is *unformatted * text"
      <==> "this is *unformatted * text"
    "this is * unformatted* text"
      <==> "this is * unformatted* text"
    "this is **unformatted** text"
      <==> "this is **unformatted** text"
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
      <==> "this is #unformatted # text"
    "this is # unformatted# text"
      <==> "this is # unformatted# text"
    "this is ## unformatted ## text"
      <==> "this is ## unformatted ## text"
    "this is#unformatted# text"
      <==> "this is#unformatted# text"
    "this is #unformatted text"
      <==> "this is #unformatted text"
    "*this* is #unformatted text"
      <==> bold "this" <> " is #unformatted text"
  it "ignored internal markdown" do
    "snippet: `this is #secret_text#`"
      <==> "snippet: " <> markdown Snippet "this is #secret_text#"

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
simplexLink linkType uriText smpHosts t = Markdown (simplexLinkFormat linkType uriText smpHosts) t

simplexLinkFormat :: SimplexLinkType -> Text -> NonEmpty Text -> Maybe Format
simplexLinkFormat linkType uriText smpHosts = case strDecode $ encodeUtf8 uriText of
  Right simplexUri -> Just SimplexLink {linkType, simplexUri, smpHosts}
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
    -- "SimpleX on GitHub (https://github.com/simplex-chat/)" <==> "SimpleX on GitHub (" <> uri "https://github.com/simplex-chat/" <> ")"
    "https://en.m.wikipedia.org/wiki/Servo_(software)" <==> uri "https://en.m.wikipedia.org/wiki/Servo_(software)"
    "example.com" <==> uri "example.com"
    "example.com." <==> uri "example.com" <> "."
    "example.com..." <==> uri "example.com" <> "..."
    "www.example.com" <==> uri "www.example.com"
    "example.academy" <==> uri "example.academy"
    "this is example.com" <==> "this is " <> uri "example.com"
    "x.com" <==> uri "x.com"
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
    "chat @simplex.chat" <==> "chat " <> mention "simplex.chat" "@simplex.chat"
    "this is chat @simplex.chat" <==> "this is chat " <> mention "simplex.chat" "@simplex.chat"
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

uri' :: Text -> FormattedText
uri' = FormattedText $ Just Uri

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
        [ FormattedText (simplexLinkFormat XLInvitation ("simplex:" <> inv) ["smp.simplex.im"]) ("https://simplex.chat" <> inv),
          "\ntext"
        ]
