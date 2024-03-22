{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownTests where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Simplex.Chat.Markdown
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
  multilineMarkdownList

textFormat :: Spec
textFormat = describe "text format (bold)" do
  it "correct markdown" do
    parseMarkdown "this is *bold formatted* text"
      `shouldBe` "this is " <> markdown Bold "bold formatted" <> " text"
    parseMarkdown "*bold formatted* text"
      `shouldBe` markdown Bold "bold formatted" <> " text"
    parseMarkdown "this is *bold*"
      `shouldBe` "this is " <> markdown Bold "bold"
    parseMarkdown " *bold* text"
      `shouldBe` " " <> markdown Bold "bold" <> " text"
    parseMarkdown "   *bold* text"
      `shouldBe` "   " <> markdown Bold "bold" <> " text"
    parseMarkdown "this is *bold* "
      `shouldBe` "this is " <> markdown Bold "bold" <> " "
    parseMarkdown "this is *bold*   "
      `shouldBe` "this is " <> markdown Bold "bold" <> "   "
  it "ignored as markdown" do
    parseMarkdown "this is * unformatted * text"
      `shouldBe` "this is * unformatted * text"
    parseMarkdown "this is *unformatted * text"
      `shouldBe` "this is *unformatted * text"
    parseMarkdown "this is * unformatted* text"
      `shouldBe` "this is * unformatted* text"
    parseMarkdown "this is **unformatted** text"
      `shouldBe` "this is **unformatted** text"
    parseMarkdown "this is*unformatted* text"
      `shouldBe` "this is*unformatted* text"
    parseMarkdown "this is *unformatted text"
      `shouldBe` "this is *unformatted text"
  it "ignored internal markdown" do
    parseMarkdown "this is *long _bold_ (not italic)* text"
      `shouldBe` "this is " <> markdown Bold "long _bold_ (not italic)" <> " text"
    parseMarkdown "snippet: `this is *bold text*`"
      `shouldBe` "snippet: " <> markdown Snippet "this is *bold text*"

secretText :: Spec
secretText = describe "secret text" do
  it "correct markdown" do
    parseMarkdown "this is #black_secret# text"
      `shouldBe` "this is " <> markdown Secret "black_secret" <> " text"
    parseMarkdown "##black_secret### text"
      `shouldBe` markdown Secret "#black_secret##" <> " text"
    parseMarkdown "this is #black secret# text"
      `shouldBe` "this is " <> markdown Secret "black secret" <> " text"
    parseMarkdown "##black secret### text"
      `shouldBe` markdown Secret "#black secret##" <> " text"
    parseMarkdown "this is #secret#"
      `shouldBe` "this is " <> markdown Secret "secret"
    parseMarkdown " #secret# text"
      `shouldBe` " " <> markdown Secret "secret" <> " text"
    parseMarkdown "   #secret# text"
      `shouldBe` "   " <> markdown Secret "secret" <> " text"
    parseMarkdown "this is #secret# "
      `shouldBe` "this is " <> markdown Secret "secret" <> " "
    parseMarkdown "this is #secret#   "
      `shouldBe` "this is " <> markdown Secret "secret" <> "   "
  it "ignored as markdown" do
    parseMarkdown "this is # unformatted # text"
      `shouldBe` "this is # unformatted # text"
    parseMarkdown "this is #unformatted # text"
      `shouldBe` "this is #unformatted # text"
    parseMarkdown "this is # unformatted# text"
      `shouldBe` "this is # unformatted# text"
    parseMarkdown "this is ## unformatted ## text"
      `shouldBe` "this is ## unformatted ## text"
    parseMarkdown "this is#unformatted# text"
      `shouldBe` "this is#unformatted# text"
    parseMarkdown "this is #unformatted text"
      `shouldBe` "this is #unformatted text"
  it "ignored internal markdown" do
    parseMarkdown "snippet: `this is #secret_text#`"
      `shouldBe` "snippet: " <> markdown Snippet "this is #secret_text#"

red :: Text -> Markdown
red = markdown (colored Red)

textColor :: Spec
textColor = describe "text color (red)" do
  it "correct markdown" do
    parseMarkdown "this is !1 red color! text"
      `shouldBe` "this is " <> red "red color" <> " text"
    parseMarkdown "!1 red! text"
      `shouldBe` red "red" <> " text"
    parseMarkdown "this is !1 red!"
      `shouldBe` "this is " <> red "red"
    parseMarkdown " !1 red! text"
      `shouldBe` " " <> red "red" <> " text"
    parseMarkdown "   !1 red! text"
      `shouldBe` "   " <> red "red" <> " text"
    parseMarkdown "this is !1 red! "
      `shouldBe` "this is " <> red "red" <> " "
    parseMarkdown "this is !1 red!   "
      `shouldBe` "this is " <> red "red" <> "   "
  it "ignored as markdown" do
    parseMarkdown "this is !1 unformatted ! text"
      `shouldBe` "this is !1 unformatted ! text"
    parseMarkdown "this is !1 unformatted ! text"
      `shouldBe` "this is !1 unformatted ! text"
    parseMarkdown "this is !1  unformatted! text"
      `shouldBe` "this is !1  unformatted! text"
    -- parseMarkdown "this is !!1 unformatted!! text"
    --   `shouldBe` "this is " <> "!!1" <> "unformatted!! text"
    parseMarkdown "this is!1 unformatted! text"
      `shouldBe` "this is!1 unformatted! text"
    parseMarkdown "this is !1 unformatted text"
      `shouldBe` "this is !1 unformatted text"
  it "ignored internal markdown" do
    parseMarkdown "this is !1 long *red* (not bold)! text"
      `shouldBe` "this is " <> red "long *red* (not bold)" <> " text"
    parseMarkdown "snippet: `this is !1 red text!`"
      `shouldBe` "snippet: " <> markdown Snippet "this is !1 red text!"

uri :: Text -> Markdown
uri = Markdown $ Just Uri

simplexLink :: SimplexLinkType -> Text -> NonEmpty Text -> Text -> Markdown
simplexLink linkType simplexUri smpHosts = Markdown $ Just SimplexLink {linkType, simplexUri, smpHosts}

textWithUri :: Spec
textWithUri = describe "text with Uri" do
  it "correct markdown" do
    parseMarkdown "https://simplex.chat" `shouldBe` uri "https://simplex.chat"
    parseMarkdown "https://simplex.chat." `shouldBe` uri "https://simplex.chat" <> "."
    parseMarkdown "https://simplex.chat, hello" `shouldBe` uri "https://simplex.chat" <> ", hello"
    parseMarkdown "http://simplex.chat" `shouldBe` uri "http://simplex.chat"
    parseMarkdown "this is https://simplex.chat" `shouldBe` "this is " <> uri "https://simplex.chat"
    parseMarkdown "https://simplex.chat site" `shouldBe` uri "https://simplex.chat" <> " site"
  it "ignored as markdown" do
    parseMarkdown "_https://simplex.chat" `shouldBe` "_https://simplex.chat"
    parseMarkdown "this is _https://simplex.chat" `shouldBe` "this is _https://simplex.chat"
  it "SimpleX links" do
    let inv = "/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D"
    parseMarkdown ("https://simplex.chat" <> inv) `shouldBe` simplexLink XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] ("https://simplex.chat" <> inv)
    parseMarkdown ("simplex:" <> inv) `shouldBe` simplexLink XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] ("simplex:" <> inv)
    parseMarkdown ("https://example.com" <> inv) `shouldBe` simplexLink XLInvitation ("simplex:" <> inv) ["smp.simplex.im"] ("https://example.com" <> inv)
    let ct = "/contact#/?v=2&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D"
    parseMarkdown ("https://simplex.chat" <> ct) `shouldBe` simplexLink XLContact ("simplex:" <> ct) ["smp.simplex.im"] ("https://simplex.chat" <> ct)
    let gr = "/contact#/?v=2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FWHV0YU1sYlU7NqiEHkHDB6gxO1ofTync%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAWbebOqVYuBXaiqHcXYjEHCpYi6VzDlu6CVaijDTmsQU%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22mL-7Divb94GGmGmRBef5Dg%3D%3D%22%7D"
    parseMarkdown ("https://simplex.chat" <> gr) `shouldBe` simplexLink XLGroup ("simplex:" <> gr) ["smp4.simplex.im", "o5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"] ("https://simplex.chat" <> gr)

email :: Text -> Markdown
email = Markdown $ Just Email

textWithEmail :: Spec
textWithEmail = describe "text with Email" do
  it "correct markdown" do
    parseMarkdown "chat@simplex.chat" `shouldBe` email "chat@simplex.chat"
    parseMarkdown "test chat@simplex.chat" `shouldBe` "test " <> email "chat@simplex.chat"
    parseMarkdown "test chat+123@simplex.chat" `shouldBe` "test " <> email "chat+123@simplex.chat"
    parseMarkdown "test chat.chat+123@simplex.chat" `shouldBe` "test " <> email "chat.chat+123@simplex.chat"
    parseMarkdown "chat@simplex.chat test" `shouldBe` email "chat@simplex.chat" <> " test"
    parseMarkdown "test1 chat@simplex.chat test2" `shouldBe` "test1 " <> email "chat@simplex.chat" <> " test2"
  it "ignored as markdown" do
    parseMarkdown "chat @simplex.chat" `shouldBe` "chat @simplex.chat"
    parseMarkdown "this is chat @simplex.chat" `shouldBe` "this is chat @simplex.chat"

phone :: Text -> Markdown
phone = Markdown $ Just Phone

textWithPhone :: Spec
textWithPhone = describe "text with Phone" do
  it "correct markdown" do
    parseMarkdown "07777777777" `shouldBe` phone "07777777777"
    parseMarkdown "test 07777777777" `shouldBe` "test " <> phone "07777777777"
    parseMarkdown "07777777777 test" `shouldBe` phone "07777777777" <> " test"
    parseMarkdown "test1 07777777777 test2" `shouldBe` "test1 " <> phone "07777777777" <> " test2"
    parseMarkdown "test 07777 777 777 test" `shouldBe` "test " <> phone "07777 777 777" <> " test"
    parseMarkdown "test +447777777777 test" `shouldBe` "test " <> phone "+447777777777" <> " test"
    parseMarkdown "test +44 (0) 7777 777 777 test" `shouldBe` "test " <> phone "+44 (0) 7777 777 777" <> " test"
    parseMarkdown "test +44-7777-777-777 test" `shouldBe` "test " <> phone "+44-7777-777-777" <> " test"
    parseMarkdown "test +44 (0) 7777.777.777 https://simplex.chat test"
      `shouldBe` "test " <> phone "+44 (0) 7777.777.777" <> " " <> uri "https://simplex.chat" <> " test"
  it "ignored as markdown (too short)" $
    parseMarkdown "test 077777 test" `shouldBe` "test 077777 test"
  it "ignored as markdown (double spaces)" $
    parseMarkdown "test 07777  777  777 test" `shouldBe` "test 07777  777  777 test"

uri' :: Text -> FormattedText
uri' = FormattedText $ Just Uri

multilineMarkdownList :: Spec
multilineMarkdownList = describe "multiline markdown" do
  it "correct markdown" do
    parseMaybeMarkdownList "http://simplex.chat\nhttp://app.simplex.chat" `shouldBe` Just [uri' "http://simplex.chat", "\n", uri' "http://app.simplex.chat"]
  it "combines the same formats" do
    parseMaybeMarkdownList "http://simplex.chat\ntext 1\ntext 2\nhttp://app.simplex.chat" `shouldBe` Just [uri' "http://simplex.chat", "\ntext 1\ntext 2\n", uri' "http://app.simplex.chat"]
  it "no markdown" do
    parseMaybeMarkdownList "not a\nmarkdown" `shouldBe` Nothing
