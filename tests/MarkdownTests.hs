{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownTests where

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

textWithUri :: Spec
textWithUri = describe "text with Uri" do
  it "correct markdown" do
    parseMarkdown "https://simplex.chat" `shouldBe` uri "https://simplex.chat"
    parseMarkdown "http://simplex.chat" `shouldBe` uri "http://simplex.chat"
    parseMarkdown "this is https://simplex.chat" `shouldBe` "this is " <> uri "https://simplex.chat"
    parseMarkdown "https://simplex.chat site" `shouldBe` uri "https://simplex.chat" <> " site"
  it "ignored as markdown" do
    parseMarkdown "_https://simplex.chat" `shouldBe` "_https://simplex.chat"
    parseMarkdown "this is _https://simplex.chat" `shouldBe` "this is _https://simplex.chat"

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
