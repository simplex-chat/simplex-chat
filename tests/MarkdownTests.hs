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
