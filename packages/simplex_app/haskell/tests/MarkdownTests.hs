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

textFormat :: Spec
textFormat = describe "text format (bold)" do
  it "correct markdown" do
    parseMarkdown "this is *bold formatted* text"
      `shouldBe` "this is " <> Markdown Bold "bold formatted" <> " " <> "text"
    parseMarkdown "*bold formatted* text"
      `shouldBe` Markdown Bold "bold formatted" <> " " <> "text"
    parseMarkdown "this is *bold*"
      `shouldBe` "this is " <> Markdown Bold "bold"
    parseMarkdown " *bold* text"
      `shouldBe` " " <> Markdown Bold "bold" <> " " <> "text"
    parseMarkdown "   *bold* text"
      `shouldBe` "   " <> Markdown Bold "bold" <> " " <> "text"
    parseMarkdown "this is *bold* "
      `shouldBe` "this is " <> Markdown Bold "bold" <> " "
    parseMarkdown "this is *bold*   "
      `shouldBe` "this is " <> Markdown Bold "bold" <> "   "
  it "ignored as markdown" do
    parseMarkdown "this is * unformatted * text"
      `shouldBe` "this is " <> "* unformatted *" <> " " <> "text"
    parseMarkdown "this is *unformatted * text"
      `shouldBe` "this is " <> "*unformatted *" <> " " <> "text"
    parseMarkdown "this is * unformatted* text"
      `shouldBe` "this is " <> "* unformatted*" <> " " <> "text"
    parseMarkdown "this is **unformatted** text"
      `shouldBe` "this is " <> "**" <> "unformatted** text"
    parseMarkdown "this is*unformatted* text"
      `shouldBe` "this is*unformatted* text"
    parseMarkdown "this is *unformatted text"
      `shouldBe` "this is " <> "*unformatted text"
  it "ignored internal markdown" do
    parseMarkdown "this is *long _bold_ (not italic)* text"
      `shouldBe` "this is " <> Markdown Bold "long _bold_ (not italic)" <> " " <> "text"
    parseMarkdown "snippet: `this is *bold text*`"
      `shouldBe` "snippet: " <> Markdown Snippet "this is *bold text*"

secretText :: Spec
secretText = describe "secret text" do
  it "correct markdown" do
    parseMarkdown "this is #black_secret# text"
      `shouldBe` "this is " <> Markdown Secret "black_secret" <> " " <> "text"
    parseMarkdown "##black_secret### text"
      `shouldBe` Markdown Secret "#black_secret##" <> " " <> "text"
    parseMarkdown "this is #black secret# text"
      `shouldBe` "this is " <> Markdown Secret "black secret" <> " " <> "text"
    parseMarkdown "##black secret### text"
      `shouldBe` Markdown Secret "#black secret##" <> " " <> "text"
    parseMarkdown "this is #secret#"
      `shouldBe` "this is " <> Markdown Secret "secret"
    parseMarkdown " #secret# text"
      `shouldBe` " " <> Markdown Secret "secret" <> " " <> "text"
    parseMarkdown "   #secret# text"
      `shouldBe` "   " <> Markdown Secret "secret" <> " " <> "text"
    parseMarkdown "this is #secret# "
      `shouldBe` "this is " <> Markdown Secret "secret" <> " "
    parseMarkdown "this is #secret#   "
      `shouldBe` "this is " <> Markdown Secret "secret" <> "   "
  it "ignored as markdown" do
    parseMarkdown "this is # unformatted # text"
      `shouldBe` "this is " <> "# unformatted #" <> " " <> "text"
    parseMarkdown "this is #unformatted # text"
      `shouldBe` "this is " <> "#unformatted #" <> " " <> "text"
    parseMarkdown "this is # unformatted# text"
      `shouldBe` "this is " <> "# unformatted#" <> " " <> "text"
    parseMarkdown "this is ## unformatted ## text"
      `shouldBe` "this is " <> "## unformatted ##" <> " " <> "text"
    parseMarkdown "this is#unformatted# text"
      `shouldBe` "this is#unformatted# text"
    parseMarkdown "this is #unformatted text"
      `shouldBe` "this is " <> "#unformatted text"
  it "ignored internal markdown" do
    parseMarkdown "snippet: `this is #secret_text#`"
      `shouldBe` "snippet: " <> Markdown Snippet "this is #secret_text#"

red :: Text -> Markdown
red = Markdown (Colored Red)

textColor :: Spec
textColor = describe "text color (red)" do
  it "correct markdown" do
    parseMarkdown "this is !1 red color! text"
      `shouldBe` "this is " <> red "red color" <> " " <> "text"
    parseMarkdown "!1 red! text"
      `shouldBe` red "red" <> " " <> "text"
    parseMarkdown "this is !1 red!"
      `shouldBe` "this is " <> red "red"
    parseMarkdown " !1 red! text"
      `shouldBe` " " <> red "red" <> " " <> "text"
    parseMarkdown "   !1 red! text"
      `shouldBe` "   " <> red "red" <> " " <> "text"
    parseMarkdown "this is !1 red! "
      `shouldBe` "this is " <> red "red" <> " "
    parseMarkdown "this is !1 red!   "
      `shouldBe` "this is " <> red "red" <> "   "
  it "ignored as markdown" do
    parseMarkdown "this is !1 unformatted ! text"
      `shouldBe` "this is " <> "!1 unformatted !" <> " " <> "text"
    parseMarkdown "this is !1 unformatted ! text"
      `shouldBe` "this is " <> "!1 unformatted !" <> " " <> "text"
    parseMarkdown "this is !1  unformatted! text"
      `shouldBe` "this is " <> "!1  unformatted!" <> " " <> "text"
    -- parseMarkdown "this is !!1 unformatted!! text"
    --   `shouldBe` "this is " <> "!!1" <> "unformatted!! text"
    parseMarkdown "this is!1 unformatted! text"
      `shouldBe` "this is!1 unformatted! text"
    parseMarkdown "this is !1 unformatted text"
      `shouldBe` "this is " <> "!1 unformatted text"
  it "ignored internal markdown" do
    parseMarkdown "this is !1 long *red* (not bold)! text"
      `shouldBe` "this is " <> red "long *red* (not bold)" <> " " <> "text"
    parseMarkdown "snippet: `this is !1 red text!`"
      `shouldBe` "snippet: " <> Markdown Snippet "this is !1 red text!"
