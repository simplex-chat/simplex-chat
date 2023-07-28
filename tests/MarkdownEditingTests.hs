{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownEditingTests where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Simplex.Chat.Markdown
import Simplex.Chat.MarkdownEditing
import System.Console.ANSI.Types
import Test.Hspec

markdownEditingTests :: Spec
markdownEditingTests = do
  formattedEditedTextTests


plainText :: Text -> FormattedText
plainText = FormattedText Nothing

redText :: Text -> FormattedText
redText = FormattedText $ Just $ colored Red

plainEdited :: Text -> Bool -> EditedText
plainEdited t added = EditedText Nothing t (Just added)

redEdited :: Text -> Bool -> EditedText
redEdited t added = EditedText (Just $ colored Red) t (Just added)

formattedEditedTextTests :: Spec
formattedEditedTextTests = fdescribe "show edits using Myers Diff algorithm" do
  it "one character change" do
    formattedEditedText [plainText "Hrllo"] [plainText "Hello"]
      `shouldBe` []
    --   `shouldBe` [plainText "H", plainEdited "r" False, plainEdited "e" True, plainText "llo"]
