{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownDiffTests where

import qualified Data.List.NonEmpty as NE
import Simplex.Chat.Markdown
import Simplex.Chat.MarkdownDiff
import System.Console.ANSI.Types
import Test.Hspec

markdownDiffTests :: Spec
markdownDiffTests = do
  formattedEditedTextTests

formattedEditedTextTests :: Spec
formattedEditedTextTests = describe "show edits" do
  it "empty no change" $
    diff [] [] `shouldBe` []
  it "no change" $
    diff [FormatChar 'H' Nothing] [FormatChar 'H' Nothing]
      `shouldBe` [DiffChar (FormatChar 'H' Nothing) Nothing]
  it "add 1 char to empty" $
    diff [] [FormatChar 'H' Nothing]
      `shouldBe` [DiffChar (FormatChar 'H' Nothing) $ Just EAInsert]
  it "del the one and only" $
    diff [FormatChar 'H' Nothing] []
      `shouldBe` [DiffChar (FormatChar 'H' Nothing) $ Just EADelete]
  it "one character change" do
    diff
      [ FormatChar 'H' Nothing,
        FormatChar 'r' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'o' Nothing
      ]
      [ FormatChar 'H' Nothing,
        FormatChar 'e' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'o' Nothing
      ]
      `shouldBe` [ DiffChar (FormatChar 'H' Nothing) Nothing,
                   DiffChar (FormatChar 'r' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'e' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'l' Nothing) Nothing,
                   DiffChar (FormatChar 'l' Nothing) Nothing,
                   DiffChar (FormatChar 'o' Nothing) Nothing
                 ]

  it "more1" do
    diff
      [ FormatChar 'H' Nothing,
        FormatChar 'r' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'o' Nothing
      ]
      [ FormatChar 'H' Nothing,
        FormatChar 'e' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'o' Nothing,
        FormatChar 'x' Nothing,
        FormatChar 'y' Nothing,
        FormatChar 'z' Nothing
      ]
      `shouldBe` [ DiffChar (FormatChar 'H' Nothing) Nothing,
                   DiffChar (FormatChar 'r' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'e' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'l' Nothing) Nothing,
                   DiffChar (FormatChar 'l' Nothing) Nothing,
                   DiffChar (FormatChar 'o' Nothing) Nothing,
                   DiffChar (FormatChar 'x' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'y' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'z' Nothing) $ Just EAInsert
                 ]

  it "more2" do
    diff
      [ FormatChar 'H' Nothing,
        FormatChar 'r' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'o' Nothing
      ]
      [ FormatChar 'H' Nothing,
        FormatChar 'e' Nothing,
        FormatChar 'x' Nothing,
        FormatChar 'y' Nothing,
        FormatChar 'z' Nothing,
        FormatChar 'o' Nothing
      ]
      `shouldBe` [ DiffChar (FormatChar 'H' Nothing) Nothing,
                   DiffChar (FormatChar 'r' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'l' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'l' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'e' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'x' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'y' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'z' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'o' Nothing) Nothing
                 ]

  it "more3" do
    diff
      [ FormatChar 'H' $ Just Bold,
        FormatChar 'H' $ Just Bold,
        FormatChar 'r' Nothing,
        FormatChar 'l' $ Just Secret,
        FormatChar 'l' Nothing,
        FormatChar 'o' $ Just $ colored Green
      ]
      [ FormatChar 'H' $ Just Italic,
        FormatChar 'H' $ Just Bold,
        FormatChar 'e' $ Just $ colored Cyan,
        FormatChar 'x' Nothing,
        FormatChar 'y' Nothing,
        FormatChar 'z' $ Just Secret,
        FormatChar 'o' $ Just $ colored Blue
      ]
      `shouldBe` [ DiffChar (FormatChar 'H' (Just Italic)) (Just EAChangeFormat),
                   DiffChar (FormatChar 'H' (Just Bold)) Nothing,
                   DiffChar (FormatChar 'r' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'l' (Just Secret)) $ Just EADelete,
                   DiffChar (FormatChar 'l' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'e' (Just $ colored Cyan)) $ Just EAInsert,
                   DiffChar (FormatChar 'x' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'y' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'z' (Just Secret)) $ Just EAInsert,
                   DiffChar (FormatChar 'o' (Just $ colored Blue)) (Just EAChangeFormat)
                 ]

  it "more4" do
    diff
      [ FormatChar 'H' Nothing,
        FormatChar 'r' Nothing,
        FormatChar 'l' Nothing,
        FormatChar '~' Nothing,
        FormatChar '!' Nothing,
        FormatChar '@' Nothing,
        FormatChar 'l' Nothing,
        FormatChar 'o' Nothing
      ]
      [ FormatChar 'H' Nothing,
        FormatChar 'e' Nothing,
        FormatChar 'r' Nothing,
        FormatChar 'x' Nothing,
        FormatChar 'y' Nothing,
        FormatChar '!' Nothing,
        FormatChar '@' Nothing,
        FormatChar 'z' Nothing,
        FormatChar 'o' Nothing,
        FormatChar '1' Nothing,
        FormatChar '2' Nothing
      ]
      `shouldBe` [ DiffChar (FormatChar 'H' Nothing) Nothing,
                   DiffChar (FormatChar 'e' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'r' Nothing) Nothing,
                   DiffChar (FormatChar 'l' Nothing) $ Just EADelete,
                   DiffChar (FormatChar '~' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'x' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'y' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar '!' Nothing) Nothing,
                   DiffChar (FormatChar '@' Nothing) Nothing,
                   DiffChar (FormatChar 'l' Nothing) $ Just EADelete,
                   DiffChar (FormatChar 'z' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar 'o' Nothing) Nothing,
                   DiffChar (FormatChar '1' Nothing) $ Just EAInsert,
                   DiffChar (FormatChar '2' Nothing) $ Just EAInsert
                 ]

  it "SimplexLink 1" do
    diff
      [ FormatChar '>' $
          Just $
            SimplexLink
              { linkType = XLContact,
                simplexUri = "https://api.twitter.com/2/tweets/:id",
                trustedUri = True,
                smpHosts = NE.fromList ["host1", "host2", "host3"]
              }
      ]
      [ FormatChar '>' $
          Just
            SimplexLink
              { linkType = XLContact,
                simplexUri = "https://api.twitter.com/3/tweets/:id",
                trustedUri = True,
                smpHosts = NE.fromList ["host0", "host2", "host3"]
              }
      ]
      `shouldBe` [ DiffChar
                     ( FormatChar '>' $
                         Just
                           SimplexLink
                             { linkType = XLContact,
                               simplexUri = "https://api.twitter.com/3/tweets/:id",
                               trustedUri = True,
                               smpHosts = NE.fromList ["host0", "host2", "host3"]
                             }
                     )
                     (Just EAChangeFormat)
                 ]

  it "SimplexLink 2" do
    diff
      [ FormatChar '>' $
          Just $
            SimplexLink
              { linkType = XLContact,
                simplexUri = "https://api.twitter.com/2/tweets/:id",
                trustedUri = True,
                smpHosts = NE.fromList ["host1", "host2", "host3"]
              }
      ]
      [ FormatChar '>' $
          Just
            SimplexLink
              { linkType = XLContact,
                simplexUri = "https://api.twitter.com/3/tweets/:id",
                trustedUri = True,
                smpHosts = NE.fromList ["host1", "host2", "host3"]
              }
      ]
      `shouldBe` [ DiffChar
                     ( FormatChar '>' $
                         Just
                           SimplexLink
                             { linkType = XLContact,
                               simplexUri = "https://api.twitter.com/3/tweets/:id",
                               trustedUri = True,
                               smpHosts = NE.fromList ["host1", "host2", "host3"]
                             }
                     )
                     (Just EAChangeFormat)
                 ]

  it "SimplexLink 3" do
    diff
      [ FormatChar '>' $
          Just $
            SimplexLink
              { linkType = XLContact,
                simplexUri = "https://api.twitter.com/2/tweets/:id",
                trustedUri = True,
                smpHosts = NE.fromList ["host1", "host2", "host3"]
              }
      ]
      [ FormatChar '>' $
          Just
            SimplexLink
              { linkType = XLContact,
                simplexUri = "https://api.twitter.com/2/tweets/:id",
                trustedUri = True,
                smpHosts = NE.fromList ["host0", "host2", "host3"]
              }
      ]
      `shouldBe` [ DiffChar
                     ( FormatChar '>' $
                         Just
                           SimplexLink
                             { linkType = XLContact,
                               simplexUri = "https://api.twitter.com/2/tweets/:id",
                               trustedUri = True,
                               smpHosts = NE.fromList ["host0", "host2", "host3"]
                             }
                     )
                     (Just EAChangeFormat)
                 ]

  it "plainDiff 1" do
    plainDiff
      "https://api.twitter.com/2/tweets/:id"
      "https://api.twitter.com/3/tweets/:id"
      `shouldBe` [ DiffPlainChar 'h' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 'p' Nothing,
                   DiffPlainChar 's' Nothing,
                   DiffPlainChar ':' Nothing,
                   DiffPlainChar '/' Nothing,
                   DiffPlainChar '/' Nothing,
                   DiffPlainChar 'a' Nothing,
                   DiffPlainChar 'p' Nothing,
                   DiffPlainChar 'i' Nothing,
                   DiffPlainChar '.' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 'w' Nothing,
                   DiffPlainChar 'i' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 'e' Nothing,
                   DiffPlainChar 'r' Nothing,
                   DiffPlainChar '.' Nothing,
                   DiffPlainChar 'c' Nothing,
                   DiffPlainChar 'o' Nothing,
                   DiffPlainChar 'm' Nothing,
                   DiffPlainChar '/' Nothing,
                   DiffPlainChar '2' $ Just EADelete,
                   DiffPlainChar '3' $ Just EAInsert,
                   DiffPlainChar '/' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 'w' Nothing,
                   DiffPlainChar 'e' Nothing,
                   DiffPlainChar 'e' Nothing,
                   DiffPlainChar 't' Nothing,
                   DiffPlainChar 's' Nothing,
                   DiffPlainChar '/' Nothing,
                   DiffPlainChar ':' Nothing,
                   DiffPlainChar 'i' Nothing,
                   DiffPlainChar 'd' Nothing
                 ]

  it "plainDiff 2" do
    plainDiff
      "Hrl~!@lo"
      "Herxy!@zo12"
      `shouldBe` [ DiffPlainChar 'H' Nothing,
                   DiffPlainChar 'e' $ Just EAInsert,
                   DiffPlainChar 'r' Nothing,
                   DiffPlainChar 'l' $ Just EADelete,
                   DiffPlainChar '~' $ Just EADelete,
                   DiffPlainChar 'x' $ Just EAInsert,
                   DiffPlainChar 'y' $ Just EAInsert,
                   DiffPlainChar '!' Nothing,
                   DiffPlainChar '@' Nothing,
                   DiffPlainChar 'l' $ Just EADelete,
                   DiffPlainChar 'z' $ Just EAInsert,
                   DiffPlainChar 'o' Nothing,
                   DiffPlainChar '1' $ Just EAInsert,
                   DiffPlainChar '2' $ Just EAInsert
                 ]