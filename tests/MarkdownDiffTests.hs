{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownDiffTests where

import qualified Data.Sequence as S
import Simplex.Chat.Markdown

import Simplex.Chat.MarkdownDiff
    ( FormattedChar(..),
      DiffedChar(..),
      DiffedPlainChar(..),
      DiffStatus(..),
      DiffPlainStatus(..),
      DiffFormatStatus(..),
      LeftSide(..),
      RightSide(..),
      diff,
      plainDiff )


import           System.Console.ANSI.Types
import           Test.Hspec
import qualified Data.List.NonEmpty as NE


markdownDiffTests :: Spec
markdownDiffTests = do
  formattedEditedTextTests


formattedEditedTextTests :: Spec
formattedEditedTextTests = describe "show edits" do
  it "empty no change" do
    diff 
        (LeftSide $ S.fromList
          [                                         
          ])   
        (RightSide $ S.fromList
          [                                                       
          ])  
      `shouldBe` S.fromList
        [                                                             
        ]

  it "no change" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing                                          
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing                                                            
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedChar UnchangedFormat                                                              
        ]

  it "add 1 char to empty" do
    diff 
        (LeftSide $ S.fromList
          [                                          
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing                                                            
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) Inserted                                                                 
        ]
     
  it "del the one and only" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing                                            
          ])   
        (RightSide $ S.fromList
          [                                                             
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) Deleted                                                                 
        ]

  it "one character change" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                                              
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedChar UnchangedFormat
        , DiffedChar (FormattedChar 'r' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted      
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedChar UnchangedFormat                                                        
        ]

  it "more1" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing            
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' Nothing                                                    
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedChar UnchangedFormat
        , DiffedChar (FormattedChar 'r' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted      
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedChar UnchangedFormat                                       
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' Nothing) Inserted                   
        ]

  it "more2" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' Nothing             
          , FormattedChar 'o' Nothing                                         
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedChar UnchangedFormat
        , DiffedChar (FormattedChar 'r' Nothing) Deleted
        , DiffedChar (FormattedChar 'l' Nothing) Deleted  
        , DiffedChar (FormattedChar 'l' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' Nothing) Inserted
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedChar UnchangedFormat                      
        ]

  it "more3" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' (Just Bold)    
          , FormattedChar 'H' (Just Bold)          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' (Just Secret)           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' (Just $ colored Green)                                       
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' (Just Italic)   
          , FormattedChar 'H' (Just Bold)          
          , FormattedChar 'e' (Just $ colored Cyan)    
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' (Just Secret)                  
          , FormattedChar 'o' (Just $ colored Blue)                                         
          ])  
      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' (Just Bold)) $ UnchangedChar (ChangedToFormat (Just Italic)) 
        , DiffedChar (FormattedChar 'H' (Just Bold)) $ UnchangedChar UnchangedFormat
        , DiffedChar (FormattedChar 'r' Nothing) Deleted
        , DiffedChar (FormattedChar 'l' (Just Secret)) Deleted  
        , DiffedChar (FormattedChar 'l' Nothing) Deleted         
        , DiffedChar (FormattedChar 'e' (Just $ colored Cyan)) Inserted
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' (Just Secret)) Inserted 
        , DiffedChar (FormattedChar 'o' (Just $ colored Green)) $ UnchangedChar (ChangedToFormat (Just $ colored Blue))                                  
        ]

  it "more4" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing      
          , FormattedChar '~' Nothing  
          , FormattedChar '!' Nothing  
          , FormattedChar '@' Nothing                                     
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing 
          , FormattedChar 'r' Nothing             
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing     
          , FormattedChar '!' Nothing  
          , FormattedChar '@' Nothing                       
          , FormattedChar 'z' Nothing             
          , FormattedChar 'o' Nothing      
          , FormattedChar '1' Nothing 
          , FormattedChar '2' Nothing                                                        
          ])  
      `shouldBe` S.fromList
          [ DiffedChar (FormattedChar 'H' Nothing) (UnchangedChar UnchangedFormat)
          , DiffedChar (FormattedChar 'e' Nothing) Inserted
          , DiffedChar (FormattedChar 'r' Nothing) (UnchangedChar UnchangedFormat)
          , DiffedChar (FormattedChar 'l' Nothing) Deleted
          , DiffedChar (FormattedChar '~' Nothing) Deleted
          , DiffedChar (FormattedChar 'x' Nothing) Inserted
          , DiffedChar (FormattedChar 'y' Nothing) Inserted
          , DiffedChar (FormattedChar '!' Nothing) (UnchangedChar UnchangedFormat)
          , DiffedChar (FormattedChar '@' Nothing) (UnchangedChar UnchangedFormat)
          , DiffedChar (FormattedChar 'l' Nothing) Deleted
          , DiffedChar (FormattedChar 'z' Nothing) Inserted
          , DiffedChar (FormattedChar 'o' Nothing) (UnchangedChar UnchangedFormat)
          , DiffedChar (FormattedChar '1' Nothing) Inserted
          , DiffedChar (FormattedChar '2' Nothing) Inserted
          ]

  it "SimplexLink 1" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar '>' $ Just $ SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]}                                                   
          ])   
        (RightSide $ S.fromList
          [ FormattedChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/3/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host0", "host2", "host3"]
              }                                                   
          ])  
      `shouldBe` S.fromList
        [ DiffedChar 
            (FormattedChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]
              } 
            ) $           
            UnchangedChar $ ChangedToFormat $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/3/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host0", "host2", "host3"]
              }                                                      
        ]

  it "SimplexLink 2" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar '>' $ Just $ SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]}                                                   
          ])   
        (RightSide $ S.fromList
          [ FormattedChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/3/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]
              }                                                   
          ])  
      `shouldBe` S.fromList
        [ DiffedChar 
            (FormattedChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]
              } 
            ) $           
            UnchangedChar $ ChangedToFormat $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/3/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]
              }                                                      
        ]

  it "SimplexLink 3" do
    diff 
        (LeftSide $ S.fromList
          [ FormattedChar '>' $ Just $ SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]}                                                   
          ])   
        (RightSide $ S.fromList
          [ FormattedChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host0", "host2", "host3"]
              }                                                   
          ])  
      `shouldBe` S.fromList
        [ DiffedChar 
            (FormattedChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]
              } 
            ) $           
            UnchangedChar $ ChangedToFormat $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host0", "host2", "host3"]
              }                                                      
        ]

  it "plainDiff 1" do
    plainDiff 
      (LeftSide  "https://api.twitter.com/2/tweets/:id") 
      (RightSide "https://api.twitter.com/3/tweets/:id")
        `shouldBe` S.fromList
            [ DiffedPlainChar 'h' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 'p' UnchangedP
            , DiffedPlainChar 's' UnchangedP
            , DiffedPlainChar ':' UnchangedP
            , DiffedPlainChar '/' UnchangedP
            , DiffedPlainChar '/' UnchangedP
            , DiffedPlainChar 'a' UnchangedP
            , DiffedPlainChar 'p' UnchangedP
            , DiffedPlainChar 'i' UnchangedP
            , DiffedPlainChar '.' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 'w' UnchangedP
            , DiffedPlainChar 'i' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 'e' UnchangedP
            , DiffedPlainChar 'r' UnchangedP
            , DiffedPlainChar '.' UnchangedP
            , DiffedPlainChar 'c' UnchangedP
            , DiffedPlainChar 'o' UnchangedP
            , DiffedPlainChar 'm' UnchangedP
            , DiffedPlainChar '/' UnchangedP
            , DiffedPlainChar '2' DeletedP
            , DiffedPlainChar '3' InsertedP
            , DiffedPlainChar '/' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 'w' UnchangedP
            , DiffedPlainChar 'e' UnchangedP
            , DiffedPlainChar 'e' UnchangedP
            , DiffedPlainChar 't' UnchangedP
            , DiffedPlainChar 's' UnchangedP
            , DiffedPlainChar '/' UnchangedP
            , DiffedPlainChar ':' UnchangedP
            , DiffedPlainChar 'i' UnchangedP
            , DiffedPlainChar 'd' UnchangedP
            ]

  it "plainDiff 2" do
    plainDiff 
      (LeftSide  "Hrl~!@lo") 
      (RightSide "Herxy!@zo12")
        `shouldBe` S.fromList
            [ DiffedPlainChar 'H' UnchangedP
            , DiffedPlainChar 'e' InsertedP
            , DiffedPlainChar 'r' UnchangedP
            , DiffedPlainChar 'l' DeletedP
            , DiffedPlainChar '~' DeletedP
            , DiffedPlainChar 'x' InsertedP
            , DiffedPlainChar 'y' InsertedP
            , DiffedPlainChar '!' UnchangedP
            , DiffedPlainChar '@' UnchangedP
            , DiffedPlainChar 'l' DeletedP
            , DiffedPlainChar 'z' InsertedP
            , DiffedPlainChar 'o' UnchangedP
            , DiffedPlainChar '1' InsertedP
            , DiffedPlainChar '2' InsertedP
            ]