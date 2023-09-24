{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownDiffTests where

import qualified Data.Sequence as S
import Simplex.Chat.Markdown

import Simplex.Chat.MarkdownDiff
    ( FormatChar(..),
      DiffChar(..),
      DiffPlainChar(..),
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
          [ FormatChar 'H' Nothing                                          
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' Nothing                                                            
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' Nothing) $ UnchangedChar UnchangedFormat                                                              
        ]

  it "add 1 char to empty" do
    diff 
        (LeftSide $ S.fromList
          [                                          
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' Nothing                                                            
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' Nothing) Inserted                                                                 
        ]
     
  it "del the one and only" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar 'H' Nothing                                            
          ])   
        (RightSide $ S.fromList
          [                                                             
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' Nothing) Deleted                                                                 
        ]

  it "one character change" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar 'H' Nothing          
          , FormatChar 'r' Nothing                 
          , FormatChar 'l' Nothing           
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' Nothing           
          , FormatChar 'e' Nothing  
          , FormatChar 'l' Nothing           
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' Nothing                                                              
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' Nothing) $ UnchangedChar UnchangedFormat
        , DiffChar (FormatChar 'r' Nothing) Deleted          
        , DiffChar (FormatChar 'e' Nothing) Inserted      
        , DiffChar (FormatChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffChar (FormatChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffChar (FormatChar 'o' Nothing) $ UnchangedChar UnchangedFormat                                                        
        ]

  it "more1" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar 'H' Nothing          
          , FormatChar 'r' Nothing                 
          , FormatChar 'l' Nothing           
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' Nothing           
          , FormatChar 'e' Nothing  
          , FormatChar 'l' Nothing           
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' Nothing            
          , FormatChar 'x' Nothing                 
          , FormatChar 'y' Nothing                 
          , FormatChar 'z' Nothing                                                    
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' Nothing) $ UnchangedChar UnchangedFormat
        , DiffChar (FormatChar 'r' Nothing) Deleted          
        , DiffChar (FormatChar 'e' Nothing) Inserted      
        , DiffChar (FormatChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffChar (FormatChar 'l' Nothing) $ UnchangedChar UnchangedFormat 
        , DiffChar (FormatChar 'o' Nothing) $ UnchangedChar UnchangedFormat                                       
        , DiffChar (FormatChar 'x' Nothing) Inserted
        , DiffChar (FormatChar 'y' Nothing) Inserted
        , DiffChar (FormatChar 'z' Nothing) Inserted                   
        ]

  it "more2" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar 'H' Nothing          
          , FormatChar 'r' Nothing                 
          , FormatChar 'l' Nothing           
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' Nothing           
          , FormatChar 'e' Nothing  
          , FormatChar 'x' Nothing                 
          , FormatChar 'y' Nothing                 
          , FormatChar 'z' Nothing             
          , FormatChar 'o' Nothing                                         
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' Nothing) $ UnchangedChar UnchangedFormat
        , DiffChar (FormatChar 'r' Nothing) Deleted
        , DiffChar (FormatChar 'l' Nothing) Deleted  
        , DiffChar (FormatChar 'l' Nothing) Deleted          
        , DiffChar (FormatChar 'e' Nothing) Inserted
        , DiffChar (FormatChar 'x' Nothing) Inserted
        , DiffChar (FormatChar 'y' Nothing) Inserted
        , DiffChar (FormatChar 'z' Nothing) Inserted
        , DiffChar (FormatChar 'o' Nothing) $ UnchangedChar UnchangedFormat                      
        ]

  it "more3" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar 'H' $ Just Bold
          , FormatChar 'H' $ Just Bold          
          , FormatChar 'r' Nothing                 
          , FormatChar 'l' $ Just Secret          
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' $ Just $ colored Green                                       
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' $ Just Italic
          , FormatChar 'H' $ Just Bold          
          , FormatChar 'e' $ Just $ colored Cyan   
          , FormatChar 'x' Nothing                 
          , FormatChar 'y' Nothing                 
          , FormatChar 'z' $ Just Secret                 
          , FormatChar 'o' $ Just $ colored Blue                                       
          ])  
      `shouldBe` S.fromList
        [ DiffChar (FormatChar 'H' (Just Bold)) $ UnchangedChar $ ChangedToFormat $ Just Italic
        , DiffChar (FormatChar 'H' (Just Bold)) $ UnchangedChar UnchangedFormat
        , DiffChar (FormatChar 'r' Nothing) Deleted
        , DiffChar (FormatChar 'l' (Just Secret)) Deleted  
        , DiffChar (FormatChar 'l' Nothing) Deleted         
        , DiffChar (FormatChar 'e' (Just $ colored Cyan)) Inserted
        , DiffChar (FormatChar 'x' Nothing) Inserted
        , DiffChar (FormatChar 'y' Nothing) Inserted
        , DiffChar (FormatChar 'z' (Just Secret)) Inserted 
        , DiffChar (FormatChar 'o' (Just $ colored Green)) $ UnchangedChar $ ChangedToFormat $ Just $ colored Blue                                  
        ]

  it "more4" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar 'H' Nothing          
          , FormatChar 'r' Nothing                 
          , FormatChar 'l' Nothing      
          , FormatChar '~' Nothing  
          , FormatChar '!' Nothing  
          , FormatChar '@' Nothing                                     
          , FormatChar 'l' Nothing                  
          , FormatChar 'o' Nothing                                    
          ])   
        (RightSide $ S.fromList
          [ FormatChar 'H' Nothing           
          , FormatChar 'e' Nothing 
          , FormatChar 'r' Nothing             
          , FormatChar 'x' Nothing                 
          , FormatChar 'y' Nothing     
          , FormatChar '!' Nothing  
          , FormatChar '@' Nothing                       
          , FormatChar 'z' Nothing             
          , FormatChar 'o' Nothing      
          , FormatChar '1' Nothing 
          , FormatChar '2' Nothing                                                        
          ])  
      `shouldBe` S.fromList
          [ DiffChar (FormatChar 'H' Nothing) $ UnchangedChar UnchangedFormat
          , DiffChar (FormatChar 'e' Nothing) Inserted
          , DiffChar (FormatChar 'r' Nothing) $ UnchangedChar UnchangedFormat
          , DiffChar (FormatChar 'l' Nothing) Deleted
          , DiffChar (FormatChar '~' Nothing) Deleted
          , DiffChar (FormatChar 'x' Nothing) Inserted
          , DiffChar (FormatChar 'y' Nothing) Inserted
          , DiffChar (FormatChar '!' Nothing) $ UnchangedChar UnchangedFormat
          , DiffChar (FormatChar '@' Nothing) $ UnchangedChar UnchangedFormat
          , DiffChar (FormatChar 'l' Nothing) Deleted
          , DiffChar (FormatChar 'z' Nothing) Inserted
          , DiffChar (FormatChar 'o' Nothing) $ UnchangedChar UnchangedFormat
          , DiffChar (FormatChar '1' Nothing) Inserted
          , DiffChar (FormatChar '2' Nothing) Inserted
          ]

  it "SimplexLink 1" do
    diff 
        (LeftSide $ S.fromList
          [ FormatChar '>' $ Just $ SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]}                                                   
          ])   
        (RightSide $ S.fromList
          [ FormatChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/3/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host0", "host2", "host3"]
              }                                                   
          ])  
      `shouldBe` S.fromList
        [ DiffChar 
            (FormatChar '>' $ Just SimplexLink    
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
          [ FormatChar '>' $ Just $ SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]}                                                   
          ])   
        (RightSide $ S.fromList
          [ FormatChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/3/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]
              }                                                   
          ])  
      `shouldBe` S.fromList
        [ DiffChar 
            (FormatChar '>' $ Just SimplexLink    
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
          [ FormatChar '>' $ Just $ SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host1", "host2", "host3"]}                                                   
          ])   
        (RightSide $ S.fromList
          [ FormatChar '>' $ Just SimplexLink    
              { linkType = XLContact
              , simplexUri = "https://api.twitter.com/2/tweets/:id"
              , trustedUri = True
              , smpHosts = NE.fromList ["host0", "host2", "host3"]
              }                                                   
          ])  
      `shouldBe` S.fromList
        [ DiffChar 
            (FormatChar '>' $ Just SimplexLink    
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
            [ DiffPlainChar 'h' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 'p' UnchangedP
            , DiffPlainChar 's' UnchangedP
            , DiffPlainChar ':' UnchangedP
            , DiffPlainChar '/' UnchangedP
            , DiffPlainChar '/' UnchangedP
            , DiffPlainChar 'a' UnchangedP
            , DiffPlainChar 'p' UnchangedP
            , DiffPlainChar 'i' UnchangedP
            , DiffPlainChar '.' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 'w' UnchangedP
            , DiffPlainChar 'i' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 'e' UnchangedP
            , DiffPlainChar 'r' UnchangedP
            , DiffPlainChar '.' UnchangedP
            , DiffPlainChar 'c' UnchangedP
            , DiffPlainChar 'o' UnchangedP
            , DiffPlainChar 'm' UnchangedP
            , DiffPlainChar '/' UnchangedP
            , DiffPlainChar '2' DeletedP
            , DiffPlainChar '3' InsertedP
            , DiffPlainChar '/' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 'w' UnchangedP
            , DiffPlainChar 'e' UnchangedP
            , DiffPlainChar 'e' UnchangedP
            , DiffPlainChar 't' UnchangedP
            , DiffPlainChar 's' UnchangedP
            , DiffPlainChar '/' UnchangedP
            , DiffPlainChar ':' UnchangedP
            , DiffPlainChar 'i' UnchangedP
            , DiffPlainChar 'd' UnchangedP
            ]

  it "plainDiff 2" do
    plainDiff 
      (LeftSide  "Hrl~!@lo") 
      (RightSide "Herxy!@zo12")
        `shouldBe` S.fromList
            [ DiffPlainChar 'H' UnchangedP
            , DiffPlainChar 'e' InsertedP
            , DiffPlainChar 'r' UnchangedP
            , DiffPlainChar 'l' DeletedP
            , DiffPlainChar '~' DeletedP
            , DiffPlainChar 'x' InsertedP
            , DiffPlainChar 'y' InsertedP
            , DiffPlainChar '!' UnchangedP
            , DiffPlainChar '@' UnchangedP
            , DiffPlainChar 'l' DeletedP
            , DiffPlainChar 'z' InsertedP
            , DiffPlainChar 'o' UnchangedP
            , DiffPlainChar '1' InsertedP
            , DiffPlainChar '2' InsertedP
            ]