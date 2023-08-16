{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownEditingTests where

import qualified Data.Sequence as S
import Simplex.Chat.Markdown

import Simplex.Chat.MarkdownEditing
    ( FormattedChar(..),
      DiffedChar(..),
      DiffedPlainChar(..),
      DiffStatus(..),
      DiffPlainStatus(..),
      DiffFormatStatus(..),
      LeftSide(..),
      RightSide(..),
      findDiffs,
      findPlainDiffs )


import           System.Console.ANSI.Types
import           Test.Hspec
import qualified Data.List.NonEmpty as NE


markdownEditingTests :: Spec
markdownEditingTests = do
  formattedEditedTextTests


formattedEditedTextTests :: Spec
formattedEditedTextTests = describe "show edits" do
  it "empty no change" do
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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
    findDiffs 
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


  it "SimplexLink" do
    findDiffs 
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


  it "findPlainDiffs" do
    findPlainDiffs (LeftSide "Hrl~!@lo") (RightSide "Herxy!@zo12")
      `shouldBe` S.fromList
          [ DiffedPlainChar 'H' PlainUnchanged
          , DiffedPlainChar 'e' PlainInserted
          , DiffedPlainChar 'r' PlainUnchanged
          , DiffedPlainChar 'l' PlainDeleted
          , DiffedPlainChar '~' PlainDeleted
          , DiffedPlainChar 'x' PlainInserted
          , DiffedPlainChar 'y' PlainInserted
          , DiffedPlainChar '!' PlainUnchanged
          , DiffedPlainChar '@' PlainUnchanged
          , DiffedPlainChar 'l' PlainDeleted
          , DiffedPlainChar 'z' PlainInserted
          , DiffedPlainChar 'o' PlainUnchanged
          , DiffedPlainChar '1' PlainInserted
          , DiffedPlainChar '2' PlainInserted
          ]