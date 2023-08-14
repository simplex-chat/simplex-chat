{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownEditingTests where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Sequence as S
import Data.Text (Text)
import Simplex.Chat.Markdown
    ( colored, Format(Secret, Italic, Bold) )
import Simplex.Chat.MarkdownEditing
    ( FormattedChar(FormattedChar),
      DiffedChar(DiffedChar),
      DiffStatus(UnchangedTextually, Inserted, Deleted),
      findDiffs,
      DiffUnchangedTextuallyStatus(ChangedToFormat, Pristine) )
import System.Console.ANSI.Types
import Test.Hspec


markdownEditingTests :: Spec
markdownEditingTests = do
  formattedEditedTextTests


formattedEditedTextTests :: Spec
formattedEditedTextTests = describe "show edits" do
  it "no change" do
    findDiffs 
        (S.fromList
          [ FormattedChar 'H' Nothing                                          
          ])   

        (S.fromList
          [ FormattedChar 'H' Nothing                                                            
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine                                                              
        ]


  it "add 1 char to empty" do
    findDiffs 
        (S.fromList
          [                                          
          ])   

        (S.fromList
          [ FormattedChar 'H' Nothing                                                            
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) Inserted                                                                 
        ]
     

  it "del the one and only" do
    findDiffs 
        (S.fromList
          [ FormattedChar 'H' Nothing                                            
          ])   

        (S.fromList
          [                                                             
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) Deleted                                                                 
        ]


  it "one character change" do
    findDiffs 
        (S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                                              
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'r' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted      
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedTextually Pristine                                                        
        ]


  it "more1" do
    findDiffs 
        (S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (S.fromList
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
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'r' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted      
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedTextually Pristine                                       
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' Nothing) Inserted                   
        ]


  it "more2" do
    findDiffs 
        (S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' Nothing             
          , FormattedChar 'o' Nothing                                         
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'e' Nothing) Inserted
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' Nothing) Inserted
        , DiffedChar (FormattedChar 'r' Nothing) Deleted
        , DiffedChar (FormattedChar 'l' Nothing) Deleted  
        , DiffedChar (FormattedChar 'l' Nothing) Deleted  
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedTextually Pristine                      
        ]


  it "more3" do
    findDiffs 
        (S.fromList
          [ FormattedChar 'H' (Just Bold)    
          , FormattedChar 'H' (Just Bold)          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' (Just Secret)           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' (Just $ colored Green)                                       
          ])   

        (S.fromList
          [ FormattedChar 'H' (Just Italic)   
          , FormattedChar 'H' (Just Bold)          
          , FormattedChar 'e' (Just $ colored Cyan)    
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' (Just Secret)                  
          , FormattedChar 'o' (Just $ colored Blue)                                         
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' (Just Bold)) $ UnchangedTextually (ChangedToFormat (Just Italic)) 
        , DiffedChar (FormattedChar 'H' (Just Bold)) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'e' (Just $ colored Cyan)) Inserted
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' (Just Secret)) Inserted
        , DiffedChar (FormattedChar 'r' Nothing) Deleted
        , DiffedChar (FormattedChar 'l' (Just Secret)) Deleted  
        , DiffedChar (FormattedChar 'l' Nothing) Deleted  
        , DiffedChar (FormattedChar 'o' (Just $ colored Green)) $ UnchangedTextually (ChangedToFormat (Just $ colored Blue))                                  
        ]