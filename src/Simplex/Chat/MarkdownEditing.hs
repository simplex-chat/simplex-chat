{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}


module Simplex.Chat.MarkdownEditing where

import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.ST ( runST )
import           Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import           Data.Foldable ( Foldable(toList) )
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics ( Generic )
import           Simplex.Messaging.Parsers ( sumTypeJSON ) 
import qualified Data.Diff.Myers as DM
import qualified Data.Vector.Unboxed as VU

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import           Simplex.Chat.Markdown ( FormattedText(..), Format )



data EditingOperation = Add | Delete | Substitute
  deriving (Show, Eq, Generic)

instance M.MVector M.MVector EditingOperation
instance G.Vector U.Vector EditingOperation
instance U.Unbox EditingOperation  
-- instance VU.Unbox (Maybe EditingOperation) 

data EditedChar = EditedChar 
  { format :: Maybe Format
  , char :: Char
  , operation :: Maybe EditingOperation
  }
  deriving (Show, Eq, Generic)

instance U.Unbox EditedChar  


-- TODO unused?
data EditedText =  EditedText 
  { format :: Maybe Format
  , text :: Text
  , added :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON EditedText where
  toEncoding :: EditedText -> J.Encoding
  toEncoding = J.genericToEncoding $ sumTypeJSON id


formattedEditedText :: [FormattedText] -> [FormattedText] -> [EditedChar]
formattedEditedText s s' = diff_ (toEditedChars s) (toEditedChars s')


toEditedChars :: [FormattedText] -> [EditedChar]
toEditedChars = concatMap toChars
  where
    toChars FormattedText {format, text} =
      map (\char -> EditedChar {format, char, operation = Nothing}) $ T.unpack text


-- TODO delete?
-- fromEditedChars :: [EditedChar] -> [EditedText]
-- fromEditedChars = reverse . foldl' addChar []
--   where
--     addChar :: [EditedText] -> EditedChar -> [EditedText]
--     addChar [] c = [toText c]
--     addChar ts@(t : rest) c
--       | sameFormat t c = appendChar t c : rest
--       | otherwise = toText c : ts

--     toText :: EditedChar -> EditedText
--     toText EditedChar {format, char, added} = EditedText {format, text = T.singleton char, added}
    
--     sameFormat :: EditedText -> EditedChar -> Bool
--     sameFormat EditedText {format, added} EditedChar {format = format', added = added'} = format == format' && added == added'
    
--     appendChar :: EditedText -> EditedChar -> EditedText
--     appendChar t@EditedText {text} EditedChar {char} = t {text = text <> T.singleton char}


diff_ :: [EditedChar] -> [EditedChar] -> [EditedChar]
diff_ left right = undefined


diff :: [EditedChar] -> [EditedChar] -> [DM.Edit] -- [EditedChar]
diff left right = toList $ runST $ do
  let l = U.fromList left 
  let r = U.fromList right 
  myersDiff l r


myersDiff :: PrimMonad m => U.Vector EditedChar -> U.Vector EditedChar -> m (Seq DM.Edit)
myersDiff left right = DM.diff left right