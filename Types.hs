{-# LANGUAGE LambdaCase #-}

module Types where

import Data.ByteString.Char8 (ByteString)

newtype Contact = Contact {toBs :: ByteString} deriving (Eq)

data TermMode = TermModeBasic | TermModeEditor deriving (Eq)

termModeName :: TermMode -> String
termModeName = \case
  TermModeBasic -> "basic"
  TermModeEditor -> "editor"
