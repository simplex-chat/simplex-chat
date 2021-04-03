{-# LANGUAGE LambdaCase #-}

module Types where

import Data.ByteString.Char8 (ByteString)

newtype Contact = Contact {toBs :: ByteString}

data TermMode = TermModeBasic | TermModeSimple | TermModeEditor deriving (Eq)

termModeName :: TermMode -> String
termModeName = \case
  TermModeBasic -> "basic"
  TermModeSimple -> "simple"
  TermModeEditor -> "editor"
