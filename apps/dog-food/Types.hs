module Types where

import Data.ByteString.Char8 (ByteString)

newtype Contact = Contact {toBs :: ByteString} deriving (Eq, Show)

newtype Group = Group {fromGroup :: ByteString} deriving (Eq, Show)
