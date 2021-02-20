module Types where

import Data.ByteString.Char8 (ByteString)

newtype Contact = Contact {toBs :: ByteString}
