module Simplex.Chat.Util where

import Control.Monad (when)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = decodeUtf8With onError
  where
    onError _ _ = Just '?'

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM ba t f = ba >>= \b -> if b then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM ba a = ba >>= (`when` a)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b $ pure ()

enumJSON :: (String -> String) -> J.Options
enumJSON tagModifier =
  J.defaultOptions
    { J.constructorTagModifier = tagModifier,
      J.allNullaryToStringTag = True
    }

singleFieldJSON :: (String -> String) -> J.Options
singleFieldJSON tagModifier =
  J.defaultOptions
    { J.constructorTagModifier = tagModifier,
      J.sumEncoding = J.ObjectWithSingleField,
      J.omitNothingFields = True
    }
