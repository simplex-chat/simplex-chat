module StoreService.Names.View
  ( addressText,
    tldText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (SimplexTLD)
import Simplex.Messaging.Util (safeDecodeUtf8)

-- | EIP-55, as the page compares owners.
addressText :: Address -> Text
addressText = safeDecodeUtf8 . strEncode

-- | The TLD as the page shows it after the name's dot.
tldText :: SimplexTLD -> Text
tldText = T.dropWhile (== '.') . safeDecodeUtf8 . strEncode
