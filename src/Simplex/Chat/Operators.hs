{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Operators
  ( conditionsCommit,
    conditionsText,
  )
where

import Data.FileEmbed
import Data.Text (Text)
import Language.Haskell.TH.Syntax (lift)
import Simplex.Chat.Operators.Conditions
import Simplex.Messaging.Util (safeDecodeUtf8)

conditionsCommit :: Text
conditionsCommit = "165143a1112308c035ac00ed669b96b60599aa1c"

conditionsText :: Text
conditionsText =
  $( let s = $(embedFile =<< makeRelativeToProject "PRIVACY.md")
      in [| stripFrontMatter (safeDecodeUtf8 $(lift s)) |]
   )
