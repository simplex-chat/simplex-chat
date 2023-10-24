module Simplex.Chat.Messages.CIContent.Events.Prefix where

import Simplex.Messaging.Parsers (dropPrefix)

dropPrefixRGE :: String -> String
dropPrefixRGE = dropPrefix "RGE"

dropPrefixSGE :: String -> String
dropPrefixSGE = dropPrefix "SGE"

dropPrefixRCE :: String -> String
dropPrefixRCE = dropPrefix "RCE"

dropPrefixSCE :: String -> String
dropPrefixSCE = dropPrefix "SCE"

dropPrefixRDE :: String -> String
dropPrefixRDE = dropPrefix "RDE"
