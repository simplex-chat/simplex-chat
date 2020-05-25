{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Simplex.Messaging.ServerAPI

import ClassyPrelude
import Servant
import Servant.Docs

apiDocs :: API
apiDocs = docsWith
            defaultDocOptions
            [serverApiIntro]
            serverApiExtra
            (Proxy :: Proxy ServerAPI)

main :: IO ()
main = writeFile "../simplex-messaging-api.md" $ fromString $ markdown apiDocs
