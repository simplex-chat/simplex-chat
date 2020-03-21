module Main where

import Simplex.Messaging.ServerAPI
import Servant
import Servant.Docs

apiDocs :: API
apiDocs = docsWith
            defaultDocOptions
            [serverApiIntro]
            serverApiExtra
            (Proxy :: Proxy ServerAPI)

main :: IO ()
main = (writeFile "../simplex-messaging-api.md" . markdown) apiDocs
