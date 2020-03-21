module Main where

import SimplexAPI
import Servant
import Servant.Docs

simplexApi :: Proxy SimplexAPI
simplexApi = Proxy

apiDocs :: API
apiDocs = docsWith defaultDocOptions [simplexApiIntro] simplexApiExtra simplexApi

main :: IO ()
main = (writeFile "../simplex-messaging-api.md" . markdown) apiDocs
