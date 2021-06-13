module Main where

import Control.Monad (void)
import Store (createStore)

main :: IO ()
main = void $ createStore "simplex-chat.db" 4
