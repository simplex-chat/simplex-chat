module Directory.Captcha (getCaptchaStr, matchCaptchaStr) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Random (randomRIO)

getCaptchaStr :: Int -> String -> IO String
getCaptchaStr 0 s = pure s
getCaptchaStr n s = do
  i <- randomRIO (0, length captchaChars - 1)
  let c = captchaChars !! i
  getCaptchaStr (n - 1) (c : s)
  where
    captchaChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

matchCaptchaStr :: T.Text -> T.Text -> Bool
matchCaptchaStr captcha guess = T.length captcha == T.length guess && matchChars (T.zip captcha guess)
  where
    matchChars [] = True
    matchChars ((c, g) : cs) = matchChar c == matchChar g && matchChars cs
    matchChar c = fromMaybe c $ M.lookup c captchaMatches
    captchaMatches =
      M.fromList
        [ ('0', 'O'),
          ('1', 'I'),
          ('c', 'C'),
          ('l', 'I'),
          ('o', 'O'),
          ('p', 'P'),
          ('s', 'S'),
          ('u', 'U'),
          ('v', 'V'),
          ('w', 'W'),
          ('x', 'X'),
          ('z', 'Z')
        ]
