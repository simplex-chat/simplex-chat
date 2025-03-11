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

matchCaptchaStr :: T.Text -> T.Text -> Bool
matchCaptchaStr captcha guess = T.length captcha == T.length guess && matchChars (T.zip captcha guess)
  where
    matchChars [] = True
    matchChars ((c, g) : cs) =
      let g' = fromMaybe g $ M.lookup g captchaMatches
       in c == g' && matchChars cs

captchaChars :: String
captchaChars = "23456789ABCDEFGHIJKLMNOPQRSTUVWXYZabdefghijkmnpqrty"

captchaMatches :: M.Map Char Char
captchaMatches =
  M.fromList
    [ ('0', 'O'),
      ('1', 'I'),
      ('c', 'C'),
      ('l', 'I'),
      ('o', 'O'),
      ('s', 'S'),
      ('u', 'U'),
      ('v', 'V'),
      ('w', 'W'),
      ('x', 'X'),
      ('z', 'Z')
    ]
