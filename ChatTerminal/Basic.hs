{-# LANGUAGE LambdaCase #-}

module ChatTerminal.Basic where

import Control.Monad.IO.Class (liftIO)
import Styled
import System.Console.ANSI.Types
import System.Exit (exitSuccess)
import System.Terminal as C

getLn :: IO String
getLn = withTerminal $ runTerminalT getTermLine

putStyledLn :: StyledString -> IO ()
putStyledLn s =
  withTerminal . runTerminalT $
    putStyled s >> C.putLn >> flush

-- Currently it is assumed that the message does not have internal line breaks.
-- Previous implementation "kind of" supported them,
-- but it was not determining the number of printed lines correctly
-- because of accounting for control sequences in length
putStyled :: MonadTerminal m => StyledString -> m ()
putStyled (s1 :<>: s2) = putStyled s1 >> putStyled s2
putStyled (Styled [] s) = putString s
putStyled (Styled sgr s) = setSGR sgr >> putString s >> resetAttributes

setSGR :: MonadTerminal m => [SGR] -> m ()
setSGR = mapM_ $ \case
  Reset -> resetAttributes
  SetConsoleIntensity BoldIntensity -> setAttribute bold
  SetConsoleIntensity _ -> resetAttribute bold
  SetItalicized True -> setAttribute italic
  SetItalicized _ -> resetAttribute italic
  SetUnderlining NoUnderline -> resetAttribute underlined
  SetUnderlining _ -> setAttribute underlined
  SetSwapForegroundBackground True -> setAttribute inverted
  SetSwapForegroundBackground _ -> resetAttribute inverted
  SetColor l i c -> setAttribute . layer l . intensity i $ color c
  SetBlinkSpeed _ -> pure ()
  SetVisible _ -> pure ()
  SetRGBColor _ _ -> pure ()
  SetPaletteColor _ _ -> pure ()
  SetDefaultColor _ -> pure ()
  where
    layer = \case
      Foreground -> foreground
      Background -> background
    intensity = \case
      Dull -> id
      Vivid -> bright
    color = \case
      Black -> black
      Red -> red
      Green -> green
      Yellow -> yellow
      Blue -> blue
      Magenta -> magenta
      Cyan -> cyan
      White -> white

getKey :: MonadTerminal m => m (Key, Modifiers)
getKey =
  awaitEvent >>= \case
    Left Interrupt -> liftIO exitSuccess
    Right (KeyEvent key ms) -> pure (key, ms)
    _ -> getKey

getTermLine :: MonadTerminal m => m String
getTermLine = getChars ""
  where
    getChars s =
      getKey >>= \(key, ms) -> case key of
        CharKey c
          | ms == mempty || ms == shiftKey -> do
            C.putChar c
            flush
            getChars (c : s)
          | otherwise -> getChars s
        EnterKey -> do
          C.putLn
          flush
          pure $ reverse s
        BackspaceKey -> do
          moveCursorBackward 1
          eraseChars 1
          flush
          getChars $ if null s then s else tail s
        _ -> getChars s
