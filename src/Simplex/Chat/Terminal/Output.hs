{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Terminal.Output where

import Control.Concurrent (ThreadId)
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Simplex.Chat (processChatCommand)
import Simplex.Chat.Controller
import Simplex.Chat.Messages hiding (NewChatItem (..))
import Simplex.Chat.Styled
import Simplex.Chat.View
import System.Console.ANSI.Types
import System.IO (IOMode (..), hPutStrLn, withFile)
import System.Mem.Weak (Weak)
import System.Terminal
import System.Terminal.Internal (LocalTerminal, Terminal, VirtualTerminal)
import UnliftIO.STM

data ChatTerminal = ChatTerminal
  { termDevice :: TerminalDevice,
    termState :: TVar TerminalState,
    termSize :: Size,
    liveMessageState :: TVar (Maybe LiveMessage),
    nextMessageRow :: TVar Int,
    termLock :: TMVar ()
  }

data TerminalState = TerminalState
  { inputPrompt :: String,
    inputString :: String,
    inputPosition :: Int,
    previousInput :: String,
    autoComplete :: AutoCompleteState
  }

data ACShowVariants = SVNone | SVSome | SVAll
  deriving (Eq, Enum)

data AutoCompleteState = ACState
  { acVariants :: [String],
    acInputString :: String,
    acTabPressed :: Bool,
    acShowVariants :: ACShowVariants
  }

data LiveMessage = LiveMessage
  { chatName :: ChatName,
    chatItemId :: ChatItemId,
    livePrompt :: Bool,
    sentMsg :: String,
    typedMsg :: String,
    liveThreadId :: Weak ThreadId,
    promptThreadId :: Weak ThreadId
  }

class Terminal t => WithTerminal t where
  withTerm :: (MonadIO m, MonadMask m) => t -> (t -> m a) -> m a

data TerminalDevice = forall t. WithTerminal t => TerminalDevice t

instance WithTerminal LocalTerminal where
  withTerm _ = withTerminal

instance WithTerminal VirtualTerminal where
  withTerm t = ($ t)

withChatTerm :: (MonadIO m, MonadMask m) => ChatTerminal -> (forall t. WithTerminal t => TerminalT t m a) -> m a
withChatTerm ChatTerminal {termDevice = TerminalDevice t} action = withTerm t $ runTerminalT action

newChatTerminal :: WithTerminal t => t -> IO ChatTerminal
newChatTerminal t = do
  termSize <- withTerm t . runTerminalT $ getWindowSize
  let lastRow = height termSize - 1
  termState <- newTVarIO mkTermState
  liveMessageState <- newTVarIO Nothing
  termLock <- newTMVarIO ()
  nextMessageRow <- newTVarIO lastRow
  -- threadDelay 500000 -- this delay is the same as timeout in getTerminalSize
  return ChatTerminal {termDevice = TerminalDevice t, termState, termSize, liveMessageState, nextMessageRow, termLock}

mkTermState :: TerminalState
mkTermState =
  TerminalState
    { inputString = "",
      inputPosition = 0,
      inputPrompt = "> ",
      previousInput = "",
      autoComplete = mkAutoComplete
    }

mkAutoComplete :: AutoCompleteState
mkAutoComplete = ACState {acVariants = [], acInputString = "", acTabPressed = False, acShowVariants = SVNone}

withTermLock :: MonadTerminal m => ChatTerminal -> m () -> m ()
withTermLock ChatTerminal {termLock} action = do
  _ <- atomically $ takeTMVar termLock
  action
  atomically $ putTMVar termLock ()

runTerminalOutput :: ChatTerminal -> ChatController -> IO ()
runTerminalOutput ct cc@ChatController {outputQ, showLiveItems, logFilePath} = do
  forever $ do
    (_, r) <- atomically $ readTBQueue outputQ
    case r of
      CRNewChatItem _ ci -> markChatItemRead ci
      CRChatItemUpdated _ ci -> markChatItemRead ci
      _ -> pure ()
    let printResp = case logFilePath of
          Just path -> if logResponseToFile r then logResponse path else printToTerminal ct
          _ -> printToTerminal ct
    liveItems <- readTVarIO showLiveItems
    responseString cc liveItems r >>= printResp
  where
    markChatItemRead (AChatItem _ _ chat item@ChatItem {chatDir, meta = CIMeta {itemStatus}}) =
      case (muted chat chatDir, itemStatus) of
        (False, CISRcvNew) -> do
          let itemId = chatItemId' item
              chatRef = chatInfoToRef chat
          void $ runReaderT (runExceptT $ processChatCommand (APIChatRead chatRef (Just (itemId, itemId)))) cc
        _ -> pure ()
    logResponse path s = withFile path AppendMode $ \h -> mapM_ (hPutStrLn h . unStyle) s

printRespToTerminal :: ChatTerminal -> ChatController -> Bool -> ChatResponse -> IO ()
printRespToTerminal ct cc liveItems r = responseString cc liveItems r >>= printToTerminal ct

responseString :: ChatController -> Bool -> ChatResponse -> IO [StyledString]
responseString cc liveItems r = do
  user <- readTVarIO $ currentUser cc
  ts <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ responseToView user (config cc) liveItems ts tz r

printToTerminal :: ChatTerminal -> [StyledString] -> IO ()
printToTerminal ct s =
  withChatTerm ct $
    withTermLock ct $ do
      printMessage ct s
      updateInput ct

updateInputView :: ChatTerminal -> IO ()
updateInputView ct = withChatTerm ct $ withTermLock ct $ updateInput ct

updateInput :: forall m. MonadTerminal m => ChatTerminal -> m ()
updateInput ChatTerminal {termSize = Size {height, width}, termState, nextMessageRow} = do
  hideCursor
  ts <- readTVarIO termState
  nmr <- readTVarIO nextMessageRow
  let ih = inputHeight ts
      iStart = height - ih
      prompt = inputPrompt ts
      acPfx = autoCompletePrefix ts
      Position {row, col} = positionRowColumn width $ length acPfx + length prompt + inputPosition ts
  if nmr >= iStart
    then atomically $ writeTVar nextMessageRow iStart
    else clearLines nmr iStart
  setCursorPosition $ Position {row = max nmr iStart, col = 0}
  putStyled $ Styled [SetColor Foreground Dull White] acPfx
  putString $ prompt <> inputString ts <> " "
  eraseInLine EraseForward
  setCursorPosition $ Position {row = iStart + row, col}
  showCursor
  flush
  where
    clearLines :: Int -> Int -> m ()
    clearLines from till
      | from >= till = return ()
      | otherwise = do
        setCursorPosition $ Position {row = from, col = 0}
        eraseInLine EraseForward
        clearLines (from + 1) till
    inputHeight :: TerminalState -> Int
    inputHeight ts = length (autoCompletePrefix ts <> inputPrompt ts <> inputString ts) `div` width + 1
    autoCompletePrefix :: TerminalState -> String
    autoCompletePrefix TerminalState {autoComplete = ac}
      | length vars <= 1 || sv == SVNone = ""
      | sv == SVAll || length vars <= 4 = "(" <> intercalate ", " vars <> ") "
      | otherwise = "(" <> intercalate ", " (take 3 vars) <> "... +" <> show (length vars - 3) <> ") "
      where
        sv = acShowVariants ac
        vars = acVariants ac
    positionRowColumn :: Int -> Int -> Position
    positionRowColumn wid pos =
      let row = pos `div` wid
          col = pos - row * wid
       in Position {row, col}

printMessage :: forall m. MonadTerminal m => ChatTerminal -> [StyledString] -> m ()
printMessage ChatTerminal {termSize = Size {height, width}, nextMessageRow} msg = do
  nmr <- readTVarIO nextMessageRow
  setCursorPosition $ Position {row = nmr, col = 0}
  mapM_ printStyled msg
  flush
  let lc = sum $ map lineCount msg
  atomically . writeTVar nextMessageRow $ min (height - 1) (nmr + lc)
  where
    lineCount :: StyledString -> Int
    lineCount s = sLength s `div` width + 1
    printStyled :: StyledString -> m ()
    printStyled s = do
      putStyled s
      eraseInLine EraseForward
      putLn

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
