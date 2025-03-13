{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal.Commands where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Controller
import Simplex.Chat.Library.Commands
import Simplex.Chat.Library.Internal
import Simplex.Chat.Library.Commands.Parsers
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types

data TerminalCommand
  = TtyChatCommand ChatCommand
  | SetActiveUser UserName (Maybe UserPwd)

parseTtyCommand :: ByteString -> Either String TerminalCommand
parseTtyCommand = A.parseOnly ttyCommandP . B.dropWhileEnd isSpace

ttyCommandP :: Parser TerminalCommand
ttyCommandP =
  cmdChoice
    [ ("/user " <|> "/u ") *> (SetActiveUser <$> displayNameP <*> optional (A.space *> pwdP)),
      TtyChatCommand <$> chatCommandP
    ]

allowRemoteTtyCommand :: TerminalCommand -> Bool
allowRemoteTtyCommand = \case
  TtyChatCommand cmd -> allowRemoteCommand cmd
  _ -> True

execTtyCommand :: Maybe RemoteHostId -> TerminalCommand -> ByteString -> CM' ChatResponse
execTtyCommand rh cmd s = do
  u <- readTVarIO =<< asks currentUser
  -- case parseChatCommand s of
    -- Left e -> pure $ chatCmdError u e
    -- Right cmd -> 
  case rh of
    Just rhId
      | allowRemoteCommand cmd -> execRemoteTtyCommand u rhId cmd s
      | otherwise -> pure $ CRChatCmdError u $ ChatErrorRemoteHost (RHId rhId) $ RHELocalCommand
    _ -> do
      cc@ChatController {config = ChatConfig {chatHooks}} <- ask
      case preCmdHook chatHooks of
        Just hook -> liftIO (hook cc cmd) >>= either pure (execTtyCommand_ u)
        Nothing -> execTtyCommand_ u cmd

execTtyCommand_ :: Maybe User -> TerminalCommand -> CM' ChatResponse
execTtyCommand_ u cmd = handleCommandError u $ processTtyCommand cmd

execRemoteTtyCommand :: Maybe User -> RemoteHostId -> TerminalCommand -> ByteString -> CM' ChatResponse
execRemoteTtyCommand u rhId cmd s = handleCommandError u $ getRemoteHostClient rhId >>= \rh -> processRemoteCommand rhId rh cmd s

processRemoteTtyCommand :: RemoteHostId -> RemoteHostClient -> TerminalCommand -> ByteString -> CM ChatResponse
processRemoteTtyCommand remoteHostId c cmd s = case cmd of
  TtyChatCommand (SendFile chatName f) -> sendFile "/f" chatName f
  TtyChatCommand (SendImage chatName f) -> sendFile "/img" chatName f
  _ -> liftRH remoteHostId $ remoteSend c s
  where
    sendFile cmdName chatName (CryptoFile path cfArgs) = do
      -- don't encrypt in host if already encrypted locally
      CryptoFile path' cfArgs' <- storeRemoteFile remoteHostId (cfArgs $> False) path
      let f = CryptoFile path' (cfArgs <|> cfArgs') -- use local or host encryption
      liftRH remoteHostId $ remoteSend c $ B.unwords [cmdName, B.pack (chatNameStr chatName), cryptoFileStr f]
    cryptoFileStr CryptoFile {filePath, cryptoArgs} =
      maybe "" (\(CFArgs key nonce) -> "key=" <> strEncode key <> " nonce=" <> strEncode nonce <> " ") cryptoArgs
        <> encodeUtf8 (T.pack filePath)

-- | Chat API commands interpreted in context of a local zone
processTtyCommand :: TerminalCommand -> CM ChatResponse
processTtyCommand cmd =
  chatVersionRange >>= (`processTtyCommand'` cmd)
{-# INLINE processTtyCommand #-}

processTtyCommand' :: VersionRangeChat -> TerminalCommand -> CM ChatResponse
processTtyCommand' vr = \case
  TtyChatCommand cmd -> processChatCommand vr cmd
  SetActiveUser uName viewPwd_ -> do
    tryChatError (withFastStore (`getUserIdByName` uName)) >>= \case
      Left _ -> throwChatError CEUserUnknown
      Right userId -> processChatCommand $ APISetActiveUser userId viewPwd_
