{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Options
  ( BadgeServiceOpts (..),
    BadgeIssuerKey (..),
    getBadgeServiceOpts,
    badgeServiceOpts,
    mkChatOpts,
  )
where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Options.Applicative
import Simplex.Chat.Controller (updateStr, versionNumber, versionString)
import Simplex.Chat.Options (ChatCmdLog (..), ChatOpts (..), CoreChatOpts, CreateBotOpts (..), coreChatOptsP)
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Encoding.String (strDecode)

data BadgeServiceOpts = BadgeServiceOpts
  { coreOptions :: CoreChatOpts,
    serviceName :: T.Text,
    clientService :: Bool,
    noAddress :: Bool,
    runCLI :: Bool,
    serviceConfigFile :: Maybe FilePath,
    -- | `Left` when exactly one of the two flags was given, which is a mistake and not a
    -- request to fall back to the ini's [issuer] section.
    issuerKey :: Either String (Maybe BadgeIssuerKey),
    testing :: Bool
  }

-- | The issuer secret that signs credentials, and the index the apps find its public half under.
data BadgeIssuerKey = BadgeIssuerKey
  { keyIdx :: Int,
    secretKey :: BBSSecretKey
  }

-- BBSSecretKey derives Show, so this is written out to keep the secret out of logs and errors
instance Show BadgeIssuerKey where
  show BadgeIssuerKey {keyIdx} = "issuer key " <> show keyIdx

badgeServiceOpts :: FilePath -> FilePath -> Parser BadgeServiceOpts
badgeServiceOpts appDir defaultDbName = do
  coreOptions <- coreChatOptsP appDir defaultDbName
  serviceName <-
    strOption
      ( long "service-name"
          <> metavar "SERVICE_NAME"
          <> help "The display name of the badge service bot, without *'s and spaces (SimpleX Badges)"
          <> value "SimpleX Badges"
      )
  clientService <-
    switch
      ( long "client-service"
          <> help "Use client service certificate"
      )
  noAddress <-
    switch
      ( long "no-address"
          <> help "skip checking and creating service address"
      )
  runCLI <-
    switch
      ( long "run-cli"
          <> help "Run badge service as CLI"
      )
  serviceConfigFile <-
    optional
      ( strOption
          ( long "service-config"
              <> metavar "INI_FILE"
              <> help "Path to badge_service.ini: parsed in full in every mode, but --run-cli starts no web listener"
          )
      )
  issuerKeyIdx <-
    optional $
      option
        auto
        ( long "issuer-key-idx"
            <> metavar "KEY_IDX"
            <> help "Index of the issuer key in the app config (required with --issuer-secret)"
        )
  issuerSecret <-
    optional $
      option
        (eitherReader $ strDecode . B.pack)
        ( long "issuer-secret"
            <> metavar "ISSUER_SECRET"
            <> help "Issuer secret from `simplex-chat badge keygen` (base64url)"
        )
  pure
    BadgeServiceOpts
      { coreOptions,
        serviceName = T.pack serviceName,
        clientService,
        noAddress,
        runCLI,
        serviceConfigFile,
        issuerKey = case (issuerKeyIdx, issuerSecret) of
          (Just idx, Just secret) -> Right (Just (BadgeIssuerKey idx secret))
          (Nothing, Nothing) -> Right Nothing
          _ -> Left "--issuer-key-idx and --issuer-secret are given together or not at all",
        testing = False
      }

getBadgeServiceOpts :: FilePath -> FilePath -> IO BadgeServiceOpts
getBadgeServiceOpts appDir defaultDbName =
  execParser $
    info
      (helper <*> versionOption <*> badgeServiceOpts appDir defaultDbName)
      (header versionStr <> fullDesc <> progDesc "Start SimpleX Badge Service with DB_FILE options")
  where
    versionStr = versionString versionNumber
    versionOption = infoOption versionAndUpdate (long "version" <> short 'v' <> help "Show version")
    versionAndUpdate = versionStr <> "\n" <> updateStr

mkChatOpts :: BadgeServiceOpts -> ChatOpts
mkChatOpts BadgeServiceOpts {coreOptions, serviceName, clientService} =
  ChatOpts
    { coreOptions,
      chatCmd = "",
      chatCmdDelay = 3,
      chatCmdLog = CCLNone,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      optTempDirectory = Nothing,
      showReactions = False,
      showFullLinks = False,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      markRead = False,
      createBot = Just CreateBotOpts {botDisplayName = serviceName, allowFiles = False, clientService},
      userDisplayName = Nothing,
      userImageFile = Nothing
    }
