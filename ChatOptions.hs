module ChatOptions (getChatOpts, ChatOpts (..)) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Simplex.Messaging.Agent.Transmission (SMPServer (..), smpServerP)

data ChatOpts = ChatOpts
  { name :: Maybe B.ByteString,
    dbFileName :: String,
    smpServer :: SMPServer
  }

chatOpts :: Parser ChatOpts
chatOpts =
  ChatOpts
    <$> option
      parseName
      ( long "name"
          <> short 'n'
          <> metavar "NAME"
          <> help "optional name to use for invitations"
          <> value Nothing
      )
    <*> strOption
      ( long "database"
          <> short 'd'
          <> metavar "DB_FILE"
          <> help "sqlite database filename (smp-chat.db)"
          <> value "smp-chat.db"
      )
    <*> option
      parseSMPServer
      ( long "server"
          <> short 's'
          <> metavar "SERVER"
          <> help "SMP server to use (localhost:5223)"
          <> value (SMPServer "smp.simplex.im" (Just "5223") Nothing)
      )

parseName :: ReadM (Maybe B.ByteString)
parseName = maybeReader $ Just . Just . B.pack

parseSMPServer :: ReadM SMPServer
parseSMPServer = eitherReader $ A.parseOnly (smpServerP <* A.endOfInput) . B.pack

getChatOpts :: IO ChatOpts
getChatOpts = execParser opts
  where
    opts =
      info
        (chatOpts <**> helper)
        ( fullDesc
            <> header "Chat prototype using Simplex Messaging Protocol (SMP)"
            <> progDesc "Start chat with DB_FILE file and use SERVER as SMP server"
        )
