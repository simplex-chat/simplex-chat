{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Unavailable
  ( Unavailable,
    ListRefusal (..),
    readUnavailable,
    unavailableName,
    addUnavailable,
    removeUnavailable,
    normalizedName,
    matchesEntry,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Char (isAsciiLower, isDigit)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, renameFile)

data Unavailable = Unavailable {uFile :: Maybe FilePath, uEntries :: TVar (Set Text), uLock :: MVar ()}

data ListRefusal = LRNoFile | LRNoLetters | LRListed Text | LRNotListed Text
  deriving (Eq, Show)

normalizedName :: Text -> Text
normalizedName = T.map letterOf . T.filter (\c -> isAsciiLower c || isDigit c) . T.toLower
  where
    letterOf = \case
      '0' -> 'o'
      '1' -> 'l'
      '2' -> 'z'
      '3' -> 'e'
      '4' -> 'a'
      '5' -> 's'
      '6' -> 'b'
      '7' -> 't'
      '8' -> 'b'
      '9' -> 'g'
      c -> c

matchesEntry :: Text -> Text -> Bool
matchesEntry name entry
  | T.length entry < 5 = name == entry
  | otherwise = entry `T.isInfixOf` name

readUnavailable :: Maybe FilePath -> IO (Either String Unavailable)
readUnavailable uFile = do
  entries <- maybe (pure $ Right []) fileEntries uFile
  traverse (\es -> (\uEntries uLock -> Unavailable {uFile, uEntries, uLock}) <$> newTVarIO (S.fromList es) <*> newMVar ()) entries
  where
    fileEntries path =
      doesFileExist path >>= \case
        False -> pure $ Right []
        True -> fmap concat . traverse entryOf . zip [1 :: Int ..] . T.lines <$> TIO.readFile path
    entryOf (n, line)
      | T.null (T.strip line) = Right []
      | T.null entry = Left $ "names.unavailable_file: line " <> show n <> " has no letters or digits"
      | otherwise = Right [entry]
      where
        entry = normalizedName line

unavailableName :: Unavailable -> Text -> IO Bool
unavailableName Unavailable {uEntries} label = any (matchesEntry $ normalizedName label) <$> readTVarIO uEntries

addUnavailable :: Unavailable -> Text -> IO (Either ListRefusal Text)
addUnavailable u@Unavailable {uEntries} typed = changeList u typed $ \path entries entry ->
  if entry `S.member` entries
    then pure $ Left $ LRListed entry
    else Right entry <$ (TIO.appendFile path (entry <> "\n") >> atomically (modifyTVar' uEntries $ S.insert entry))

removeUnavailable :: Unavailable -> Text -> IO (Either ListRefusal Text)
removeUnavailable u@Unavailable {uEntries} typed = changeList u typed $ \path entries entry ->
  if entry `S.member` entries
    then do
      kept <- filter ((entry /=) . normalizedName) . T.lines <$> TIO.readFile path
      TIO.writeFile (path <> ".new") (T.unlines kept)
      renameFile (path <> ".new") path
      Right entry <$ atomically (modifyTVar' uEntries $ S.delete entry)
    else pure $ Left $ LRNotListed entry

changeList :: Unavailable -> Text -> (FilePath -> Set Text -> Text -> IO (Either ListRefusal Text)) -> IO (Either ListRefusal Text)
changeList Unavailable {uFile, uEntries, uLock} typed change = case uFile of
  Nothing -> pure $ Left LRNoFile
  Just path
    | T.null entry -> pure $ Left LRNoLetters
    | otherwise -> withMVar uLock $ \_ -> readTVarIO uEntries >>= \entries -> change path entries entry
  where
    entry = normalizedName typed
