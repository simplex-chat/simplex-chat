{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Signed
  ( OrderSignature,
    querySignature,
    bodySignature,
    outsideWindow,
    signedByOwner,
    ownerSigned,
  )
where

import Control.Monad (join)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LB
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Network.Wai (Request, queryString)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Eth.Address (recoverAddress)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (eitherToMaybe)
import StoreService.Config (boundedDecimal)
import StoreService.Names.Orders (checkTimestamp)
import StoreService.Names.Signatures (OrderAction, orderRequestMessage, personalDigest)
import StoreService.Names.Store (NameRegistration (..))

data OrderSignature = OrderSignature
  { osTimestamp :: Maybe Word64,
    osSignature :: Maybe RecoverableSignature
  }

querySignature :: Request -> OrderSignature
querySignature req =
  OrderSignature
    { osTimestamp = param "timestamp" >>= eitherToMaybe . parseAll (boundedDecimal 0 maxBound),
      osSignature = param "signature" >>= eitherToMaybe . strDecode
    }
  where
    param k = join $ lookup k $ queryString req

bodySignature :: Maybe LB.ByteString -> OrderSignature
bodySignature body = OrderSignature {osTimestamp = field "timestamp", osSignature = field "signature"}
  where
    object = body >>= J.decode :: Maybe J.Object
    field :: J.FromJSON a => J.Key -> Maybe a
    field k = object >>= KM.lookup k >>= JT.parseMaybe J.parseJSON

outsideWindow :: UTCTime -> OrderSignature -> Bool
outsideWindow now OrderSignature {osTimestamp} = maybe False (isLeft . checkTimestamp now) osTimestamp

signedByOwner :: Text -> OrderAction -> OrderSignature -> NameRegistration -> IO Bool
signedByOwner oid action OrderSignature {osTimestamp, osSignature} NameRegistration {nrOwner} = case (osTimestamp, osSignature) of
  (Just timestamp, Just signature) -> (== Right nrOwner) <$> recoverAddress signature (personalDigest $ orderRequestMessage oid action timestamp)
  _ -> pure False

ownerSigned :: Text -> OrderAction -> OrderSignature -> Maybe NameRegistration -> IO (Maybe NameRegistration)
ownerSigned oid action signed = \case
  Just r -> (\owned -> if owned then Just r else Nothing) <$> signedByOwner oid action signed r
  Nothing -> pure Nothing
