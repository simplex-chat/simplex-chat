{-# LANGUAGE OverloadedStrings #-}

-- TODO: move to simplexmq

module Simplex.Chat.Credentials where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ASN1.Types (getObjectID)
import qualified Data.ByteArray as Memory
import Data.Hourglass (Hours(..), timeAdd)
import qualified Data.X509 as X509
import Data.X509.Validation (Fingerprint, getFingerprint)
import qualified Network.TLS as TLS
import qualified Time.System as Hourglass

genTlsCredentials :: IO (Fingerprint, TLS.Credentials)
genTlsCredentials = do
  today <- Hourglass.dateCurrent
  let
    validity =
      ( timeAdd today (-25 :: Hours)
      , timeAdd today (365 * 24 :: Hours)
      )

  rootSecret <- Ed25519.generateSecretKey
  let
    rootPublic = Ed25519.toPublic rootSecret
    rootDN = X509.DistinguishedName
      [ (getObjectID X509.DnCommonName, "Root")
      ]
    root = X509.Certificate
      { X509.certVersion      = 2
      , X509.certSerial       = 1
      , X509.certSignatureAlg = X509.SignatureALG_IntrinsicHash X509.PubKeyALG_Ed25519
      , X509.certIssuerDN     = rootDN
      , X509.certValidity     = validity
      , X509.certSubjectDN    = rootDN
      , X509.certPubKey       = X509.PubKeyEd25519 rootPublic
      , X509.certExtensions   = X509.Extensions Nothing
      }
    (signedRoot, _rootBytes) =
      X509.objectToSignedExact
        ( \bytes ->
            ( Memory.convert $ Ed25519.sign rootSecret rootPublic bytes
            , X509.SignatureALG_IntrinsicHash X509.PubKeyALG_Ed25519
            , bytes
            )
        )
        root

  entitySecret <- Ed25519.generateSecretKey
  let
    entityPublic = Ed25519.toPublic entitySecret
    entityDN = X509.DistinguishedName
      [ (getObjectID X509.DnCommonName, "Entity")
      ]
    entity = X509.Certificate
      { X509.certVersion      = 2
      , X509.certSerial       = 1
      , X509.certSignatureAlg = X509.SignatureALG_IntrinsicHash X509.PubKeyALG_Ed25519
      , X509.certIssuerDN     = rootDN
      , X509.certValidity     = validity
      , X509.certSubjectDN    = entityDN
      , X509.certPubKey       = X509.PubKeyEd25519 entityPublic
      , X509.certExtensions   = X509.Extensions Nothing
      }
    (signedEntity, _entityBytes) =
      X509.objectToSignedExact
        ( \bytes ->
            ( Memory.convert $ Ed25519.sign rootSecret rootPublic bytes
            , X509.SignatureALG_IntrinsicHash X509.PubKeyALG_Ed25519
            , bytes
            )
        )
        entity

  let rootFingerprint = getFingerprint signedRoot X509.HashSHA256
  let chain = X509.CertificateChain [signedEntity, signedRoot]

  pure
    ( rootFingerprint
    , TLS.Credentials
        [ ( chain
          , X509.PrivKeyEd25519 entitySecret
          )
        ]
    )
