-- TODO: move to simplexmq

module Simplex.Chat.Credentials where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as Memory
import Data.Hourglass (Hours(..), timeAdd)
import qualified Data.X509 as X509
import Data.X509.Validation (Fingerprint, getFingerprint)
import qualified Network.TLS as TLS
import qualified Time.System as Hourglass

genTlsCredentials :: IO (Fingerprint, TLS.Credentials)
genTlsCredentials = do
  secret <- Ed25519.generateSecretKey
  let public = Ed25519.toPublic secret

  today <- Hourglass.dateCurrent
  let
    validity =
      ( timeAdd today (-25 :: Hours)
      , timeAdd today (365 * 24 :: Hours)
      )

  let
    certificate = X509.Certificate
      { X509.certVersion      = 1
      , X509.certSerial       = 1
      , X509.certSignatureAlg = X509.SignatureALG_IntrinsicHash X509.PubKeyALG_Ed25519
      , X509.certIssuerDN     = mempty
      , X509.certValidity     = validity
      , X509.certSubjectDN    = mempty
      , X509.certPubKey       = X509.PubKeyEd25519 public
      , X509.certExtensions   = X509.Extensions Nothing
      }
    (signed, ()) =
      X509.objectToSignedExact
        ( \bytes ->
            ( Memory.convert $ Ed25519.sign secret public bytes
            , X509.SignatureALG_IntrinsicHash X509.PubKeyALG_Ed25519
            , ()
            )
        )
        certificate

  pure
    ( getFingerprint signed X509.HashSHA256
    , TLS.Credentials
        [ ( X509.CertificateChain [signed] -- BUG: Client.validateCertificateChain enforces 2-certificate chain of [_, caCert]
          , X509.PrivKeyEd25519 secret
          )
        ]
    )
