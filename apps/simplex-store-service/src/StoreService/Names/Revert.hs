{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Revert
  ( RevertName (..),
    actedReverts,
    otherRevertSignatures,
    RevertReason (..),
    maxNodeMessageLength,
    decodeRevert,
    revertName,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import StoreService.Names.Abi (decodeString, hex, selector)

-- | The custom errors of SimplexController, SimplexSponsor, BaseRegistrarImplementation and SimplexResolver, and the reservation and terms errors of SimplexNamePolicy, that the service acts on, and RNOther for the others.
-- The sponsor passes the controller's and the base registrar's reverts through, so any call can revert with any of them.
data RevertName
  = RNCommitmentNotFound
  | RNCommitmentTooNew
  | RNCommitmentTooOld
  | RNDurationTooLong
  | RNDurationTooShort
  | RNInsufficientAllowance
  | RNInvalidNonce
  | RNInvalidSignature
  | RNLabelTooLong
  | RNNameNotAvailable
  | RNNameReserved
  | RNNameTooShort
  | RNReentrantCall
  | RNResolverRequiredWhenDataSupplied
  | RNReverseRecordNotSupported
  | RNSignatureExpired
  | RNTermsMismatch
  | RNTermsNotAccepted
  | RNOther Text
  deriving (Eq, Show)

data RevertReason = RRWithoutData | RRNamed RevertName | RRUnnamed Text
  deriving (Eq, Show)

revertSignature :: RevertName -> ByteString
revertSignature = \case
  RNCommitmentNotFound -> "CommitmentNotFound(bytes32)"
  RNCommitmentTooNew -> "CommitmentTooNew(bytes32,uint256,uint256)"
  RNCommitmentTooOld -> "CommitmentTooOld(bytes32,uint256,uint256)"
  RNDurationTooLong -> "DurationTooLong(uint256)"
  RNDurationTooShort -> "DurationTooShort(uint256)"
  RNInsufficientAllowance -> "InsufficientAllowance(uint256,uint256)"
  RNInvalidNonce -> "InvalidNonce()"
  RNInvalidSignature -> "InvalidSignature()"
  RNLabelTooLong -> "LabelTooLong(uint256,uint256)"
  RNNameNotAvailable -> "NameNotAvailable(string)"
  RNNameReserved -> "NameReserved(string)"
  RNNameTooShort -> "NameTooShort(string,uint8)"
  RNReentrantCall -> "ReentrantCall()"
  RNResolverRequiredWhenDataSupplied -> "ResolverRequiredWhenDataSupplied()"
  RNReverseRecordNotSupported -> "ReverseRecordNotSupported()"
  RNSignatureExpired -> "SignatureExpired()"
  RNTermsMismatch -> "TermsMismatch(bytes32,bytes32)"
  RNTermsNotAccepted -> "TermsNotAccepted(string)"
  RNOther name -> encodeUtf8 name

actedReverts :: [RevertName]
actedReverts =
  [ RNCommitmentNotFound,
    RNCommitmentTooNew,
    RNCommitmentTooOld,
    RNDurationTooLong,
    RNDurationTooShort,
    RNInsufficientAllowance,
    RNInvalidNonce,
    RNInvalidSignature,
    RNLabelTooLong,
    RNNameNotAvailable,
    RNNameReserved,
    RNNameTooShort,
    RNReentrantCall,
    RNResolverRequiredWhenDataSupplied,
    RNReverseRecordNotSupported,
    RNSignatureExpired,
    RNTermsMismatch,
    RNTermsNotAccepted
  ]

otherRevertSignatures :: [ByteString]
otherRevertSignatures =
  [ "AlreadyFrozen()",
    "Frozen()",
    "InsufficientValue()",
    "InvalidEVMAddress(bytes)",
    "MaxCommitmentAgeTooHigh()",
    "MaxCommitmentAgeTooLow()",
    "MinCharLengthCanOnlyDecrease()",
    "MinCharLengthZero()",
    "MinCommitmentAgeZero()",
    "NameNotReserved(string)",
    "NftGateCanOnlyBeDisabled()",
    "NftRequired()",
    "NodeNotOwnedByRegistrar()",
    "NotBeneficiary()",
    "NotController()",
    "NotNameOwner()",
    "NotOffered()",
    "NotOwnerOrBeneficiary()",
    "NotProxy()",
    "NotSponsor()",
    "NotSubnameRegistrar()",
    "OffsetOutOfBoundsError(uint256,uint256)",
    "OwnerMustBeCaller()",
    "PublicSalesClosed()",
    "ReasonRequired()",
    "RecipientMustBeCaller()",
    "TransferFailed()",
    "TransferToSelf()",
    "UnexpiredCommitmentExists(bytes32)",
    "ZeroAddress()"
  ]

signatureName :: ByteString -> Text
signatureName = decodeLatin1 . BC.takeWhile (/= '(')

instance TextEncoding RevertName where
  textEncode = signatureName . revertSignature
  textDecode = \case
    "CommitmentNotFound" -> Just RNCommitmentNotFound
    "CommitmentTooNew" -> Just RNCommitmentTooNew
    "CommitmentTooOld" -> Just RNCommitmentTooOld
    "DurationTooLong" -> Just RNDurationTooLong
    "DurationTooShort" -> Just RNDurationTooShort
    "InsufficientAllowance" -> Just RNInsufficientAllowance
    "InvalidNonce" -> Just RNInvalidNonce
    "InvalidSignature" -> Just RNInvalidSignature
    "LabelTooLong" -> Just RNLabelTooLong
    "NameNotAvailable" -> Just RNNameNotAvailable
    "NameReserved" -> Just RNNameReserved
    "NameTooShort" -> Just RNNameTooShort
    "ReentrantCall" -> Just RNReentrantCall
    "ResolverRequiredWhenDataSupplied" -> Just RNResolverRequiredWhenDataSupplied
    "ReverseRecordNotSupported" -> Just RNReverseRecordNotSupported
    "SignatureExpired" -> Just RNSignatureExpired
    "TermsMismatch" -> Just RNTermsMismatch
    "TermsNotAccepted" -> Just RNTermsNotAccepted
    name
      | name `elem` map signatureName otherRevertSignatures -> Just $ RNOther name
      | otherwise -> Nothing

revertName :: RevertReason -> Maybe RevertName
revertName = \case
  RRNamed n -> Just n
  _ -> Nothing

revertSelectors :: Map ByteString RevertName
revertSelectors = M.fromList $ [(selector $ revertSignature n, n) | n <- actedReverts] <> [(selector s, RNOther $ signatureName s) | s <- otherRevertSignatures]

-- | A node's error message is cut to this many characters.
maxNodeMessageLength :: Int
maxNodeMessageLength = 200

decodeRevert :: ByteString -> RevertReason
decodeRevert revertData
  | B.null revertData = RRWithoutData
  | Just n <- M.lookup revertSelector revertSelectors = RRNamed n
  | revertSelector == selector "Error(string)", Right message <- decodeString arguments = RRUnnamed $ T.take maxNodeMessageLength message
  | otherwise = RRUnnamed $ hex revertSelector
  where
    (revertSelector, arguments) = B.splitAt selectorSize revertData
    selectorSize = 4
