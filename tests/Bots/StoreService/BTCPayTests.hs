{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.StoreService.BTCPayTests (badgeBTCPayTests) where

import StoreService.Config (BTCPayConfig (..))
import StoreService.Providers
  ( Funded (..),
    ListPass (..),
    OrderDraft (..),
    PaymentSignal (..),
    Provider (..),
    ProviderError (..),
    ProviderInvoice (..),
    Rate (..),
    Received (..),
    Confirmed (..),
    Uncreditable (..),
    WebhookError (..),
    settleWindow,
  )
import StoreService.Providers.BTCPay (btcMethodId, btcpayProvider, listSignals, minorToDecimal, paymentMethodsSignal, verifyBTCPaySig, xmrMethodId)
import StoreService.Config (SpeedPolicy (..))
import Bots.StoreService.FakeBTCPay
import Control.Exception (evaluate)
import Control.Monad (forM_, join)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Word (Word32)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.HTTP.Types.Header (Header, hAuthorization)
import Simplex.Chat.PaymentService.Types
  ( CryptoCurrency (..),
    CurrencyAmount (..),
    ServicePaymentDestination (..),
    ServicePaymentMethod (..),
  )
import System.Timeout (timeout)
import Test.Hspec
import Text.Read (readMaybe)

badgeBTCPayTests :: Spec
badgeBTCPayTests = describe "badge btcpay adapter" $ do
  describe "minor units to the provider's decimal string" $
    it "pins the eight values a wrong pad would charge wrongly" testMinorToDecimal
  describe "the status table" $ do
    it "settles on Settled, timed by the latest Settled payment" testSettled
    it "falls back to the read time when a Settled invoice was marked by hand" testSettledManuallyMarked
    it "funds on Processing" testProcessing
    it "closes on Expired and on Invalid" testClosed
    it "reports a closed invoice paid in full on PaidLate, PaidOver or a due at 0 or less, and in part otherwise" testClosedPaidInFull
    it "reads an overpaid invoice's negative due as 0" testOverpaidDueIsZero
    it "funds on New with something received" testNewWithPayment
    it "funds on New with a payment worth less than a minor unit" testNewWithDust
    it "reports no change on New with nothing received" testNewWithNothing
    it "refuses a decimal whose exponent would build a billion-digit number" testAbsurdExponentIsRefused
    it "clamps a figure too wrong to charge, rather than wrapping it" testAbsurdAmountIsClamped
    it "refuses the same exponent sent as a JSON number, without formatting it" testAbsurdExponentAsANumberIsRefused
    it "accepts a rate written at full decimal scale" testLongRateIsAccepted
    it "names an unknown status rather than reporting no change" testUnknownStatus
    it "does not know Complete, which is the legacy invoice API's name" testNoCompleteStatus
    it "names the payment methods it found when none is one of ours" testUnknownPaymentMethod
    it "names a response it cannot read" testUnreadableResponse
  describe "amounts" $ do
    it "reads paymentMethodPaid and never totalPaid" testReadsPaymentMethodPaid
    it "keeps the crypto amount exactly as the provider wrote it" testCryptoAmountVerbatim
    it "multiplies the decimals exactly, where a Double would be a cent out" testExactDecimalMultiplication
    it "reads a numeric field sent as a JSON number as well as a string" testNumberAsWellAsString
  describe "confirmed money" $ do
    it "confirms all of paymentMethodPaid on an invoice-level Settled, with a payment still Processing" testConfirmedInvoiceSettled
    it "confirms all of paymentMethodPaid when every payment reads Settled, whatever their values" testConfirmedAllSettled
    it "confirms the Settled payments' value otherwise, rounded to cents once and capped at paymentMethodPaid" testConfirmedSettledSum
    it "counts BTC in satoshi and XMR in piconero" testConfirmedUnits
    it "confirms XMR only when the invoice's settled-confirmation threshold is at least 1, never with 0 or none" testXmrConfirmations
    it "confirms nothing under HighSpeed, or with no or an unknown speed policy, an invoice-level Settled included" testHighSpeedConfirmsNothing
    it "accepts trailing zeros past the unit" testTrailingZeros
    it "drops only the units of an amount with a non-zero digit past its unit, the signal and its cents as before" testDigitPastUnit
  describe "the list pass" $ do
    it "skips an invoice whose status it does not know, and settles the rest" testListSkipsUnknownStatus
    it "skips an invoice it cannot parse, and settles the rest" testListSkipsUnparseable
    it "skips an invoice paid by a method it does not know, and settles the rest" testListSkipsUnknownMethod
    it "fails the pass when the payment methods are absent, rather than reporting health" testListFailsWithoutPaymentMethods
    it "skips an invoice it cannot even name, unaccounted for" testListSkipsWithoutId
    it "fails the pass on a response it cannot read" testListFailsOnUnreadableBody
    it "reports a listed closed invoice paid in full from its PaidLate" testListClosedPaidLate
  describe "BTCPay-Sig" $ do
    it "accepts the indented bytes as received, for each acted-on type" testWebhookAccepts
    it "finds the header whatever its case" testWebhookHeaderCase
    it "accepts an uppercase hex signature" testWebhookUppercaseHex
    it "ignores a valid signature over a type this service does not act on" testWebhookIgnoresOtherType
    it "ignores a valid signature over a payload with no event in it" testWebhookIgnoresUnreadableBody
    it "rejects the same body signed with a different secret" testWebhookWrongSecret
    it "rejects a re-serialised body, whose bytes are not the ones signed" testWebhookReserialised
    it "rejects a missing, unprefixed or non-hex signature" testWebhookMalformed
  describe "against the fake Greenfield" $ do
    it "creates a bitcoin invoice and reads back where to pay" testFakeCreatesBtc
    it "creates a monero invoice and reads back where to pay" testFakeCreatesXmr
    it "sends the api key as `token <key>` on every call it makes" testFakeSendsApiKey
    it "is refused when the api key is wrong, rather than passing silently" testFakeWrongApiKey
    it "sends the documented create body, and no redirectURL" testFakeCreateBody
    it "walks New to Processing to Settled, two calls per read" testFakeLifecycle
    it "closes on Expired and on Invalid, in full once the whole amount or BTCPay's PaidLate says so" testFakeClosed
    it "names a status the provider invented" testFakeUnknownStatus
    it "reads paymentMethodPaid over the wire, never totalPaid" testFakePaymentMethodPaid
    it "makes a 500 at checkout a ProviderError" testFakeCreate500
    it "abandons an invoice it created but could not learn the address of" testFakeCreatePostFailure
    it "names the payment method a created invoice does not offer" testFakeCreateWrongMethod
    it "names a payment method with no destination, and writes nothing" testFakeCreateNoDestination
    it "names a payment method with no amount, and writes nothing" testFakeCreateNoAmount
    it "refuses an answer past the size it will hold, and reads again after" testFakeOversizeAnswer
    it "makes a 500 on a read a ProviderError the next read recovers from" testFakeRead500
    it "verifies a webhook with the secret it was configured with" testFakeWebhookSecretWiring
    it "keeps totalPaid distinguishable in every fixture an amount is asserted from" testFixturesGuardTotalPaid
    it "keeps due distinguishable from paymentMethodFee, so the wrong field cannot be read" testFixturesGuardDue
  describe "rates and speed policies against the fake Greenfield" $ do
    it "reads the store's rate for the coin against the catalog's currency" testFakeRate
    it "refuses a failed read, a missing pair, a rate with errors, a null rate and a rate at or below 0" testFakeRateRefusals
    it "reads the speed policy an invoice was made under, whatever the configuration says now" testFakeInvoicePolicy
  describe "the list pass against the fake Greenfield" $ do
    it "returns the invoices whose state moved and omits the one that did not" testFakeListMoved
    it "bounds the request at 72 hours plus one invoice window" testFakeListWindow
    it "skips an invoice whose status it does not know, and settles the rest" testFakeListAlienStatus
    it "skips an invoice paid by a method it does not know, and settles the rest" testFakeListAlienMethod
    it "fails the pass when the payment methods are absent" testFakeListNoPaymentMethods
    it "skips an invoice it cannot even name, unaccounted for" testFakeListNoId

-- | A payment of the value given, as the methods body lists it.
paymentOf :: Text -> Text -> J.Value
paymentOf status value =
  J.object
    [ "id" .= ("txid-" <> value),
      "status" .= status,
      "receivedDate" .= (1700000000 :: Int),
      "value" .= value,
      "fee" .= ("0.00000100" :: Text)
    ]

-- | The Monero plugin's method, its additionalData carrying the invoice's settled-confirmation threshold when given.
xmrPaid :: Maybe Int -> Text -> [J.Value] -> J.Value
xmrPaid threshold paid payments =
  J.object
    [ "paymentMethodId" .= xmrMethodId,
      "destination" .= ("44AFFq5kSiGBoZ4NMDwYtN18obc8AemS33DBLWs3H7otXft3XjrpDtQGv7SqSsaBYBb98uNbr2VBBEt7f2wfn3RVGQBEP3A" :: Text),
      "amount" .= ("0.32095000" :: Text),
      "rate" .= ("168.25" :: Text),
      "paymentMethodPaid" .= paid,
      "due" .= ("0.00000000" :: Text),
      "payments" .= payments,
      "additionalData" .= J.object (maybe [] (\n -> ["invoiceSettledConfirmationThreshold" .= n]) threshold)
    ]

testConfirmedInvoiceSettled :: IO ()
testConfirmedInvoiceSettled =
  signalOf "Settled" [btcPaid "0.00050000" [paymentOf "Settled" "0.00025000", paymentOf "Processing" "0.00025000"]]
    `shouldBe` Right (Just (SigSettled halfMilliBtc (posixSecondsToUTCTime 1700000000)))

-- | The values sum to more than paymentMethodPaid, as a fee BTCPay nets out would; all of it is confirmed, never more.
testConfirmedAllSettled :: IO ()
testConfirmedAllSettled =
  signalOf "Processing" [btcPaid "0.00049800" [paymentOf "Settled" "0.00025000", paymentOf "Settled" "0.00025000"]]
    `shouldBe` Right (Just (SigFunded (btcReceived 5378 (Just "0.00049800") "0.00050000" 49800 49800 5378) PaidInFull))

-- | 5 + 5 satoshi are 0.54 + 0.54 cents, 2 cents rounded one by one but 1 rounded once.
testConfirmedSettledSum :: IO ()
testConfirmedSettledSum = do
  signalOf "Processing" [btcPaid "0.00000015" (map (paymentOf "Settled") ["0.00000005", "0.00000005"] <> [paymentOf "Processing" "0.00000005"])]
    `shouldBe` Right (Just (SigFunded (btcReceived 2 (Just "0.00000015") "0.00050000" 15 10 1) PaidInFull))
  signalOf "Processing" [btcPaid "0.00050000" [paymentOf "Settled" "0.00030000", paymentOf "Settled" "0.00030000", paymentOf "Processing" "0.00010000"]]
    `shouldBe` Right (Just (SigFunded halfMilliBtc PaidInFull))

testConfirmedUnits :: IO ()
testConfirmedUnits = do
  (rcvUnitsOf =<<) <$> signalOf "Settled" [btcPaid "0.00050000" []] `shouldBe` Right (Just 50000)
  signalOf "Settled" [xmrPaid (Just 1) "0.32095000" [paymentOf "Settled" "0.32095000"]]
    `shouldBe` Right (Just (SigSettled xmrConfirmed (posixSecondsToUTCTime 1700000000)))

xmrConfirmed :: Received
xmrConfirmed = xmrReceived {rcvConfirmed = Confirmed {cfCents = CurrencyAmount 5400, cfUnits = Just 320950000000}, rcvUncreditable = Nothing}

-- | A Monero payment's own confirmations are not in Greenfield's payment model, so only the invoice's threshold shows one.
testXmrConfirmations :: IO ()
testXmrConfirmations = do
  let settledWith threshold = signalOf "Settled" [xmrPaid threshold "0.32095000" [paymentOf "Settled" "0.32095000"]]
      settled r = Right (Just (SigSettled r (posixSecondsToUTCTime 1700000000)))
  settledWith (Just 0) `shouldBe` settled xmrReceived
  settledWith Nothing `shouldBe` settled xmrReceived
  mapM_ (\n -> settledWith (Just n) `shouldBe` settled xmrConfirmed) [1, 10]

rcvUnitsOf :: PaymentSignal -> Maybe Int64
rcvUnitsOf = \case
  SigFunded r _ -> rcvUnits r
  SigSettled r _ -> rcvUnits r
  SigClosed r _ -> rcvUnits r

testHighSpeedConfirmsNothing :: IO ()
testHighSpeedConfirmsNothing = do
  let settledUnder policy = signalUnder policy "Settled" [btcPaid "0.00050000" [paymentOf "Settled" "0.00050000"]]
      unconfirmed = halfMilliBtcUnconfirmed {rcvUncreditable = Just UCHighSpeed}
  mapM_ (\policy -> (policy, settledUnder policy) `shouldBe` (policy, Right (Just (SigSettled unconfirmed (posixSecondsToUTCTime 1700000000))))) [Just "HighSpeed", Nothing, Just "Turbo"]
  mapM_ (\policy -> (policy, settledUnder policy) `shouldBe` (policy, Right (Just (SigSettled halfMilliBtc (posixSecondsToUTCTime 1700000000))))) [Just "MediumSpeed", Just "LowMediumSpeed", Just "LowSpeed"]

testTrailingZeros :: IO ()
testTrailingZeros =
  signalOf "Settled" [btcPaid "0.000500000000" [paymentOf "Settled" "0.0005000000000"]]
    `shouldBe` Right (Just (SigSettled halfMilliBtc {rcvCrypto = Just "0.000500000000", rcvDue = Just "0.00050000"} (posixSecondsToUTCTime 1700000000)))

-- | A badge invoice reads and settles on this signal as before; only a late credit needs the units.
testDigitPastUnit :: IO ()
testDigitPastUnit = do
  let uncounted crypto due = halfMilliBtcUnconfirmed {rcvCrypto = Just crypto, rcvDue = Just due, rcvUnits = Nothing, rcvUncreditable = Just UCUncounted}
  signalOf "Settled" [btcPaid "0.000500001" [paymentOf "Settled" "0.000500001"]]
    `shouldBe` Right (Just (SigSettled (uncounted "0.000500001" "0.00050000") (posixSecondsToUTCTime 1700000000)))
  signalOf "Processing" [btcPaid "0.00050000" [paymentOf "Settled" "0.000250001", paymentOf "Processing" "0.00025000"]]
    `shouldBe` Right (Just (SigFunded (uncounted "0.00050000" "0.00000000") PaidInFull))

testMinorToDecimal :: IO ()
testMinorToDecimal =
  map (minorToDecimal . CurrencyAmount) [0, 5, 99, 100, 700, 4200, 42000, 100000000]
    `shouldBe` ["0.00", "0.05", "0.99", "1.00", "7.00", "42.00", "420.00", "1000000.00"]

readTime :: UTCTime
readTime = posixSecondsToUTCTime 1800000000

ref :: Text
ref = "GREENFIELDINVOICEID"

btcPaid :: Text -> [J.Value] -> J.Value
btcPaid paid payments =
  J.object
    [ "paymentMethodId" .= btcMethodId,
      "destination" .= ("bc1qexampleaddress" :: Text),
      "amount" .= ("0.00050000" :: Text),
      "rate" .= ("108000.12" :: Text),
      "paymentMethodPaid" .= paid,
      "totalPaid" .= ("9.99999999" :: Text),
      "due" .= dueAfter paid,
      "paymentMethodFee" .= ("0.00000500" :: Text),
      "payments" .= payments
    ]

-- | The due each payment figure leaves owed on a 0.00050000 invoice. The dust case does not use
-- the difference between amount and paid, because a partial payment adds a network fee, so a body
-- computed by subtraction would also pass an adapter that subtracted instead of reading the field.
dueAfter :: Text -> Text
dueAfter = \case
  "0.00050000" -> "0.00000000"
  "0.00080000" -> "-0.00030000"
  "0.00000001" -> "0.00050499"
  _ -> "0.00050000"

payment :: Text -> Integer -> J.Value
payment status receivedDate =
  J.object
    [ "id" .= ("txid" <> T.pack (show receivedDate)),
      "status" .= status,
      "receivedDate" .= receivedDate,
      "value" .= ("0.00025000" :: Text),
      "fee" .= ("0.00000100" :: Text)
    ]

methodsBody :: [J.Value] -> LB.ByteString
methodsBody = J.encode

signalOf :: Text -> [J.Value] -> Either ProviderError (Maybe PaymentSignal)
signalOf = signalUnder (Just "MediumSpeed")

-- | Read from an invoice made under the speed policy given, or none.
signalUnder :: Maybe Text -> Text -> [J.Value] -> Either ProviderError (Maybe PaymentSignal)
signalUnder policy status methods = paymentMethodsSignal readTime ref status Nothing policy (methodsBody methods)

-- | Read from an invoice with the additional status given.
signalWith :: Text -> Text -> [J.Value] -> Either ProviderError (Maybe PaymentSignal)
signalWith additional status methods = paymentMethodsSignal readTime ref status (Just additional) (Just "MediumSpeed") (methodsBody methods)

-- | BTC at the rate the bodies carry: the cents, crypto and due received, and the satoshi received and confirmed, with the confirmed cents.
btcReceived :: Word32 -> Maybe Text -> Text -> Int64 -> Int64 -> Word32 -> Received
btcReceived cents crypto due units confirmedUnits confirmedCents =
  Received
    { rcvAmount = CurrencyAmount cents,
      rcvCrypto = crypto,
      rcvDue = Just due,
      rcvUnits = Just units,
      rcvRate = Just "108000.12",
      rcvConfirmed = Confirmed {cfCents = CurrencyAmount confirmedCents, cfUnits = Just confirmedUnits},
      rcvUncreditable = Nothing
    }

halfMilliBtc :: Received
halfMilliBtc = btcReceived 5400 (Just "0.00050000") "0.00000000" 50000 50000 5400

halfMilliBtcUnconfirmed :: Received
halfMilliBtcUnconfirmed = halfMilliBtc {rcvConfirmed = Confirmed {cfCents = CurrencyAmount 0, cfUnits = Just 0}}

nothingReceived :: Text -> Received
nothingReceived due = btcReceived 0 Nothing due 0 0 0

testSettled :: IO ()
testSettled =
  signalOf "Settled" [btcPaid "0.00050000" [payment "Settled" 1700003600, payment "Settled" 1700000000, payment "Processing" 1700007200]]
    `shouldBe` Right (Just (SigSettled halfMilliBtc (posixSecondsToUTCTime 1700003600)))

testSettledManuallyMarked :: IO ()
testSettledManuallyMarked =
  signalOf "Settled" [btcPaid "0.00050000" []]
    `shouldBe` Right (Just (SigSettled halfMilliBtc readTime))

testProcessing :: IO ()
testProcessing =
  signalOf "Processing" [btcPaid "0.00050000" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigFunded halfMilliBtcUnconfirmed PaidInFull))

testClosed :: IO ()
testClosed = do
  signalOf "Expired" [btcPaid "0.00050000" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigClosed halfMilliBtcUnconfirmed PaidInFull))
  signalOf "Invalid" [btcPaid "0.00000000" []]
    `shouldBe` Right (Just (SigClosed (nothingReceived "0.00050000") PaidInPart))

-- | BTCPay keeps a late-paid invoice Expired, or Invalid after a cancel, and says the whole amount arrived only by these.
testClosedPaidInFull :: IO ()
testClosedPaidInFull = do
  let verdict additional status paid = (\case Right (Just (SigClosed _ f)) -> Just f; _ -> Nothing) (signalWith additional status [btcPaid paid [payment "Settled" 1700000000]])
  verdict "PaidLate" "Expired" "0.00025000" `shouldBe` Just PaidInFull
  verdict "PaidOver" "Invalid" "0.00025000" `shouldBe` Just PaidInFull
  verdict "None" "Expired" "0.00050000" `shouldBe` Just PaidInFull
  verdict "Marked" "Invalid" "0.00080000" `shouldBe` Just PaidInFull
  verdict "PaidPartial" "Expired" "0.00025000" `shouldBe` Just PaidInPart
  verdict "None" "Invalid" "0.00025000" `shouldBe` Just PaidInPart
  verdict "None" "Expired" "0.00000000" `shouldBe` Just PaidInPart

testOverpaidDueIsZero :: IO ()
testOverpaidDueIsZero = do
  let overpaid = btcReceived 8640 (Just "0.00080000") "0" 80000 80000 8640
  signalOf "Settled" [btcPaid "0.00080000" []] `shouldBe` Right (Just (SigSettled overpaid readTime))
  signalWith "PaidOver" "Expired" [btcPaid "0.00080000" []] `shouldBe` Right (Just (SigClosed overpaid {rcvConfirmed = Confirmed {cfCents = CurrencyAmount 0, cfUnits = Just 0}} PaidInFull))

testNewWithPayment :: IO ()
testNewWithPayment =
  signalOf "New" [btcPaid "0.00050000" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigFunded halfMilliBtcUnconfirmed PaidInPart))

testNewWithDust :: IO ()
testNewWithDust =
  signalOf "New" [btcPaid "0.00000001" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigFunded (btcReceived 0 (Just "0.00000001") "0.00050499" 1 0 0) PaidInPart))

testNewWithNothing :: IO ()
testNewWithNothing = signalOf "New" [btcPaid "0.00000000" []] `shouldBe` Right Nothing

-- | A figure with an absurd exponent parses as a Scientific and then asks for a number with that
-- many digits, which never returns. The test bounds the call with a timeout, since the guard must
-- refuse it before anything rounds or formats it.
testAbsurdExponentIsRefused :: IO ()
testAbsurdExponentIsRefused = do
  answered <- timeout 5000000 (evaluate (signalOf "Settled" [btcPaid "1e1000000000" []]))
  case answered of
    Nothing -> expectationFailure "a decimal with a billion-digit exponent was not refused"
    Just r -> r `shouldSatisfy` namesInError "exponent out of range"

-- | BTCPay sends `receivedDate` as a bare JSON number, so the same absurd figure must be refused
-- in that form too, and the refusal must not format it on the way out or the guard rebuilds it.
testAbsurdExponentAsANumberIsRefused :: IO ()
testAbsurdExponentAsANumberIsRefused = do
  let huge = J.Number (scientific 1 1000000000)
      method =
        J.object
          [ "paymentMethodId" .= btcMethodId,
            "destination" .= ("bc1qexampleaddress" :: Text),
            "amount" .= huge,
            "rate" .= ("108000.12" :: Text),
            "paymentMethodPaid" .= ("0.00050000" :: Text),
            "due" .= ("0.00000000" :: Text),
            "paymentMethodFee" .= ("0.00000500" :: Text),
            "payments" .= ([] :: [J.Value])
          ]
  answered <- timeout 5000000 (evaluate (signalOf "Settled" [method]))
  case answered of
    Nothing -> expectationFailure "a JSON number with a billion-digit exponent was not refused"
    Just r -> r `shouldSatisfy` namesInError "exponent out of range"

testLongRateIsAccepted :: IO ()
testLongRateIsAccepted =
  signalOf "Settled" [longRate] `shouldSatisfy` \case
    Right (Just SigSettled {}) -> True
    _ -> False
  where
    longRate =
      J.object
        [ "paymentMethodId" .= btcMethodId,
          "destination" .= ("bc1qexampleaddress" :: Text),
          "amount" .= ("0.00050000" :: Text),
          "rate" .= ("0.3333333333333333333333333333" :: Text),
          "paymentMethodPaid" .= ("0.00050000" :: Text),
          "due" .= ("0.00000000" :: Text),
          "paymentMethodFee" .= ("0.00000500" :: Text),
          "payments" .= ([] :: [J.Value])
        ]

testUnknownStatus :: IO ()
testUnknownStatus = signalOf "Frobnicated" [btcPaid "0.00050000" []] `shouldSatisfy` namesInError "Frobnicated"

testNoCompleteStatus :: IO ()
testNoCompleteStatus =
  signalOf "Complete" [btcPaid "0.00050000" [payment "Settled" 1700000000]] `shouldSatisfy` namesInError "Complete"

testUnknownPaymentMethod :: IO ()
testUnknownPaymentMethod = do
  let ltc = J.object ["paymentMethodId" .= ("LTC-CHAIN" :: Text), "rate" .= ("1" :: Text), "paymentMethodPaid" .= ("0" :: Text)]
  signalOf "Settled" [ltc] `shouldSatisfy` namesInError "LTC-CHAIN"
  signalOf "Settled" [ltc] `shouldSatisfy` namesInError btcMethodId
  signalOf "Settled" [ltc] `shouldSatisfy` namesInError xmrMethodId

testUnreadableResponse :: IO ()
testUnreadableResponse =
  paymentMethodsSignal readTime ref "Settled" Nothing (Just "MediumSpeed") "<html>502 Bad Gateway</html>" `shouldSatisfy` namesInError "could not read the response"

namesInError :: Text -> Either ProviderError a -> Bool
namesInError what = \case
  Left (ProviderError e) -> what `T.isInfixOf` e
  Right _ -> False

listEntry :: Text -> Text -> Maybe [J.Value] -> J.Value
listEntry invId status methods =
  J.object $
    [ "id" .= invId,
      "status" .= status,
      "additionalStatus" .= ("None" :: Text),
      "amount" .= ("54.00" :: Text),
      "currency" .= ("USD" :: Text),
      "checkout" .= J.object ["speedPolicy" .= ("MediumSpeed" :: Text)]
    ]
      <> maybe [] (\ms -> ["paymentMethods" .= ms]) methods

oursSettled :: J.Value
oursSettled = listEntry ref "Settled" (Just [btcPaid "0.00050000" [payment "Settled" 1700000000]])

oursSettledSignal :: (Text, PaymentSignal)
oursSettledSignal = (ref, SigSettled halfMilliBtc (posixSecondsToUTCTime 1700000000))

movedOnly :: [(Text, PaymentSignal)] -> Either ProviderError ListPass
movedOnly moved = Right ListPass {lpMoved = moved, lpSkipped = []}

skippedReason :: HasCallStack => Either ProviderError ListPass -> IO Text
skippedReason = fmap snd . skipped

skipped :: HasCallStack => Either ProviderError ListPass -> IO (Maybe Text, Text)
skipped = \case
  Right ListPass {lpSkipped = [skip]} -> pure skip
  other -> failWith ("expected exactly one skipped invoice, got " <> show other)

testListSkipsUnknownStatus :: IO ()
testListSkipsUnknownStatus = do
  let alien = listEntry "SOMEONEELSESINVOICE" "Frobnicated" (Just [btcPaid "0.00050000" []])
      r = listSignals readTime (J.encode [alien, oursSettled])
  lpMoved <$> r `shouldBe` Right [oursSettledSignal]
  (skippedRef, reason) <- skipped r
  reason `shouldSatisfy` T.isInfixOf "Frobnicated"
  reason `shouldSatisfy` T.isInfixOf "SOMEONEELSESINVOICE"
  skippedRef `shouldBe` Just "SOMEONEELSESINVOICE"

-- | Each invoice is decoded on its own, not the whole page at once, because XMR payments come from
-- a BTCPay plugin and one malformed entry must not stop every other invoice settling on every pass.
testListSkipsUnparseable :: IO ()
testListSkipsUnparseable = do
  let noStatus =
        J.object
          [ "paymentMethodId" .= btcMethodId,
            "destination" .= ("bc1qexampleaddress" :: Text),
            "amount" .= ("0.00050000" :: Text),
            "rate" .= ("108000.12" :: Text),
            "paymentMethodPaid" .= ("0.00050000" :: Text),
            "due" .= ("0.00000000" :: Text),
            "paymentMethodFee" .= ("0.00000500" :: Text),
            "payments" .= [J.object ["receivedDate" .= (1700000000 :: Int)]]
          ]
      broken = listEntry "MALFORMEDINVOICE" "Settled" (Just [noStatus])
      r = listSignals readTime (J.encode [broken, oursSettled])
  lpMoved <$> r `shouldBe` Right [oursSettledSignal]
  (skippedRef, reason) <- skipped r
  skippedRef `shouldBe` Just "MALFORMEDINVOICE"
  reason `shouldSatisfy` T.isInfixOf "status"

testListSkipsUnknownMethod :: IO ()
testListSkipsUnknownMethod = do
  let ltc =
        J.object
          [ "paymentMethodId" .= ("BTC-LN" :: Text),
            "rate" .= ("108000.12" :: Text),
            "paymentMethodPaid" .= ("0.00050000" :: Text),
            "payments" .= ([] :: [J.Value])
          ]
      alien = listEntry "SOMEONEELSESINVOICE" "Settled" (Just [ltc])
      r = listSignals readTime (J.encode [alien, oursSettled])
  lpMoved <$> r `shouldBe` Right [oursSettledSignal]
  skippedReason r >>= (`shouldSatisfy` T.isInfixOf "BTC-LN")

testListFailsWithoutPaymentMethods :: IO ()
testListFailsWithoutPaymentMethods = do
  listSignals readTime (J.encode [listEntry ref "Settled" Nothing]) `shouldSatisfy` namesInError "paymentMethods"

testListSkipsWithoutId :: IO ()
testListSkipsWithoutId = do
  let noId = J.object ["status" .= ("Settled" :: Text), "paymentMethods" .= [btcPaid "0.00050000" []]]
      r = listSignals readTime (J.encode [oursSettled, noId])
  lpMoved <$> r `shouldBe` Right [oursSettledSignal]
  (skippedRef, reason) <- skipped r
  skippedRef `shouldBe` Nothing
  reason `shouldSatisfy` T.isInfixOf "id"

testListFailsOnUnreadableBody :: IO ()
testListFailsOnUnreadableBody =
  listSignals readTime "<html>502 Bad Gateway</html>" `shouldSatisfy` namesInError "could not read the response"

testListClosedPaidLate :: IO ()
testListClosedPaidLate = do
  let paidLate = case listEntry ref "Expired" (Just [btcPaid "0.00025000" []]) of
        J.Object o -> J.Object (KM.insert "additionalStatus" (J.String "PaidLate") o)
        other -> other
      quarter = btcReceived 2700 (Just "0.00025000") "0.00050000" 25000 0 0
  lpMoved <$> listSignals readTime (J.encode [paidLate]) `shouldBe` Right [(ref, SigClosed quarter PaidInFull)]

testReadsPaymentMethodPaid :: IO ()
testReadsPaymentMethodPaid = do
  signalOf "Settled" [btcPaid "0.00050000" [payment "Settled" 1700000000]]
    `shouldBe` Right (Just (SigSettled halfMilliBtc (posixSecondsToUTCTime 1700000000)))
  signalOf "New" [btcPaid "0.00000000" []] `shouldBe` Right Nothing

testCryptoAmountVerbatim :: IO ()
testCryptoAmountVerbatim =
  case signalOf "Processing" [btcPaid "0.00050000" []] of
    Right (Just (SigFunded r _)) -> rcvCrypto r `shouldBe` Just "0.00050000"
    other -> expectationFailure ("expected SigFunded, got " <> show other)

testExactDecimalMultiplication :: IO ()
testExactDecimalMultiplication = do
  let atRateOne paid =
        J.object
          [ "paymentMethodId" .= btcMethodId,
            "rate" .= ("1" :: Text),
            "paymentMethodPaid" .= (paid :: Text),
            "payments" .= ([] :: [J.Value])
          ]
  case signalOf "Processing" [atRateOne "0.545"] of
    Right (Just (SigFunded r _)) -> rcvAmount r `shouldBe` CurrencyAmount 54
    other -> expectationFailure ("expected SigFunded, got " <> show other)

-- The exponent gate bounds how long a figure may be, not how large, so a negative or eleven-digit
-- value still reaches the conversion. Unclamped, both wrap Word32 into a positive amount that reads
-- as money received, and the invoice then holds against every sweep and answers every cancel funded.
testAbsurdAmountIsClamped :: IO ()
testAbsurdAmountIsClamped = do
  chargeFor "-1" `shouldBe` Just (CurrencyAmount 0)
  chargeFor "99999999999" `shouldBe` Just (CurrencyAmount 4294967295)
  where
    chargeFor paid = case signalOf "Processing" [atRateOne paid] of
      Right (Just (SigFunded r _)) -> Just (rcvAmount r)
      _ -> Nothing
    atRateOne paid =
      J.object
        [ "paymentMethodId" .= btcMethodId,
          "rate" .= ("1" :: Text),
          "paymentMethodPaid" .= (paid :: Text),
          "payments" .= ([] :: [J.Value])
        ]

testNumberAsWellAsString :: IO ()
testNumberAsWellAsString = do
  let asNumbers =
        "[{\"paymentMethodId\":\"BTC-CHAIN\",\"rate\":108000.12,\"paymentMethodPaid\":0.00050000,\
        \\"payments\":[{\"status\":\"Settled\",\"receivedDate\":1700000000}]}]"
  case paymentMethodsSignal readTime ref "Settled" Nothing (Just "MediumSpeed") asNumbers of
    Right (Just (SigSettled r t)) -> do
      rcvAmount r `shouldBe` CurrencyAmount 5400
      rcvCrypto r `shouldBe` Just "0.00050000"
      t `shouldBe` posixSecondsToUTCTime 1700000000
    other -> expectationFailure ("expected SigSettled, got " <> show other)

secret :: Text
secret = "3d8f5c6a2b1e4f7089abcdef01234567"

verifyEvent :: [Header] -> LB.ByteString -> Either WebhookError (Maybe Text)
verifyEvent hdrs body = verifyBTCPaySig secret hdrs (LB.toStrict body)

testWebhookAccepts :: IO ()
testWebhookAccepts =
  mapM_
    (\t -> let b = webhookEvent t ref in verifyEvent (webhookSigHeader secret b) b `shouldBe` Right (Just ref))
    ["InvoiceProcessing", "InvoiceSettled", "InvoiceExpired", "InvoiceInvalid", "InvoiceReceivedPayment", "InvoicePaymentSettled"]

testWebhookHeaderCase :: IO ()
testWebhookHeaderCase = do
  let b = webhookEvent "InvoiceSettled" ref
  verifyEvent [("btcpay-sig", "sha256=" <> webhookHexSig secret b)] b `shouldBe` Right (Just ref)

testWebhookUppercaseHex :: IO ()
testWebhookUppercaseHex = do
  let b = webhookEvent "InvoiceSettled" ref
  verifyEvent [("BTCPay-Sig", "sha256=" <> B8.map toUpper (webhookHexSig secret b))] b `shouldBe` Right (Just ref)

testWebhookIgnoresOtherType :: IO ()
testWebhookIgnoresOtherType = do
  let b = webhookEvent "InvoiceCreated" ref
  verifyEvent (webhookSigHeader secret b) b `shouldBe` Right Nothing

testWebhookIgnoresUnreadableBody :: IO ()
testWebhookIgnoresUnreadableBody = do
  let b = "{\n  \"type\": \"InvoiceSettled\"\n}\n"
  verifyEvent (webhookSigHeader secret b) b `shouldBe` Right Nothing

testWebhookWrongSecret :: IO ()
testWebhookWrongSecret = do
  let b = webhookEvent "InvoiceSettled" ref
  verifyEvent (webhookSigHeader "3d8f5c6a2b1e4f7089abcdef01234568" b) b `shouldSatisfy` isRefused

testWebhookReserialised :: IO ()
testWebhookReserialised = do
  let b = webhookEvent "InvoiceSettled" ref
      reserialised = maybe "" J.encode (J.decode b :: Maybe J.Value)
  reserialised `shouldNotBe` b
  verifyEvent (webhookSigHeader secret b) reserialised `shouldSatisfy` isRefused

testWebhookMalformed :: IO ()
testWebhookMalformed = do
  let b = webhookEvent "InvoiceSettled" ref
  verifyEvent [] b `shouldSatisfy` isRefused
  verifyEvent [("BTCPay-Sig", "")] b `shouldSatisfy` isRefused
  verifyEvent [("BTCPay-Sig", webhookHexSig secret b)] b `shouldSatisfy` isRefused
  verifyEvent [("BTCPay-Sig", "sha512=" <> webhookHexSig secret b)] b `shouldSatisfy` isRefused
  verifyEvent [("BTCPay-Sig", "sha256=not hex at all")] b `shouldSatisfy` isRefused
  verifyEvent [("BTCPay-Sig", "sha256=")] b `shouldSatisfy` isRefused

isRefused :: Either WebhookError (Maybe Text) -> Bool
isRefused = \case
  Left (WebhookError _) -> True
  _ -> False

btcAddress, xmrAddress :: Text
btcAddress = "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"
xmrAddress = "44AFFq5kSiGBoZ4NMDwYtN18obc8AemS33DBLWs3H7otXft3XjrpDtQGv7SqSsaBYBb98uNbr2VBBEt7f2wfn3RVGQBEP3A"

fixtureSettledAt :: UTCTime
fixtureSettledAt = posixSecondsToUTCTime 1700003600

listSettledRef, listProcessingRef :: Text
listSettledRef = "SettledInvoiceRefAAAA"
listProcessingRef = "ProcessingInvoiceRefB"

xmrReceived :: Received
xmrReceived =
  Received
    { rcvAmount = CurrencyAmount 5400,
      rcvCrypto = Just "0.32095000",
      rcvDue = Just "0.00000000",
      rcvUnits = Just 320950000000,
      rcvRate = Just "168.25",
      rcvConfirmed = Confirmed {cfCents = CurrencyAmount 0, cfUnits = Just 0},
      rcvUncreditable = Just UCNoConfirmationShown
    }

fiftyFourDollars :: OrderDraft
fiftyFourDollars =
  OrderDraft
    { odAmount = CurrencyAmount 5400,
      odCurrency = "usd"
    }

exampleCeiling :: Int
exampleCeiling = 20000000

failWith :: HasCallStack => String -> IO a
failWith msg = expectationFailure msg >> error msg

withProvider :: HasCallStack => (FakeBTCPay -> Provider -> IO a) -> IO a
withProvider = withProviderConfigured id

withProviderConfigured :: HasCallStack => (BTCPayConfig -> BTCPayConfig) -> (FakeBTCPay -> Provider -> IO a) -> IO a
withProviderConfigured configure action =
  bounded $ withFakeBTCPay $ \fake -> btcpayProvider (configure (fbConfig fake)) >>= action fake
  where
    bounded act = timeout exampleCeiling act >>= maybe (failWith "the fake greenfield did not answer within 20s") pure

createdInvoice :: HasCallStack => Provider -> ServicePaymentMethod -> IO ProviderInvoice
createdInvoice p spm =
  pCreateInvoice p spm fiftyFourDollars >>= \case
    Right inv -> pure inv
    Left e -> failWith ("expected an invoice, got " <> show e)

testFakeCreatesBtc :: IO ()
testFakeCreatesBtc = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef, piDestination} <- createdInvoice p (SPMCrypto CCBtc)
  fakeInvoiceIds fake `shouldReturn` [piProviderRef]
  piDestination `shouldBe` SPDCrypto CCBtc btcAddress "0.00050000"
  ms <- apiRequests fake "GET" ["invoices", piProviderRef, "payment-methods"]
  length ms `shouldBe` 1

testFakeCreatesXmr :: IO ()
testFakeCreatesXmr = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef, piDestination} <- createdInvoice p (SPMCrypto CCXmr)
  piDestination `shouldBe` SPDCrypto CCXmr xmrAddress "0.32095000"
  posts <- apiRequests fake "POST" ["invoices"]
  case posts of
    [created] -> chosenPaymentMethods (frBody created) `shouldBe` Just ["XMR-CHAIN"]
    _ -> expectationFailure ("expected one create, got " <> show (length posts))
  fakeInvoiceIds fake `shouldReturn` [piProviderRef]

chosenPaymentMethods :: LB.ByteString -> Maybe [Text]
chosenPaymentMethods body = case J.decode body of
  Just (J.Object o) | Just (J.Object c) <- KM.lookup "checkout" o -> case KM.lookup "paymentMethods" c of
    Just v -> case J.fromJSON v of
      J.Success ms -> Just ms
      J.Error _ -> Nothing
    Nothing -> Nothing
  _ -> Nothing

testFakeSendsApiKey :: IO ()
testFakeSendsApiKey = withProvider $ \fake p -> do
  _ <- createdInvoice p (SPMCrypto CCBtc)
  _ <- pListOpen p
  rs <- fakeRequests fake
  length rs `shouldBe` 4
  map (lookup hAuthorization . frHeaders) rs
    `shouldBe` replicate 4 (Just ("token " <> TE.encodeUtf8 fakeApiKey))

testFakeWrongApiKey :: IO ()
testFakeWrongApiKey = withProviderConfigured (\c -> c {bApiKey = "not-the-api-key"}) $ \fake p -> do
  r <- pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars
  r `shouldSatisfy` namesInError "401"
  rd <- pReadInvoice p "FakeInvoiceRef0001"
  rd `shouldSatisfy` namesInError "401"
  rs <- fakeRequests fake
  length rs `shouldBe` 3
  fakeInvoiceIds fake `shouldReturn` []

testFakeCreateBody :: IO ()
testFakeCreateBody = withProvider $ \fake p -> do
  _ <- createdInvoice p (SPMCrypto CCBtc)
  posts <- apiRequests fake "POST" ["invoices"]
  case posts of
    [created] ->
      J.decode (frBody created)
        `shouldBe` Just
          ( J.object
              [ "amount" .= ("54.00" :: Text),
                "currency" .= ("USD" :: Text),
                "checkout"
                  .= J.object
                    [ "expirationMinutes" .= fakeExpiryMinutes,
                      "speedPolicy" .= ("MediumSpeed" :: Text),
                      "paymentTolerance" .= (0.5 :: Double),
                      "paymentMethods" .= ([btcMethodId] :: [Text])
                    ]
              ]
          )
    _ -> expectationFailure ("expected one create, got " <> show (length posts))

testFakeLifecycle :: IO ()
testFakeLifecycle = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  pReadInvoice p invRef `shouldReturn` Right Nothing
  setInvoiceState fake invRef ["paymentMethodPaid" .= ("0.00025000" :: Text)]
  pReadInvoice p invRef
    `shouldReturn` Right (Just (SigFunded quarterMilliBtc PaidInPart))
  setInvoiceState fake invRef ["status" .= ("Processing" :: Text), "additionalStatus" .= ("PaidPartial" :: Text)]
  pReadInvoice p invRef
    `shouldReturn` Right (Just (SigFunded quarterMilliBtc PaidInFull))
  setInvoiceState fake invRef ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigSettled halfMilliBtc fixtureSettledAt))
  details <- apiRequests fake "GET" ["invoices", invRef]
  methods <- apiRequests fake "GET" ["invoices", invRef, "payment-methods"]
  (length details, length methods) `shouldBe` (4, 5)

quarterMilliBtc :: Received
quarterMilliBtc = btcReceived 2700 (Just "0.00025000") "0.00000000" 25000 0 0

testFakeClosed :: IO ()
testFakeClosed = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  setInvoiceState fake invRef ["status" .= ("Expired" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigClosed halfMilliBtcUnconfirmed PaidInFull))
  setInvoiceState fake invRef ["status" .= ("Invalid" :: Text), "paymentMethodPaid" .= ("0.00000000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigClosed (nothingReceived "0.00050000") PaidInPart))
  setInvoiceState fake invRef ["status" .= ("Expired" :: Text), "additionalStatus" .= ("PaidLate" :: Text), "paymentMethodPaid" .= ("0.00025000" :: Text), "due" .= ("0.00025000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigClosed (btcReceived 2700 (Just "0.00025000") "0.00025000" 25000 0 0) PaidInFull))

testFakeUnknownStatus :: IO ()
testFakeUnknownStatus = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  setInvoiceState fake invRef ["status" .= ("Frobnicated" :: Text)]
  r <- pReadInvoice p invRef
  r `shouldSatisfy` namesInError "Frobnicated"

testFakePaymentMethodPaid :: IO ()
testFakePaymentMethodPaid = withProvider $ \fake p -> do
  totalPaidStaysDistinguishable "payment-methods-btc"
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  setInvoiceState fake invRef ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigSettled halfMilliBtc fixtureSettledAt))

testFakeCreate500 :: IO ()
testFakeCreate500 = withProvider $ \fake p -> do
  failNextCalls fake 1 500
  r <- pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars
  r `shouldSatisfy` namesInError "500"
  fakeInvoiceIds fake `shouldReturn` []

testFakeCreatePostFailure :: IO ()
testFakeCreatePostFailure = withProvider $ \fake p -> do
  failAfterCalls 1 fake 1 500
  r <- pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars
  r `shouldSatisfy` namesInError "500"
  ids <- fakeInvoiceIds fake
  length ids `shouldBe` 1
  r `shouldSatisfy` \case Right _ -> False; Left _ -> True

testFakeCreateWrongMethod :: IO ()
testFakeCreateWrongMethod = withProvider $ \fake p -> do
  usePaymentMethodsFixture fake "payment-methods-xmr"
  r <- pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars
  r `shouldSatisfy` namesInError "offers no BTC-CHAIN"
  r `shouldSatisfy` namesInError "XMR-CHAIN"

testFakeCreateNoDestination :: IO ()
testFakeCreateNoDestination = withProvider $ \fake p -> do
  usePaymentMethodsFixture fake "payment-methods-no-destination"
  r <- pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars
  r `shouldSatisfy` namesInError "has no destination"

testFakeCreateNoAmount :: IO ()
testFakeCreateNoAmount = withProvider $ \fake p -> do
  usePaymentMethodsFixture fake "payment-methods-no-amount"
  r <- pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars
  r `shouldSatisfy` namesInError "has no amount"

testFakeOversizeAnswer :: IO ()
testFakeOversizeAnswer = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  answerOversize fake True
  pReadInvoice p invRef >>= (`shouldSatisfy` namesInError "over 10485760 bytes")
  answerOversize fake False
  pReadInvoice p invRef >>= \case
    Right _ -> pure ()
    Left e -> expectationFailure ("the next read must recover, and got " <> show e)

testFakeRead500 :: IO ()
testFakeRead500 = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  setInvoiceState fake invRef ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  failNextCalls fake 1 500
  r <- pReadInvoice p invRef
  r `shouldSatisfy` namesInError "500"
  pReadInvoice p invRef `shouldReturn` Right (Just (SigSettled halfMilliBtc fixtureSettledAt))

testFakeWebhookSecretWiring :: IO ()
testFakeWebhookSecretWiring = withProvider $ \fake p -> do
  let configured = bWebhookSecret (fbConfig fake)
      b = webhookEvent "InvoiceSettled" ref
  pVerifyWebhook p readTime (webhookSigHeader configured b) (LB.toStrict b) `shouldBe` Right (Just ref)
  pVerifyWebhook p readTime (webhookSigHeader (configured <> "0") b) (LB.toStrict b) `shouldSatisfy` isRefused

-- | An amount assertion only proves the adapter reads paymentMethodPaid rather than totalPaid while
-- the two differ in the fixture, so this checks that directly.
totalPaidStaysDistinguishable :: HasCallStack => Text -> IO ()
totalPaidStaysDistinguishable name = do
  v <- fixtureResponse name
  case paymentMethodsIn v of
    [] -> expectationFailure (T.unpack name <> " carries no payment method")
    ms -> mapM_ distinguishable ms
  where
    distinguishable m
      | paidNothing m = pure ()
      | otherwise = KM.lookup "totalPaid" m `shouldNotBe` KM.lookup "paymentMethodPaid" m
    paidNothing m = case KM.lookup "paymentMethodPaid" m of
      Just (J.String t) -> (J.decodeStrict (TE.encodeUtf8 t) :: Maybe Scientific) == Just 0
      _ -> False

paymentMethodsIn :: J.Value -> [J.Object]
paymentMethodsIn = \case
  J.Object o -> [o | KM.member "paymentMethodPaid" o] <> concatMap paymentMethodsIn (KM.elems o)
  J.Array vs -> concatMap paymentMethodsIn (toList vs)
  _ -> []

testFixturesGuardTotalPaid :: IO ()
testFixturesGuardTotalPaid =
  mapM_
    totalPaidStaysDistinguishable
    ["payment-methods-btc", "payment-methods-xmr", "invoice-list", "invoice-list-alien-status", "invoice-list-alien-method"]

-- | With due equal to paymentMethodFee a fixture cannot tell which field the adapter reads, and
-- reading the fee would report a part-paid invoice as covered.
dueStaysDistinguishable :: HasCallStack => Text -> IO ()
dueStaysDistinguishable name = do
  v <- fixtureResponse name
  case paymentMethodsIn v of
    [] -> expectationFailure (T.unpack name <> " carries no payment method")
    ms -> mapM_ distinguishable ms
  where
    distinguishable m = case (KM.lookup "due" m, KM.lookup "paymentMethodFee" m) of
      (Just d, Just f) -> d `shouldNotBe` f
      _ -> pure ()

testFixturesGuardDue :: IO ()
testFixturesGuardDue =
  mapM_
    dueStaysDistinguishable
    ["payment-methods-btc", "payment-methods-xmr", "invoice-list", "invoice-list-alien-status", "invoice-list-alien-method"]

testFakeRate :: IO ()
testFakeRate = withProvider $ \fake p -> do
  readRate <- maybe (failWith "btcpay offers no current rate") pure (pCurrentRate p)
  readRate CCBtc `shouldReturn` Right (Rate "108000.12" 108000.12)
  setRates fake (J.toJSON [rateEntry "XMR_USD" [] (Just "168.25")])
  readRate CCXmr `shouldReturn` Right (Rate "168.25" 168.25)
  map (lookup "currencyPair" . frQuery) <$> apiRequests fake "GET" ["rates"] `shouldReturn` [Just (Just "BTC_USD"), Just (Just "XMR_USD")]

rateEntry :: Text -> [Text] -> Maybe Text -> J.Value
rateEntry pair errors rate = J.object ["currencyPair" .= pair, "errors" .= errors, "rate" .= rate]

testFakeRateRefusals :: IO ()
testFakeRateRefusals = withProvider $ \fake p -> do
  readRate <- maybe (failWith "btcpay offers no current rate") pure (pCurrentRate p)
  failNextCalls fake 1 500
  readRate CCBtc >>= (`shouldSatisfy` namesInError "HTTP 500")
  let refusedFor answer what = do
        setRates fake (J.toJSON answer)
        readRate CCBtc >>= (`shouldSatisfy` namesInError what)
  refusedFor [rateEntry "XMR_USD" [] (Just "168.25")] "BTC_USD"
  refusedFor [rateEntry "BTC_USD" ["coingecko: rate limited"] Nothing] "rate limited"
  refusedFor [rateEntry "BTC_USD" [] Nothing] "no rate"
  refusedFor [rateEntry "BTC_USD" [] (Just "0")] "not above 0"
  refusedFor [rateEntry "BTC_USD" [] (Just "-108000.12")] "not above 0"

testFakeInvoicePolicy :: IO ()
testFakeInvoicePolicy = withFakeBTCPay $ \fake -> do
  highSpeed <- btcpayProvider (fbConfig fake) {bSpeedPolicy = HighSpeed}
  mediumSpeed <- btcpayProvider (fbConfig fake)
  ProviderInvoice {piProviderRef = madeHigh} <- createdInvoice highSpeed (SPMCrypto CCBtc)
  ProviderInvoice {piProviderRef = madeMedium} <- createdInvoice mediumSpeed (SPMCrypto CCBtc)
  forM_ [madeHigh, madeMedium] $ \r -> setInvoiceState fake r ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  pReadInvoice mediumSpeed madeHigh `shouldReturn` Right (Just (SigSettled halfMilliBtcUnconfirmed {rcvUncreditable = Just UCHighSpeed} fixtureSettledAt))
  pReadInvoice highSpeed madeMedium `shouldReturn` Right (Just (SigSettled halfMilliBtc fixtureSettledAt))

testFakeListMoved :: IO ()
testFakeListMoved = withProvider $ \_ p ->
  pListOpen p
    `shouldReturn` movedOnly
      [ (listSettledRef, SigSettled halfMilliBtc fixtureSettledAt),
        (listProcessingRef, SigFunded xmrReceived PaidInFull)
      ]

testFakeListWindow :: IO ()
testFakeListWindow = withProvider $ \fake p -> do
  askedAt <- getCurrentTime
  _ <- pListOpen p
  answeredAt <- getCurrentTime
  gets <- apiRequests fake "GET" ["invoices"]
  case gets of
    [listed] -> do
      lookup "includePaymentMethods" (frQuery listed) `shouldBe` Just (Just "true")
      case join (lookup "startDate" (frQuery listed)) >>= readMaybe . B8.unpack of
        Nothing -> expectationFailure ("no readable startDate in " <> show (frQuery listed))
        Just sent -> do
          let window = truncate settleWindow + 60 * toInteger fakeExpiryMinutes
              seconds t = floor (utcTimeToPOSIXSeconds t) :: Integer
          sent `shouldSatisfy` \s -> s >= seconds askedAt - window && s <= seconds answeredAt - window
    _ -> expectationFailure ("expected one list request, got " <> show (length gets))

testFakeListAlienStatus :: IO ()
testFakeListAlienStatus = withProvider $ \fake p -> do
  useListFixture fake "invoice-list-alien-status"
  r <- pListOpen p
  lpMoved <$> r `shouldBe` Right [(listSettledRef, SigSettled halfMilliBtc fixtureSettledAt)]
  skippedReason r >>= (`shouldSatisfy` T.isInfixOf "Frobnicated")

testFakeListAlienMethod :: IO ()
testFakeListAlienMethod = withProvider $ \fake p -> do
  useListFixture fake "invoice-list-alien-method"
  r <- pListOpen p
  lpMoved <$> r `shouldBe` Right [(listSettledRef, SigSettled halfMilliBtc fixtureSettledAt)]
  skippedReason r >>= (`shouldSatisfy` T.isInfixOf "BTC-LN")

testFakeListNoPaymentMethods :: IO ()
testFakeListNoPaymentMethods = withProvider $ \fake p -> do
  useListFixture fake "invoice-list-no-payment-methods"
  r <- pListOpen p
  r `shouldSatisfy` namesInError "paymentMethods"

testFakeListNoId :: IO ()
testFakeListNoId = withProvider $ \fake p -> do
  useListFixture fake "invoice-list-no-id"
  r <- pListOpen p
  (skippedRef, reason) <- skipped r
  skippedRef `shouldBe` Nothing
  reason `shouldSatisfy` T.isInfixOf "id"
