{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.BadgeBTCPayTests (badgeBTCPayTests) where

import BadgeService.Config (BTCPayConfig (..))
import BadgeService.Providers
  ( Funded (..),
    ListPass (..),
    OrderDraft (..),
    PaymentSignal (..),
    Provider (..),
    ProviderError (..),
    ProviderInvoice (..),
    Received (..),
    WebhookError (..),
    settleWindow,
  )
import BadgeService.Providers.BTCPay (btcMethodId, btcpayProvider, listSignals, minorToDecimal, paymentMethodsSignal, verifyBTCPaySig, xmrMethodId)
import Bots.FakeBTCPay
import Control.Exception (evaluate)
import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Char (toUpper)
import Data.Foldable (toList)
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
  describe "the list pass" $ do
    it "skips an invoice whose status it does not know, and settles the rest" testListSkipsUnknownStatus
    it "skips an invoice it cannot parse, and settles the rest" testListSkipsUnparseable
    it "skips an invoice paid by a method it does not know, and settles the rest" testListSkipsUnknownMethod
    it "fails the pass when the payment methods are absent, rather than reporting health" testListFailsWithoutPaymentMethods
    it "skips an invoice it cannot even name, unaccounted for" testListSkipsWithoutId
    it "fails the pass on a response it cannot read" testListFailsOnUnreadableBody
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
    it "closes on Expired and on Invalid" testFakeClosed
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
    it "keeps due distinguishable from networkFee, so the wrong field cannot be read" testFixturesGuardDue
  describe "the list pass against the fake Greenfield" $ do
    it "returns the invoices whose state moved and omits the one that did not" testFakeListMoved
    it "bounds the request at 72 hours plus one invoice window" testFakeListWindow
    it "skips an invoice whose status it does not know, and settles the rest" testFakeListAlienStatus
    it "skips an invoice paid by a method it does not know, and settles the rest" testFakeListAlienMethod
    it "fails the pass when the payment methods are absent" testFakeListNoPaymentMethods
    it "skips an invoice it cannot even name, unaccounted for" testFakeListNoId

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
      "networkFee" .= ("0.00000500" :: Text),
      "payments" .= payments
    ]

-- | What BTCPay is still owed on a 0.00050000 invoice, per payment figure. The dust case is
-- deliberately not the difference: a partial payment adds a network fee, so a body computed by
-- subtraction would also pass an adapter that subtracted instead of reading the field.
dueAfter :: Text -> Text
dueAfter = \case
  "0.00050000" -> "0.00000000"
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
signalOf status methods = paymentMethodsSignal readTime ref status (methodsBody methods)

halfMilliBtc :: Received
halfMilliBtc = Received {rcvAmount = CurrencyAmount 5400, rcvCrypto = Just "0.00050000", rcvDue = Just "0.00000000"}

-- | Nothing arrived, so what is owed is whatever the source says: the inline bodies derive it
-- from the payment, the fixtures carry their own.
nothingReceived :: Text -> Received
nothingReceived due = Received {rcvAmount = CurrencyAmount 0, rcvCrypto = Nothing, rcvDue = Just due}

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
    `shouldBe` Right (Just (SigFunded halfMilliBtc PaidInFull))

testClosed :: IO ()
testClosed = do
  signalOf "Expired" [btcPaid "0.00050000" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigClosed halfMilliBtc))
  signalOf "Invalid" [btcPaid "0.00000000" []]
    `shouldBe` Right (Just (SigClosed (nothingReceived "0.00050000")))

testNewWithPayment :: IO ()
testNewWithPayment =
  signalOf "New" [btcPaid "0.00050000" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigFunded halfMilliBtc PaidInPart))

testNewWithDust :: IO ()
testNewWithDust =
  signalOf "New" [btcPaid "0.00000001" [payment "Processing" 1700000000]]
    `shouldBe` Right (Just (SigFunded Received {rcvAmount = CurrencyAmount 0, rcvCrypto = Just "0.00000001", rcvDue = Just "0.00050499"} PaidInPart))

testNewWithNothing :: IO ()
testNewWithNothing = signalOf "New" [btcPaid "0.00000000" []] `shouldBe` Right Nothing

-- | A figure with an absurd exponent parses as a Scientific and then asks for a number with
-- that many digits. Bounded before anything rounds or formats it, because the thread that would
-- build it is the poller. The timeout is the point: without the guard this does not return.
testAbsurdExponentIsRefused :: IO ()
testAbsurdExponentIsRefused = do
  answered <- timeout 5000000 (evaluate (signalOf "Settled" [btcPaid "1e1000000000" []]))
  case answered of
    Nothing -> expectationFailure "a decimal with a billion-digit exponent was not refused"
    Just r -> r `shouldSatisfy` namesInError "exponent out of range"

-- | The same figure as a bare JSON number, which is how BTCPay sends `receivedDate`. The
-- refusal must not format it on the way out either, or the guard rebuilds what it refuses.
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
            "networkFee" .= ("0.00000500" :: Text),
            "payments" .= ([] :: [J.Value])
          ]
  answered <- timeout 5000000 (evaluate (signalOf "Settled" [method]))
  case answered of
    Nothing -> expectationFailure "a JSON number with a billion-digit exponent was not refused"
    Just r -> r `shouldSatisfy` namesInError "exponent out of range"

-- | A rate that carries a repeating division at full decimal scale is a real figure, and the
-- magnitude guard must not be the thing that fails a whole list pass over one of them.
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
          "networkFee" .= ("0.00000500" :: Text),
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
  paymentMethodsSignal readTime ref "Settled" "<html>502 Bad Gateway</html>" `shouldSatisfy` namesInError "could not read the response"

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
      "currency" .= ("USD" :: Text)
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

-- | The decode is per invoice, not per page: BTCPay's payments come from a plugin for XMR, and
-- one malformed entry failing the whole page would stop every invoice settling, on every pass.
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
            "networkFee" .= ("0.00000500" :: Text),
            -- a payment with no status: the field GPayment requires to read one at all
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

-- | Skipped with no ref, which the poller counts as unaccounted for: it holds the sweep back
-- exactly as failing the pass did, and the invoices either side of it still settle.
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

-- The exponent gate bounds how long a figure may be, not how large: a negative or an
-- eleven-digit one still reaches the conversion. Unclamped both wrap Word32 into a positive
-- figure, which reads as money received: the invoice then holds against every sweep and answers
-- every cancel with `funded`.
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
  case paymentMethodsSignal readTime ref "Settled" asNumbers of
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
    ["InvoiceProcessing", "InvoiceSettled", "InvoiceExpired", "InvoiceInvalid"]

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
  let b = webhookEvent "InvoiceReceivedPayment" ref
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
  Right _ -> False

btcAddress, xmrAddress :: Text
btcAddress = "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"
xmrAddress = "44AFFq5kSiGBoZ4NMDwYtN18obc8AemS33DBLWs3H7otXft3XjrpDtQGv7SqSsaBYBb98uNbr2VBBEt7f2wfn3RVGQBEP3A"

fixtureSettledAt :: UTCTime
fixtureSettledAt = posixSecondsToUTCTime 1700003600

listSettledRef, listProcessingRef :: Text
listSettledRef = "SettledInvoiceRefAAAA"
listProcessingRef = "ProcessingInvoiceRefB"

xmrReceived :: Received
xmrReceived = Received {rcvAmount = CurrencyAmount 5400, rcvCrypto = Just "0.32095000", rcvDue = Just "0.00000000"}

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
    `shouldReturn` Right (Just (SigFunded Received {rcvAmount = CurrencyAmount 2700, rcvCrypto = Just "0.00025000", rcvDue = Just "0.00000000"} PaidInPart))
  setInvoiceState fake invRef ["status" .= ("Processing" :: Text), "additionalStatus" .= ("PaidPartial" :: Text)]
  pReadInvoice p invRef
    `shouldReturn` Right (Just (SigFunded Received {rcvAmount = CurrencyAmount 2700, rcvCrypto = Just "0.00025000", rcvDue = Just "0.00000000"} PaidInFull))
  setInvoiceState fake invRef ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigSettled halfMilliBtc fixtureSettledAt))
  details <- apiRequests fake "GET" ["invoices", invRef]
  methods <- apiRequests fake "GET" ["invoices", invRef, "payment-methods"]
  (length details, length methods) `shouldBe` (4, 5)

testFakeClosed :: IO ()
testFakeClosed = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = invRef} <- createdInvoice p (SPMCrypto CCBtc)
  setInvoiceState fake invRef ["status" .= ("Expired" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigClosed halfMilliBtc))
  -- nothing arrived, so the whole amount is still owed - the same figure the inline body in
  -- `testClosed` carries, and the real provider reports that figure for an unpaid invoice
  setInvoiceState fake invRef ["status" .= ("Invalid" :: Text), "paymentMethodPaid" .= ("0.00000000" :: Text)]
  pReadInvoice p invRef `shouldReturn` Right (Just (SigClosed (nothingReceived "0.00050000")))

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

-- | The poller thread is the one that settles orders. A provider answering with something
-- enormous must be refused at the read rather than held in memory entire.
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
  pVerifyWebhook p (webhookSigHeader configured b) (LB.toStrict b) `shouldBe` Right (Just ref)
  pVerifyWebhook p (webhookSigHeader (configured <> "0") b) (LB.toStrict b) `shouldSatisfy` isRefused

-- | Telling totalPaid from paymentMethodPaid only means anything while the two differ in
-- the fixture, so we check that directly.
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

-- | With due equal to networkFee a fixture cannot tell which field the adapter reads, and
-- reading the fee would report a part-paid invoice as covered.
dueStaysDistinguishable :: HasCallStack => Text -> IO ()
dueStaysDistinguishable name = do
  v <- fixtureResponse name
  case paymentMethodsIn v of
    [] -> expectationFailure (T.unpack name <> " carries no payment method")
    ms -> mapM_ distinguishable ms
  where
    distinguishable m = case (KM.lookup "due" m, KM.lookup "networkFee" m) of
      (Just d, Just f) -> d `shouldNotBe` f
      _ -> pure ()

testFixturesGuardDue :: IO ()
testFixturesGuardDue =
  mapM_
    dueStaysDistinguishable
    ["payment-methods-btc", "payment-methods-xmr", "invoice-list", "invoice-list-alien-status", "invoice-list-alien-method"]

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
