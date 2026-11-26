{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesSignedTests (namesSignedTests) where

import Bots.StoreService.NamesOrdersTests (Ctx (..), advance, aliceOrder, invoiceRegistration, keyA, keyB, placed, post, postRaw, seconds, signWith, withOrders)
import Bots.StoreService.NamesOwnerTests (answerFree, committedPhase, paidIn, selectionBodyAt, selectionOf, stored, takenIn)
import Bots.StoreService.NamesPhasesTests (registrationPath)
import Bots.StoreService.NamesRevealTests (Fx (..), alice, committed, orderOf, ownerBody, revealStored, withReveals)
import Bots.StoreService.NamesWorkerTests (serveWorker)
import Bots.StoreService.WebTests (StubCall (..), answerFixture, answerOf, codeHashText, createBody, fieldOf, phaseOf, sampleCode, seedCheckoutCatalog, statusOf, stubCalls, supporterPriceId, webGet)
import ChatTests.Utils (writeFixture)
import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LB
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word64)
import Network.HTTP.Client (Response, responseBody, responseHeaders)
import Network.HTTP.Types (Header, hDate)
import Simplex.Chat.PaymentService.Types (InvoiceId (..))
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import StoreService.Names.Abi (hex)
import StoreService.Names.Orders (signedRequestSeconds)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderAction (..), orderRequestMessage, personalDigest)
import StoreService.Names.Store (NameRegistration (..))
import Test.Hspec

namesSignedTests :: Spec
namesSignedTests = describe "StoreService names: signed order requests" $ do
  it "signs the message simplex.domains order <id> <action> <timestamp>, recorded for the names page" testMessage
  it "answers a registration read unsigned, by another key, over another action or id, and for an unknown id alike, a timestamp outside the window 400 for a known and an unknown id alike, and the owner's read with the view" testRegistrationRead
  it "answers a retry unsigned, by another key, over another action or id, and for an unknown id alike, writing nothing, a timestamp outside the window 400 alike, and retries the owner's" testRetry
  it "answers a reveal whose request is unsigned, by another key, over another action or id, and for an unknown id alike, writing nothing, a timestamp outside the window 400 alike, and accepts the owner's" testReveal
  it "answers a name invoice read unsigned, by another key, over another action or id, and for an unknown id alike, a timestamp outside the window 400 alike, and the owner's read with the view" testInvoiceRead
  it "answers a name invoice cancel unsigned, by another key, over another action or id, and for an unknown id alike, cancelling nothing, a timestamp outside the window 400 alike, and cancels the owner's" testInvoiceCancel
  it "reads and cancels a badge invoice unsigned" testBadgeUnsigned
  it "answers a selection by another key or over another id as one for an unknown id, and a malformed one alike for a known and an unknown id, writing nothing" testSelection

type Shape = (Int, [Header], LB.ByteString)

shapeOf :: Response LB.ByteString -> Shape
shapeOf r = (statusOf r, filter ((/= hDate) . fst) $ responseHeaders r, responseBody r)

statusOfShape :: Shape -> Int
statusOfShape (status, _, _) = status

bodyOfShape :: Shape -> Maybe J.Object
bodyOfShape (_, _, body) = J.decode body

unknownRid :: Text
unknownRid = "t2Jk8RmQ4vXw9NcP3yLb6g"

unknownInvoice :: Text
unknownInvoice = "no-such-invoice"

outsideWindow :: Word64
outsideWindow = fromInteger signedRequestSeconds + 1

clockOf :: Ctx -> IO Word64
clockOf Ctx {cClock} = seconds <$> readIORef cClock

signedAs :: Ctx -> Secp256k1PrivateKey -> Text -> OrderAction -> Word64 -> IO (Word64, Text)
signedAs ctx key oid action t = (t,) <$> signWith ctx key (orderRequestMessage oid action t)

data Signed = Signed
  { sPath :: Text -> String,
    sAction :: OrderAction,
    sSend :: String -> Maybe (Word64, Text) -> IO Shape
  }

readRoute :: Ctx -> (Text -> String) -> Signed
readRoute Ctx {cClient} path = Signed {sPath = path, sAction = OARead, sSend = \p sig -> shapeOf <$> webGet cClient (p <> maybe "" query sig)}
  where
    query (t, sig) = "?timestamp=" <> show t <> "&signature=" <> T.unpack sig

changeRoute :: Ctx -> OrderAction -> (Text -> String) -> Signed
changeRoute ctx action path = Signed {sPath = path, sAction = action, sSend = \p sig -> shapeOf <$> postRaw ctx p (maybe "" (J.encode . requestFields) sig)}

revealRoute :: Ctx -> J.Object -> Signed
revealRoute ctx body = Signed {sPath = (<> "/reveal") . registrationRoute, sAction = OAReveal, sSend = \p sig -> shapeOf <$> postRaw ctx p (J.encode $ maybe body (\s -> KM.insert "request" (requestFields s) body) sig)}

requestFields :: (Word64, Text) -> J.Value
requestFields (t, sig) = J.object ["timestamp" .= t, "signature" .= sig]

otherAction :: OrderAction -> OrderAction
otherAction = \case
  OARead -> OACancel
  _ -> OARead

refusedUnlessOwner :: Ctx -> Signed -> Text -> Text -> IO ()
refusedUnlessOwner ctx Signed {sPath, sAction, sSend} oid unknown = do
  t <- clockOf ctx
  let send i = sSend (sPath i)
      signedFor key i action at = Just <$> signedAs ctx key i action at
  unknownAnswer <- send unknown =<< signedFor keyA unknown sAction t
  (statusOfShape unknownAnswer, bodyOfShape unknownAnswer) `shouldBe` (404, Just $ KM.fromList [("type", "not_found")])
  send oid Nothing `shouldReturn` unknownAnswer
  forM_
    [ ("another key", signedFor keyB oid sAction t),
      ("another action", signedFor keyA oid (otherAction sAction) t),
      ("another id", signedFor keyA unknown sAction t),
      ("another timestamp", fmap (\(_, sig) -> (t + 1, sig)) <$> signedFor keyA oid sAction t)
    ]
    $ \(what, sig) -> (what :: String,) <$> (send oid =<< sig) `shouldReturn` (what, unknownAnswer)
  forM_ [t - outsideWindow, t + outsideWindow] $ \at -> do
    stale <- send oid =<< signedFor keyA oid sAction at
    (statusOfShape stale, bodyOfShape stale) `shouldBe` (400, Just $ KM.fromList [("type", "bad_request")])
    (send unknown =<< signedFor keyA unknown sAction at) `shouldReturn` stale

ownerSends :: Ctx -> Signed -> Text -> IO (Int, Maybe J.Object)
ownerSends ctx Signed {sPath, sAction, sSend} oid = do
  t <- clockOf ctx
  shape <- sSend (sPath oid) . Just =<< signedAs ctx keyA oid sAction t
  pure (statusOfShape shape, bodyOfShape shape)

withSigned :: (Ctx -> IO a) -> IO a
withSigned action = withOrders $ \ctx -> serveWorker ctx action

registrationRoute :: Text -> String
registrationRoute = registrationPath . RegistrationId

invoiceRoute :: Text -> String
invoiceRoute iid = "/api/invoice/" <> T.unpack iid

testMessage :: IO ()
testMessage = do
  map message actions
    `shouldBe` [ "simplex.domains order t2Jk8RmQ4vXw9NcP3yLb6g read 1792828800",
                 "simplex.domains order t2Jk8RmQ4vXw9NcP3yLb6g cancel 1792828800",
                 "simplex.domains order t2Jk8RmQ4vXw9NcP3yLb6g retry 1792828800",
                 "simplex.domains order t2Jk8RmQ4vXw9NcP3yLb6g reveal 1792828800"
               ]
  writeFixture "store-service/rest-api/vectors-order-requests" $
    J.object
      [ "id" .= unknownRid,
        "timestamp" .= timestamp,
        "windowSeconds" .= signedRequestSeconds,
        "messages" .= [J.object ["action" .= textEncode action, "text" .= decodeLatin1 (message action), "digest" .= hex (personalDigest $ message action)] | action <- actions]
      ]
  where
    actions = [OARead, OACancel, OARetry, OAReveal]
    timestamp = 1792828800 :: Word64
    message action = orderRequestMessage unknownRid action timestamp

testRegistrationRead :: IO ()
testRegistrationRead = withSigned $ \ctx -> do
  (invId, _) <- placed ctx aliceOrder
  r@NameRegistration {nrId = RegistrationId rid} <- invoiceRegistration ctx invId
  let route = readRoute ctx registrationRoute
  refusedUnlessOwner ctx route rid unknownRid
  (status, view) <- ownerSends ctx route rid
  (status, (`fieldOf` "version") =<< view) `shouldBe` (200, Just $ J.toJSON $ nrVersion r)

testRetry :: IO ()
testRetry = withSigned $ \ctx -> do
  r@NameRegistration {nrId = RegistrationId rid} <- paidIn ctx aliceOrder committedPhase
  advance ctx 86401
  let route = changeRoute ctx OARetry ((<> "/retry") . registrationRoute)
  refusedUnlessOwner ctx route rid unknownRid
  stored ctx r `shouldReturn` r
  (status, view) <- ownerSends ctx route rid
  (status, phaseOf =<< view) `shouldBe` (200, Just "queued")

testReveal :: IO ()
testReveal = withReveals $ \Fx {fCtx = ctx} -> do
  rid@(RegistrationId ridText) <- committed ctx (orderOf alice)
  route <- revealRoute ctx <$> ownerBody ctx alice
  refusedUnlessOwner ctx route ridText unknownRid
  unsignedAnswer <- sSend route (sPath route ridText) Nothing
  answerFixture "reveal-not-found" (statusOfShape unsignedAnswer, bodyOfShape unsignedAnswer)
  revealStored ctx rid `shouldReturn` Nothing
  ownerSends ctx route ridText `shouldReturn` (200, Just KM.empty)

testInvoiceRead :: IO ()
testInvoiceRead = withSigned $ \ctx -> do
  (InvoiceId iid, _) <- placed ctx aliceOrder
  let route = readRoute ctx invoiceRoute
  refusedUnlessOwner ctx route iid unknownInvoice
  (status, view) <- ownerSends ctx route iid
  (status, (`fieldOf` "status") =<< view) `shouldBe` (200, Just "open")

testInvoiceCancel :: IO ()
testInvoiceCancel = withSigned $ \ctx -> do
  (invId@(InvoiceId iid), _) <- placed ctx aliceOrder
  r <- invoiceRegistration ctx invId
  let route = changeRoute ctx OACancel ((<> "/cancel") . invoiceRoute)
  refusedUnlessOwner ctx route iid unknownInvoice
  stored ctx r `shouldReturn` r
  stubCalls (cStub ctx) >>= (`shouldBe` []) . filter (\case StubCancel {} -> True; _ -> False)
  (status, view) <- ownerSends ctx route iid
  (status, (`fieldOf` "status") =<< view) `shouldBe` (200, Just "expired")

testBadgeUnsigned :: IO ()
testBadgeUnsigned = withSigned $ \ctx@Ctx {cStore, cClient} -> do
  seedCheckoutCatalog cStore
  (status, view) <- post ctx "/api/invoice" (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  status `shouldBe` 200
  Just (J.String iid) <- pure $ (`fieldOf` "invoiceId") =<< view
  (readStatus, readView) <- answerOf <$> webGet cClient (invoiceRoute iid)
  (readStatus, (`fieldOf` "badgeType") =<< readView) `shouldBe` (200, Just "supporter")
  (cancelled, cancelView) <- post ctx (invoiceRoute iid <> "/cancel") ""
  (cancelled, (`fieldOf` "status") =<< cancelView) `shouldBe` (200, Just "expired")

testSelection :: IO ()
testSelection = withSigned $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  to <- selectionOf ctx "alicebakers" 10
  t <- clockOf ctx
  let rid = nrId r
      unknown = RegistrationId unknownRid
      send i body = shapeOf <$> postRaw ctx (registrationPath i <> "/name") body
      selection key signedFor at = J.encode <$> selectionBodyAt ctx key signedFor to at
  unknownAnswer <- send unknown =<< selection keyA unknown t
  (statusOfShape unknownAnswer, bodyOfShape unknownAnswer) `shouldBe` (404, Just $ KM.fromList [("type", "not_found")])
  (send rid =<< selection keyB rid t) `shouldReturn` unknownAnswer
  (send rid =<< selection keyA unknown t) `shouldReturn` unknownAnswer
  known <- send rid "[]"
  statusOfShape known `shouldBe` 400
  send unknown "[]" `shouldReturn` known
  (send unknown =<< selection keyA unknown (t - outsideWindow)) `shouldReturn` unknownAnswer
  stored ctx r `shouldReturn` r
  fst <$> (post ctx (registrationPath rid <> "/name") =<< selection keyA rid t) `shouldReturn` 200
