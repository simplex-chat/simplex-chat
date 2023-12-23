{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtocolTests where

import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Time.Clock.System (SystemTime (..), systemToUTCTime)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Ratchet
import Simplex.Messaging.Protocol (supportedSMPClientVRange)
import Simplex.Messaging.Version
import Test.Hspec

protocolTests :: Spec
protocolTests = decodeChatMessageTest

srv :: SMPServer
srv = SMPServer "smp.simplex.im" "5223" (C.KeyHash "\215m\248\251")

queue :: SMPQueueUri
queue =
  SMPQueueUri
    supportedSMPClientVRange
    SMPQueueAddress
      { smpServer = srv,
        senderId = "\223\142z\251",
        dhPublicKey = "MCowBQYDK2VuAyEAjiswwI3O/NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o="
      }

connReqData :: ConnReqUriData
connReqData =
  ConnReqUriData
    { crScheme = CRSSimplex,
      crAgentVRange = mkVersionRange 1 1,
      crSmpQueues = [queue],
      crClientData = Nothing
    }

testDhPubKey :: C.PublicKeyX448
testDhPubKey = "MEIwBQYDK2VvAzkAmKuSYeQ/m0SixPDS8Wq8VBaTS1cW+Lp0n0h4Diu+kUpR+qXx4SDJ32YGEFoGFGSbGPry5Ychr6U="

testE2ERatchetParams :: E2ERatchetParamsUri 'C.X448
testE2ERatchetParams = E2ERatchetParamsUri supportedE2EEncryptVRange testDhPubKey testDhPubKey

testConnReq :: ConnectionRequestUri 'CMInvitation
testConnReq = CRInvitationUri connReqData testE2ERatchetParams

quotedMsg :: QuotedMsg
quotedMsg =
  QuotedMsg
    (MsgRef (Just $ SharedMsgId "\5\6\7\8") (systemToUTCTime $ MkSystemTime 1 1) True Nothing)
    $ MCText "hello there!"

(==##) :: MsgEncodingI e => ByteString -> ChatMessage e -> Expectation
s ==## msg = do
  case parseChatMessages s of
    [acMsg] -> case acMsg of
      Right (ACMsg _ msg') -> case checkEncoding msg' of
        Right msg'' -> msg'' `shouldBe` msg
        Left e -> expectationFailure $ "checkEncoding error: " <> show e
      Left e -> expectationFailure $ "parse error: " <> show e
    _ -> expectationFailure "exactly one message expected"

(##==) :: MsgEncodingI e => ByteString -> ChatMessage e -> Expectation
s ##== msg = do
  let r = encodeChatMessage msg
  case r of
    ECMEncoded encodedBody ->
      J.eitherDecodeStrict' (LB.toStrict encodedBody)
        `shouldBe` (J.eitherDecodeStrict' s :: Either String J.Value)
    ECMLarge -> expectationFailure $ "large message"

(##==##) :: MsgEncodingI e => ByteString -> ChatMessage e -> Expectation
s ##==## msg = do
  s ##== msg
  s ==## msg

(==#) :: MsgEncodingI e => ByteString -> ChatMsgEvent e -> Expectation
s ==# msg = s ==## ChatMessage chatInitialVRange Nothing msg

(#==) :: MsgEncodingI e => ByteString -> ChatMsgEvent e -> Expectation
s #== msg = s ##== ChatMessage chatInitialVRange Nothing msg

(#==#) :: MsgEncodingI e => ByteString -> ChatMsgEvent e -> Expectation
s #==# msg = do
  s #== msg
  s ==# msg

testChatPreferences :: Maybe Preferences
testChatPreferences = Just Preferences {voice = Just VoicePreference {allow = FAYes}, fullDelete = Nothing, timedMessages = Nothing, calls = Nothing, reactions = Just ReactionsPreference {allow = FAYes}}

testGroupPreferences :: Maybe GroupPreferences
testGroupPreferences = Just GroupPreferences {timedMessages = Nothing, directMessages = Nothing, reactions = Just ReactionsGroupPreference {enable = FEOn}, voice = Just VoiceGroupPreference {enable = FEOn}, files = Nothing, fullDelete = Nothing, history = Nothing}

testProfile :: Profile
testProfile = Profile {displayName = "alice", fullName = "Alice", image = Just (ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="), contactLink = Nothing, preferences = testChatPreferences}

testGroupProfile :: GroupProfile
testGroupProfile = GroupProfile {displayName = "team", fullName = "Team", description = Nothing, image = Nothing, groupPreferences = testGroupPreferences}

decodeChatMessageTest :: Spec
decodeChatMessageTest = describe "Chat message encoding/decoding" $ do
  it "x.msg.new simple text" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      #==# XMsgNew (MCSimple (extMsgContent (MCText "hello") Nothing))
  it "x.msg.new simple text - timed message TTL" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"ttl\":3600}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCText "hello") Nothing (Just 3600) Nothing))
  it "x.msg.new simple text - live message" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"live\":true}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCText "hello") Nothing Nothing (Just True)))
  it "x.msg.new simple link" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"https://simplex.chat\",\"type\":\"link\",\"preview\":{\"description\":\"SimpleX Chat\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgA\",\"title\":\"SimpleX Chat\",\"uri\":\"https://simplex.chat\"}}}}"
      #==# XMsgNew (MCSimple (extMsgContent (MCLink "https://simplex.chat" $ LinkPreview {uri = "https://simplex.chat", title = "SimpleX Chat", description = "SimpleX Chat", image = ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgA", content = Nothing}) Nothing))
  it "x.msg.new simple image" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XMsgNew (MCSimple (extMsgContent (MCImage "" $ ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=") Nothing))
  it "x.msg.new simple image with text" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"here's an image\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XMsgNew (MCSimple (extMsgContent (MCImage "here's an image" $ ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=") Nothing))
  it "x.msg.new chat message" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      ##==## ChatMessage chatInitialVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew (MCSimple (extMsgContent (MCText "hello") Nothing)))
  it "x.msg.new chat message with chat version range" $
    "{\"v\":\"1-5\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      ##==## ChatMessage supportedChatVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew (MCSimple (extMsgContent (MCText "hello") Nothing)))
  it "x.msg.new quote" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello to you too\",\"type\":\"text\"},\"quote\":{\"content\":{\"text\":\"hello there!\",\"type\":\"text\"},\"msgRef\":{\"msgId\":\"BQYHCA==\",\"sent\":true,\"sentAt\":\"1970-01-01T00:00:01.000000001Z\"}}}}"
      ##==## ChatMessage
        chatInitialVRange
        (Just $ SharedMsgId "\1\2\3\4")
        (XMsgNew (MCQuote quotedMsg (extMsgContent (MCText "hello to you too") Nothing)))
  it "x.msg.new quote - timed message TTL" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello to you too\",\"type\":\"text\"},\"quote\":{\"content\":{\"text\":\"hello there!\",\"type\":\"text\"},\"msgRef\":{\"msgId\":\"BQYHCA==\",\"sent\":true,\"sentAt\":\"1970-01-01T00:00:01.000000001Z\"}},\"ttl\":3600}}"
      ##==## ChatMessage
        chatInitialVRange
        (Just $ SharedMsgId "\1\2\3\4")
        (XMsgNew (MCQuote quotedMsg (ExtMsgContent (MCText "hello to you too") Nothing (Just 3600) Nothing)))
  it "x.msg.new quote - live message" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello to you too\",\"type\":\"text\"},\"quote\":{\"content\":{\"text\":\"hello there!\",\"type\":\"text\"},\"msgRef\":{\"msgId\":\"BQYHCA==\",\"sent\":true,\"sentAt\":\"1970-01-01T00:00:01.000000001Z\"}},\"live\":true}}"
      ##==## ChatMessage
        chatInitialVRange
        (Just $ SharedMsgId "\1\2\3\4")
        (XMsgNew (MCQuote quotedMsg (ExtMsgContent (MCText "hello to you too") Nothing Nothing (Just True))))
  it "x.msg.new forward" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"forward\":true}}"
      ##==## ChatMessage chatInitialVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew $ MCForward (extMsgContent (MCText "hello") Nothing))
  it "x.msg.new forward - timed message TTL" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"forward\":true,\"ttl\":3600}}"
      ##==## ChatMessage chatInitialVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew $ MCForward (ExtMsgContent (MCText "hello") Nothing (Just 3600) Nothing))
  it "x.msg.new forward - live message" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"forward\":true,\"live\":true}}"
      ##==## ChatMessage chatInitialVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew $ MCForward (ExtMsgContent (MCText "hello") Nothing Nothing (Just True)))
  it "x.msg.new simple text with file" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XMsgNew (MCSimple (extMsgContent (MCText "hello") (Just FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileDigest = Nothing, fileConnReq = Nothing, fileInline = Nothing, fileDescr = Nothing})))
  it "x.msg.new simple file with file" $
    "{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"\",\"type\":\"file\"},\"file\":{\"fileSize\":12345,\"fileName\":\"file.txt\"}}}"
      #==# XMsgNew (MCSimple (extMsgContent (MCFile "") (Just FileInvitation {fileName = "file.txt", fileSize = 12345, fileDigest = Nothing, fileConnReq = Nothing, fileInline = Nothing, fileDescr = Nothing})))
  it "x.msg.new quote with file" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello to you too\",\"type\":\"text\"},\"quote\":{\"content\":{\"text\":\"hello there!\",\"type\":\"text\"},\"msgRef\":{\"msgId\":\"BQYHCA==\",\"sent\":true,\"sentAt\":\"1970-01-01T00:00:01.000000001Z\"}},\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      ##==## ChatMessage
        chatInitialVRange
        (Just $ SharedMsgId "\1\2\3\4")
        ( XMsgNew
            ( MCQuote
                quotedMsg
                ( extMsgContent
                    (MCText "hello to you too")
                    (Just FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileDigest = Nothing, fileConnReq = Nothing, fileInline = Nothing, fileDescr = Nothing})
                )
            )
        )
  it "x.msg.new forward with file" $
    "{\"v\":\"1\",\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"forward\":true,\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      ##==## ChatMessage chatInitialVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew $ MCForward (extMsgContent (MCText "hello") (Just FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileDigest = Nothing, fileConnReq = Nothing, fileInline = Nothing, fileDescr = Nothing})))
  it "x.msg.update" $
    "{\"v\":\"1\",\"event\":\"x.msg.update\",\"params\":{\"msgId\":\"AQIDBA==\", \"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      #==# XMsgUpdate (SharedMsgId "\1\2\3\4") (MCText "hello") Nothing Nothing
  it "x.msg.del" $
    "{\"v\":\"1\",\"event\":\"x.msg.del\",\"params\":{\"msgId\":\"AQIDBA==\"}}"
      #==# XMsgDel (SharedMsgId "\1\2\3\4") Nothing
  it "x.msg.deleted" $
    "{\"v\":\"1\",\"event\":\"x.msg.deleted\",\"params\":{}}"
      #==# XMsgDeleted
  it "x.file" $
    "{\"v\":\"1\",\"event\":\"x.file\",\"params\":{\"file\":{\"fileConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XFile FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileDigest = Nothing, fileConnReq = Just testConnReq, fileInline = Nothing, fileDescr = Nothing}
  it "x.file without file invitation" $
    "{\"v\":\"1\",\"event\":\"x.file\",\"params\":{\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XFile FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileDigest = Nothing, fileConnReq = Nothing, fileInline = Nothing, fileDescr = Nothing}
  it "x.file.acpt" $
    "{\"v\":\"1\",\"event\":\"x.file.acpt\",\"params\":{\"fileName\":\"photo.jpg\"}}"
      #==# XFileAcpt "photo.jpg"
  it "x.file.acpt.inv" $
    "{\"v\":\"1\",\"event\":\"x.file.acpt.inv\",\"params\":{\"msgId\":\"AQIDBA==\",\"fileName\":\"photo.jpg\",\"fileConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}"
      #==# XFileAcptInv (SharedMsgId "\1\2\3\4") (Just testConnReq) "photo.jpg"
  it "x.file.acpt.inv" $
    "{\"v\":\"1\",\"event\":\"x.file.acpt.inv\",\"params\":{\"msgId\":\"AQIDBA==\",\"fileName\":\"photo.jpg\"}}"
      #==# XFileAcptInv (SharedMsgId "\1\2\3\4") Nothing "photo.jpg"
  it "x.file.cancel" $
    "{\"v\":\"1\",\"event\":\"x.file.cancel\",\"params\":{\"msgId\":\"AQIDBA==\"}}"
      #==# XFileCancel (SharedMsgId "\1\2\3\4")
  it "x.info" $
    "{\"v\":\"1\",\"event\":\"x.info\",\"params\":{\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XInfo testProfile
  it "x.info with empty full name" $
    "{\"v\":\"1\",\"event\":\"x.info\",\"params\":{\"profile\":{\"fullName\":\"\",\"displayName\":\"alice\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XInfo Profile {displayName = "alice", fullName = "", image = Nothing, contactLink = Nothing, preferences = testChatPreferences}
  it "x.contact with xContactId" $
    "{\"v\":\"1\",\"event\":\"x.contact\",\"params\":{\"contactReqId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XContact testProfile (Just $ XContactId "\1\2\3\4")
  it "x.contact without XContactId" $
    "{\"v\":\"1\",\"event\":\"x.contact\",\"params\":{\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XContact testProfile Nothing
  it "x.contact with content null" $
    "{\"v\":\"1\",\"event\":\"x.contact\",\"params\":{\"content\":null,\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      ==# XContact testProfile Nothing
  it "x.contact with content (ignored)" $
    "{\"v\":\"1\",\"event\":\"x.contact\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      ==# XContact testProfile Nothing
  it "x.grp.inv" $
    "{\"v\":\"1\",\"event\":\"x.grp.inv\",\"params\":{\"groupInvitation\":{\"connRequest\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"invitedMember\":{\"memberRole\":\"member\",\"memberId\":\"BQYHCA==\"},\"groupProfile\":{\"fullName\":\"Team\",\"displayName\":\"team\",\"groupPreferences\":{\"reactions\":{\"enable\":\"on\"},\"voice\":{\"enable\":\"on\"}}},\"fromMember\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\"}}}}"
      #==# XGrpInv GroupInvitation {fromMember = MemberIdRole (MemberId "\1\2\3\4") GRAdmin, invitedMember = MemberIdRole (MemberId "\5\6\7\8") GRMember, connRequest = testConnReq, groupProfile = testGroupProfile, groupLinkId = Nothing}
  it "x.grp.inv with group link id" $
    "{\"v\":\"1\",\"event\":\"x.grp.inv\",\"params\":{\"groupInvitation\":{\"connRequest\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"invitedMember\":{\"memberRole\":\"member\",\"memberId\":\"BQYHCA==\"},\"groupProfile\":{\"fullName\":\"Team\",\"displayName\":\"team\",\"groupPreferences\":{\"reactions\":{\"enable\":\"on\"},\"voice\":{\"enable\":\"on\"}}},\"fromMember\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\"}, \"groupLinkId\":\"AQIDBA==\"}}}"
      #==# XGrpInv GroupInvitation {fromMember = MemberIdRole (MemberId "\1\2\3\4") GRAdmin, invitedMember = MemberIdRole (MemberId "\5\6\7\8") GRMember, connRequest = testConnReq, groupProfile = testGroupProfile, groupLinkId = Just $ GroupLinkId "\1\2\3\4"}
  it "x.grp.acpt without incognito profile" $
    "{\"v\":\"1\",\"event\":\"x.grp.acpt\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpAcpt (MemberId "\1\2\3\4")
  it "x.grp.mem.new" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.new\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemNew MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, v = Nothing, profile = testProfile}
  it "x.grp.mem.new with member chat version range" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.new\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"v\":\"1-5\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemNew MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, v = Just $ ChatVersionRange supportedChatVRange, profile = testProfile}
  it "x.grp.mem.intro" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.intro\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemIntro MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, v = Nothing, profile = testProfile}
  it "x.grp.mem.intro with member chat version range" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.intro\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"v\":\"1-5\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemIntro MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, v = Just $ ChatVersionRange supportedChatVRange, profile = testProfile}
  it "x.grp.mem.inv" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.inv\",\"params\":{\"memberId\":\"AQIDBA==\",\"memberIntro\":{\"directConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"groupConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}}"
      #==# XGrpMemInv (MemberId "\1\2\3\4") IntroInvitation {groupConnReq = testConnReq, directConnReq = Just testConnReq}
  it "x.grp.mem.inv w/t directConnReq" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.inv\",\"params\":{\"memberId\":\"AQIDBA==\",\"memberIntro\":{\"groupConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}}"
      #==# XGrpMemInv (MemberId "\1\2\3\4") IntroInvitation {groupConnReq = testConnReq, directConnReq = Nothing}
  it "x.grp.mem.fwd" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.fwd\",\"params\":{\"memberIntro\":{\"directConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"groupConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"},\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemFwd MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, v = Nothing, profile = testProfile} IntroInvitation {groupConnReq = testConnReq, directConnReq = Just testConnReq}
  it "x.grp.mem.fwd with member chat version range and w/t directConnReq" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.fwd\",\"params\":{\"memberIntro\":{\"groupConnReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"},\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"v\":\"1-5\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemFwd MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, v = Just $ ChatVersionRange supportedChatVRange, profile = testProfile} IntroInvitation {groupConnReq = testConnReq, directConnReq = Nothing}
  it "x.grp.mem.info" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.info\",\"params\":{\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XGrpMemInfo (MemberId "\1\2\3\4") testProfile
  it "x.grp.mem.con" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.con\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpMemCon (MemberId "\1\2\3\4")
  it "x.grp.mem.con.all" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.con.all\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpMemConAll (MemberId "\1\2\3\4")
  it "x.grp.mem.del" $
    "{\"v\":\"1\",\"event\":\"x.grp.mem.del\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpMemDel (MemberId "\1\2\3\4")
  it "x.grp.leave" $
    "{\"v\":\"1\",\"event\":\"x.grp.leave\",\"params\":{}}"
      ==# XGrpLeave
  it "x.grp.del" $
    "{\"v\":\"1\",\"event\":\"x.grp.del\",\"params\":{}}"
      ==# XGrpDel
  it "x.grp.direct.inv" $
    "{\"v\":\"1\",\"event\":\"x.grp.direct.inv\",\"params\":{\"connReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\", \"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      #==# XGrpDirectInv testConnReq (Just $ MCText "hello")
  it "x.grp.direct.inv without content" $
    "{\"v\":\"1\",\"event\":\"x.grp.direct.inv\",\"params\":{\"connReq\":\"simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}"
      #==# XGrpDirectInv testConnReq Nothing
  -- it "x.grp.msg.forward"
  --   $ "{\"v\":\"1\",\"event\":\"x.grp.msg.forward\",\"params\":{\"msgForward\":{\"memberId\":\"AQIDBA==\",\"msg\":\"{\"v\":\"1\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}\",\"msgTs\":\"1970-01-01T00:00:01.000000001Z\"}}}"
  --   #==# XGrpMsgForward
  --     (MemberId "\1\2\3\4")
  --     (ChatMessage chatInitialVRange (Just $ SharedMsgId "\1\2\3\4") (XMsgNew (MCSimple (extMsgContent (MCText "hello") Nothing))))
  --     (systemToUTCTime $ MkSystemTime 1 1)
  it "x.info.probe" $
    "{\"v\":\"1\",\"event\":\"x.info.probe\",\"params\":{\"probe\":\"AQIDBA==\"}}"
      #==# XInfoProbe (Probe "\1\2\3\4")
  it "x.info.probe.check" $
    "{\"v\":\"1\",\"event\":\"x.info.probe.check\",\"params\":{\"probeHash\":\"AQIDBA==\"}}"
      #==# XInfoProbeCheck (ProbeHash "\1\2\3\4")
  it "x.info.probe.ok" $
    "{\"v\":\"1\",\"event\":\"x.info.probe.ok\",\"params\":{\"probe\":\"AQIDBA==\"}}"
      #==# XInfoProbeOk (Probe "\1\2\3\4")
  it "x.ok" $
    "{\"v\":\"1\",\"event\":\"x.ok\",\"params\":{}}"
      ==# XOk
