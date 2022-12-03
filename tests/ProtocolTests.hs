{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtocolTests where

import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock.System (SystemTime (..), systemToUTCTime)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Ratchet
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
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
    { crScheme = simplexChat,
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

(==##) :: MsgEncodingI e => ByteString -> ChatMessage e -> Expectation
s ==## msg = do
  strDecode s `shouldBe` Right msg
  parseAll strP s `shouldBe` Right msg

(##==) :: MsgEncodingI e => ByteString -> ChatMessage e -> Expectation
s ##== msg =
  J.eitherDecodeStrict' (strEncode msg)
    `shouldBe` (J.eitherDecodeStrict' s :: Either String J.Value)

(##==##) :: MsgEncodingI e => ByteString -> ChatMessage e -> Expectation
s ##==## msg = do
  s ##== msg
  s ==## msg

(==#) :: MsgEncodingI e => ByteString -> ChatMsgEvent e -> Expectation
s ==# msg = s ==## ChatMessage Nothing msg

(#==) :: MsgEncodingI e => ByteString -> ChatMsgEvent e -> Expectation
s #== msg = s ##== ChatMessage Nothing msg

(#==#) :: MsgEncodingI e => ByteString -> ChatMsgEvent e -> Expectation
s #==# msg = do
  s #== msg
  s ==# msg

testChatPreferences :: Maybe Preferences
testChatPreferences = Just Preferences {voice = Just Preference {allow = FAYes}, fullDelete = Nothing}

testGroupPreferences :: Maybe GroupPreferences
testGroupPreferences = Just GroupPreferences {directMessages = Nothing, voice = Just GroupPreference {enable = FEOn}, fullDelete = Nothing}

testProfile :: Profile
testProfile = Profile {displayName = "alice", fullName = "Alice", image = Just (ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII="), preferences = testChatPreferences}

testGroupProfile :: GroupProfile
testGroupProfile = GroupProfile {displayName = "team", fullName = "Team", image = Nothing, groupPreferences = testGroupPreferences}

decodeChatMessageTest :: Spec
decodeChatMessageTest = describe "Chat message encoding/decoding" $ do
  it "x.msg.new simple text" $
    "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCText "hello") Nothing))
  it "x.msg.new simple link" $
    "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"https://simplex.chat\",\"type\":\"link\",\"preview\":{\"description\":\"SimpleX Chat\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgA\",\"title\":\"SimpleX Chat\",\"uri\":\"https://simplex.chat\"}}}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCLink "https://simplex.chat" $ LinkPreview {uri = "https://simplex.chat", title = "SimpleX Chat", description = "SimpleX Chat", image = ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgA"}) Nothing))
  it "x.msg.new simple image" $
    "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCImage "" $ ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=") Nothing))
  it "x.msg.new simple image with text" $
    "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"here's an image\",\"type\":\"image\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCImage "here's an image" $ ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=") Nothing))
  it "x.msg.new chat message " $
    "{\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      ##==## ChatMessage (Just $ SharedMsgId "\1\2\3\4") (XMsgNew (MCSimple (ExtMsgContent (MCText "hello") Nothing)))
  it "x.msg.new quote" $
    "{\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello to you too\",\"type\":\"text\"},\"quote\":{\"content\":{\"text\":\"hello there!\",\"type\":\"text\"},\"msgRef\":{\"msgId\":\"BQYHCA==\",\"sent\":true,\"sentAt\":\"1970-01-01T00:00:01.000000001Z\"}}}}"
      ##==## ChatMessage
        (Just $ SharedMsgId "\1\2\3\4")
        ( XMsgNew
            ( MCQuote
                ( QuotedMsg
                    (MsgRef (Just $ SharedMsgId "\5\6\7\8") (systemToUTCTime $ MkSystemTime 1 1) True Nothing)
                    $ MCText "hello there!"
                )
                ( ExtMsgContent
                    (MCText "hello to you too")
                    Nothing
                )
            )
        )
  it "x.msg.new forward" $
    "{\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"forward\":true}}"
      ##==## ChatMessage (Just $ SharedMsgId "\1\2\3\4") (XMsgNew $ MCForward (ExtMsgContent (MCText "hello") Nothing))
  it "x.msg.new simple text with file" $
    "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCText "hello") (Just FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileConnReq = Nothing, fileInline = Nothing})))
  it "x.msg.new simple file with file" $
    "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"\",\"type\":\"file\"},\"file\":{\"fileSize\":12345,\"fileName\":\"file.txt\"}}}"
      #==# XMsgNew (MCSimple (ExtMsgContent (MCFile "") (Just FileInvitation {fileName = "file.txt", fileSize = 12345, fileConnReq = Nothing, fileInline = Nothing})))
  it "x.msg.new quote with file" $
    "{\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello to you too\",\"type\":\"text\"},\"quote\":{\"content\":{\"text\":\"hello there!\",\"type\":\"text\"},\"msgRef\":{\"msgId\":\"BQYHCA==\",\"sent\":true,\"sentAt\":\"1970-01-01T00:00:01.000000001Z\"}},\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      ##==## ChatMessage
        (Just $ SharedMsgId "\1\2\3\4")
        ( XMsgNew
            ( MCQuote
                ( QuotedMsg
                    (MsgRef (Just $ SharedMsgId "\5\6\7\8") (systemToUTCTime $ MkSystemTime 1 1) True Nothing)
                    $ MCText "hello there!"
                )
                ( ExtMsgContent
                    (MCText "hello to you too")
                    (Just FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileConnReq = Nothing, fileInline = Nothing})
                )
            )
        )
  it "x.msg.new forward with file" $
    "{\"msgId\":\"AQIDBA==\",\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"forward\":true,\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      ##==## ChatMessage (Just $ SharedMsgId "\1\2\3\4") (XMsgNew $ MCForward (ExtMsgContent (MCText "hello") (Just FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileConnReq = Nothing, fileInline = Nothing})))
  it "x.msg.update" $
    "{\"event\":\"x.msg.update\",\"params\":{\"msgId\":\"AQIDBA==\", \"content\":{\"text\":\"hello\",\"type\":\"text\"}}}"
      #==# XMsgUpdate (SharedMsgId "\1\2\3\4") (MCText "hello")
  it "x.msg.del" $
    "{\"event\":\"x.msg.del\",\"params\":{\"msgId\":\"AQIDBA==\"}}"
      #==# XMsgDel (SharedMsgId "\1\2\3\4")
  it "x.msg.deleted" $
    "{\"event\":\"x.msg.deleted\",\"params\":{}}"
      #==# XMsgDeleted
  it "x.file" $
    "{\"event\":\"x.file\",\"params\":{\"file\":{\"fileConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XFile FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileConnReq = Just testConnReq, fileInline = Nothing}
  it "x.file without file invitation" $
    "{\"event\":\"x.file\",\"params\":{\"file\":{\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XFile FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileConnReq = Nothing, fileInline = Nothing}
  it "x.file.acpt" $
    "{\"event\":\"x.file.acpt\",\"params\":{\"fileName\":\"photo.jpg\"}}"
      #==# XFileAcpt "photo.jpg"
  it "x.file.acpt.inv" $
    "{\"event\":\"x.file.acpt.inv\",\"params\":{\"msgId\":\"AQIDBA==\",\"fileName\":\"photo.jpg\",\"fileConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}"
      #==# XFileAcptInv (SharedMsgId "\1\2\3\4") (Just testConnReq) "photo.jpg"
  it "x.file.acpt.inv" $
    "{\"event\":\"x.file.acpt.inv\",\"params\":{\"msgId\":\"AQIDBA==\",\"fileName\":\"photo.jpg\"}}"
      #==# XFileAcptInv (SharedMsgId "\1\2\3\4") Nothing "photo.jpg"
  it "x.file.cancel" $
    "{\"event\":\"x.file.cancel\",\"params\":{\"msgId\":\"AQIDBA==\"}}"
      #==# XFileCancel (SharedMsgId "\1\2\3\4")
  it "x.info" $
    "{\"event\":\"x.info\",\"params\":{\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XInfo testProfile
  it "x.info with empty full name" $
    "{\"event\":\"x.info\",\"params\":{\"profile\":{\"fullName\":\"\",\"displayName\":\"alice\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XInfo Profile {displayName = "alice", fullName = "", image = Nothing, preferences = testChatPreferences}
  it "x.contact with xContactId" $
    "{\"event\":\"x.contact\",\"params\":{\"contactReqId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XContact testProfile (Just $ XContactId "\1\2\3\4")
  it "x.contact without XContactId" $
    "{\"event\":\"x.contact\",\"params\":{\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XContact testProfile Nothing
  it "x.contact with content null" $
    "{\"event\":\"x.contact\",\"params\":{\"content\":null,\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      ==# XContact testProfile Nothing
  it "x.contact with content (ignored)" $
    "{\"event\":\"x.contact\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      ==# XContact testProfile Nothing
  it "x.grp.inv" $
    "{\"event\":\"x.grp.inv\",\"params\":{\"groupInvitation\":{\"connRequest\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"invitedMember\":{\"memberRole\":\"member\",\"memberId\":\"BQYHCA==\"},\"groupProfile\":{\"fullName\":\"Team\",\"displayName\":\"team\",\"groupPreferences\":{\"voice\":{\"enable\":\"on\"}}},\"fromMember\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\"}}}}"
      #==# XGrpInv GroupInvitation {fromMember = MemberIdRole (MemberId "\1\2\3\4") GRAdmin, invitedMember = MemberIdRole (MemberId "\5\6\7\8") GRMember, connRequest = testConnReq, groupProfile = testGroupProfile, groupLinkId = Nothing}
  it "x.grp.inv with group link id" $
    "{\"event\":\"x.grp.inv\",\"params\":{\"groupInvitation\":{\"connRequest\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"invitedMember\":{\"memberRole\":\"member\",\"memberId\":\"BQYHCA==\"},\"groupProfile\":{\"fullName\":\"Team\",\"displayName\":\"team\",\"groupPreferences\":{\"voice\":{\"enable\":\"on\"}}},\"fromMember\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\"}, \"groupLinkId\":\"AQIDBA==\"}}}"
      #==# XGrpInv GroupInvitation {fromMember = MemberIdRole (MemberId "\1\2\3\4") GRAdmin, invitedMember = MemberIdRole (MemberId "\5\6\7\8") GRMember, connRequest = testConnReq, groupProfile = testGroupProfile, groupLinkId = Just $ GroupLinkId "\1\2\3\4"}
  it "x.grp.acpt without incognito profile" $
    "{\"event\":\"x.grp.acpt\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpAcpt (MemberId "\1\2\3\4")
  it "x.grp.mem.new" $
    "{\"event\":\"x.grp.mem.new\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemNew MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, profile = testProfile}
  it "x.grp.mem.intro" $
    "{\"event\":\"x.grp.mem.intro\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemIntro MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, profile = testProfile}
  it "x.grp.mem.inv" $
    "{\"event\":\"x.grp.mem.inv\",\"params\":{\"memberId\":\"AQIDBA==\",\"memberIntro\":{\"directConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"groupConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}}"
      #==# XGrpMemInv (MemberId "\1\2\3\4") IntroInvitation {groupConnReq = testConnReq, directConnReq = testConnReq}
  it "x.grp.mem.fwd" $
    "{\"event\":\"x.grp.mem.fwd\",\"params\":{\"memberIntro\":{\"directConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"groupConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D1-2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"},\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}}"
      #==# XGrpMemFwd MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, profile = testProfile} IntroInvitation {groupConnReq = testConnReq, directConnReq = testConnReq}
  it "x.grp.mem.info" $
    "{\"event\":\"x.grp.mem.info\",\"params\":{\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\",\"preferences\":{\"voice\":{\"allow\":\"yes\"}}}}}"
      #==# XGrpMemInfo (MemberId "\1\2\3\4") testProfile
  it "x.grp.mem.con" $
    "{\"event\":\"x.grp.mem.con\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpMemCon (MemberId "\1\2\3\4")
  it "x.grp.mem.con.all" $
    "{\"event\":\"x.grp.mem.con.all\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpMemConAll (MemberId "\1\2\3\4")
  it "x.grp.mem.del" $
    "{\"event\":\"x.grp.mem.del\",\"params\":{\"memberId\":\"AQIDBA==\"}}"
      #==# XGrpMemDel (MemberId "\1\2\3\4")
  it "x.grp.leave" $
    "{\"event\":\"x.grp.leave\",\"params\":{}}"
      ==# XGrpLeave
  it "x.grp.del" $
    "{\"event\":\"x.grp.del\",\"params\":{}}"
      ==# XGrpDel
  it "x.info.probe" $
    "{\"event\":\"x.info.probe\",\"params\":{\"probe\":\"AQIDBA==\"}}"
      #==# XInfoProbe (Probe "\1\2\3\4")
  it "x.info.probe.check" $
    "{\"event\":\"x.info.probe.check\",\"params\":{\"probeHash\":\"AQIDBA==\"}}"
      #==# XInfoProbeCheck (ProbeHash "\1\2\3\4")
  it "x.info.probe.ok" $
    "{\"event\":\"x.info.probe.ok\",\"params\":{\"probe\":\"AQIDBA==\"}}"
      #==# XInfoProbeOk (Probe "\1\2\3\4")
  it "x.ok" $
    "{\"event\":\"x.ok\",\"params\":{}}"
      ==# XOk
