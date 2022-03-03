{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtocolTests where

import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Ratchet
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Protocol (smpClientVRange)
import Test.Hspec

protocolTests :: Spec
protocolTests = decodeChatMessageTest

srv :: SMPServer
srv =
  SMPServer
    { host = "smp.simplex.im",
      port = "5223",
      keyHash = C.KeyHash "\215m\248\251"
    }

queue :: SMPQueueUri
queue =
  SMPQueueUri
    { smpServer = srv,
      senderId = "\223\142z\251",
      clientVRange = smpClientVRange,
      dhPublicKey = "MCowBQYDK2VuAyEAjiswwI3O/NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o="
    }

connReqData :: ConnReqUriData
connReqData =
  ConnReqUriData
    { crScheme = simplexChat,
      crAgentVRange = smpAgentVRange,
      crSmpQueues = [queue]
    }

testDhPubKey :: C.PublicKeyX448
testDhPubKey = "MEIwBQYDK2VvAzkAmKuSYeQ/m0SixPDS8Wq8VBaTS1cW+Lp0n0h4Diu+kUpR+qXx4SDJ32YGEFoGFGSbGPry5Ychr6U="

testE2ERatchetParams :: E2ERatchetParamsUri 'C.X448
testE2ERatchetParams = E2ERatchetParamsUri e2eEncryptVRange testDhPubKey testDhPubKey

testConnReq :: ConnectionRequestUri 'CMInvitation
testConnReq = CRInvitationUri connReqData testE2ERatchetParams

(==#) :: ByteString -> ChatMsgEvent -> Expectation
s ==# msg = do
  strDecode s `shouldBe` Right (ChatMessage msg)
  parseAll strP s `shouldBe` Right (ChatMessage msg)

(#==) :: ByteString -> ChatMsgEvent -> Expectation
s #== msg =
  J.eitherDecodeStrict' (strEncode $ ChatMessage msg)
    `shouldBe` (J.eitherDecodeStrict' s :: Either String J.Value)

(#==#) :: ByteString -> ChatMsgEvent -> Expectation
s #==# msg = do
  s #== msg
  s ==# msg

testProfile :: Profile
testProfile = Profile {displayName = "alice", fullName = "Alice", image = Just (ProfileImage "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=")}

testGroupProfile :: GroupProfile
testGroupProfile = GroupProfile {displayName = "team", fullName = "Team", image = Nothing}

decodeChatMessageTest :: Spec
decodeChatMessageTest = describe "Chat message encoding/decoding" $ do
  it "x.msg.new" $ "{\"event\":\"x.msg.new\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"}}}" #==# XMsgNew (MCText "hello")
  it "x.file" $
    "{\"event\":\"x.file\",\"params\":{\"file\":{\"fileConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23MCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"fileSize\":12345,\"fileName\":\"photo.jpg\"}}}"
      #==# XFile FileInvitation {fileName = "photo.jpg", fileSize = 12345, fileConnReq = testConnReq}
  it "x.file.acpt" $ "{\"event\":\"x.file.acpt\",\"params\":{\"fileName\":\"photo.jpg\"}}" #==# XFileAcpt "photo.jpg"
  it "x.info" $ "{\"event\":\"x.info\",\"params\":{\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}" #==# XInfo testProfile
  it "x.info" $ "{\"event\":\"x.info\",\"params\":{\"profile\":{\"fullName\":\"\",\"displayName\":\"alice\"}}}" #==# XInfo Profile {displayName = "alice", fullName = "", image = Nothing}
  it "x.contact with xContactId" $
    "{\"event\":\"x.contact\",\"params\":{\"contactReqId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XContact testProfile (Just $ XContactId "\1\2\3\4")
  it "x.contact without XContactId" $
    "{\"event\":\"x.contact\",\"params\":{\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XContact testProfile Nothing
  it "x.contact with content null" $
    "{\"event\":\"x.contact\",\"params\":{\"content\":null,\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      ==# XContact testProfile Nothing
  it "x.contact with content (ignored)" $
    "{\"event\":\"x.contact\",\"params\":{\"content\":{\"text\":\"hello\",\"type\":\"text\"},\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      ==# XContact testProfile Nothing
  it "x.grp.inv" $
    "{\"event\":\"x.grp.inv\",\"params\":{\"groupInvitation\":{\"connRequest\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23MCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"invitedMember\":{\"memberRole\":\"member\",\"memberId\":\"BQYHCA==\"},\"groupProfile\":{\"fullName\":\"Team\",\"displayName\":\"team\"},\"fromMember\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\"}}}}"
      #==# XGrpInv GroupInvitation {fromMember = MemberIdRole (MemberId "\1\2\3\4") GRAdmin, invitedMember = MemberIdRole (MemberId "\5\6\7\8") GRMember, connRequest = testConnReq, groupProfile = testGroupProfile}
  it "x.grp.acpt" $ "{\"event\":\"x.grp.acpt\",\"params\":{\"memberId\":\"AQIDBA==\"}}" #==# XGrpAcpt (MemberId "\1\2\3\4")
  it "x.grp.acpt" $ "{\"event\":\"x.grp.acpt\",\"params\":{\"memberId\":\"AQIDBA==\"}}" #==# XGrpAcpt (MemberId "\1\2\3\4")
  it "x.grp.mem.new" $
    "{\"event\":\"x.grp.mem.new\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}}"
      #==# XGrpMemNew MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, profile = testProfile}
  it "x.grp.mem.intro" $
    "{\"event\":\"x.grp.mem.intro\",\"params\":{\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}}"
      #==# XGrpMemIntro MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, profile = testProfile}
  it "x.grp.mem.inv" $
    "{\"event\":\"x.grp.mem.inv\",\"params\":{\"memberId\":\"AQIDBA==\",\"memberIntro\":{\"directConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23MCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"groupConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23MCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"}}}"
      #==# XGrpMemInv (MemberId "\1\2\3\4") IntroInvitation {groupConnReq = testConnReq, directConnReq = testConnReq}
  it "x.grp.mem.fwd" $
    "{\"event\":\"x.grp.mem.fwd\",\"params\":{\"memberIntro\":{\"directConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23MCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\",\"groupConnReq\":\"https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23MCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D\"},\"memberInfo\":{\"memberRole\":\"admin\",\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}}"
      #==# XGrpMemFwd MemberInfo {memberId = MemberId "\1\2\3\4", memberRole = GRAdmin, profile = testProfile} IntroInvitation {groupConnReq = testConnReq, directConnReq = testConnReq}
  it "x.grp.mem.info" $
    "{\"event\":\"x.grp.mem.info\",\"params\":{\"memberId\":\"AQIDBA==\",\"profile\":{\"fullName\":\"Alice\",\"displayName\":\"alice\",\"image\":\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII=\"}}}"
      #==# XGrpMemInfo (MemberId "\1\2\3\4") testProfile
  it "x.grp.mem.con" $ "{\"event\":\"x.grp.mem.con\",\"params\":{\"memberId\":\"AQIDBA==\"}}" #==# XGrpMemCon (MemberId "\1\2\3\4")
  it "x.grp.mem.con.all" $ "{\"event\":\"x.grp.mem.con.all\",\"params\":{\"memberId\":\"AQIDBA==\"}}" #==# XGrpMemConAll (MemberId "\1\2\3\4")
  it "x.grp.mem.del" $ "{\"event\":\"x.grp.mem.del\",\"params\":{\"memberId\":\"AQIDBA==\"}}" #==# XGrpMemDel (MemberId "\1\2\3\4")
  it "x.grp.leave" $ "{\"event\":\"x.grp.leave\",\"params\":{}}" ==# XGrpLeave
  it "x.grp.del" $ "{\"event\":\"x.grp.del\",\"params\":{}}" ==# XGrpDel
  it "x.info.probe" $ "{\"event\":\"x.info.probe\",\"params\":{\"probe\":\"AQIDBA==\"}}" #==# XInfoProbe (Probe "\1\2\3\4")
  it "x.info.probe.check" $ "{\"event\":\"x.info.probe.check\",\"params\":{\"probeHash\":\"AQIDBA==\"}}" #==# XInfoProbeCheck (ProbeHash "\1\2\3\4")
  it "x.info.probe.ok" $ "{\"event\":\"x.info.probe.ok\",\"params\":{\"probe\":\"AQIDBA==\"}}" #==# XInfoProbeOk (Probe "\1\2\3\4")
  it "x.ok" $ "{\"event\":\"x.ok\",\"params\":{}}" ==# XOk
