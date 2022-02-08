{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Protocol where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Chat.Types
import Simplex.Chat.Util (eitherToMaybe)
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util ((<$?>))

data ConnectionEntity
  = RcvDirectMsgConnection {entityConnection :: Connection, contact :: Maybe Contact}
  | RcvGroupMsgConnection {entityConnection :: Connection, groupInfo :: GroupInfo, groupMember :: GroupMember}
  | SndFileConnection {entityConnection :: Connection, sndFileTransfer :: SndFileTransfer}
  | RcvFileConnection {entityConnection :: Connection, rcvFileTransfer :: RcvFileTransfer}
  | UserContactConnection {entityConnection :: Connection, userContact :: UserContact}
  deriving (Eq, Show)

updateEntityConnStatus :: ConnectionEntity -> ConnStatus -> ConnectionEntity
updateEntityConnStatus connEntity connStatus = case connEntity of
  RcvDirectMsgConnection c ct_ -> RcvDirectMsgConnection (st c) ((\ct -> (ct :: Contact) {activeConn = st c}) <$> ct_)
  RcvGroupMsgConnection c gInfo m@GroupMember {activeConn = c'} -> RcvGroupMsgConnection (st c) gInfo m {activeConn = st <$> c'}
  SndFileConnection c ft -> SndFileConnection (st c) ft
  RcvFileConnection c ft -> RcvFileConnection (st c) ft
  UserContactConnection c uc -> UserContactConnection (st c) uc
  where
    st c = c {connStatus}

-- chat message is sent as JSON with these properties
data AppMessage = AppMessage
  { event :: Text,
    params :: J.Object
  }
  deriving (Generic, FromJSON)

instance ToJSON AppMessage where toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

newtype ChatMessage = ChatMessage {chatMsgEvent :: ChatMsgEvent}
  deriving (Eq, Show)

instance StrEncoding ChatMessage where
  strEncode = LB.toStrict . J.encode . chatToAppMessage
  strDecode = appToChatMessage <=< J.eitherDecodeStrict'
  strP = strDecode <$?> A.takeByteString

data ChatMsgEvent
  = XMsgNew MsgContent
  | XFile FileInvitation
  | XFileAcpt String
  | XInfo Profile
  | XContact Profile (Maybe MsgContent)
  | XGrpInv GroupInvitation
  | XGrpAcpt MemberId
  | XGrpMemNew MemberInfo
  | XGrpMemIntro MemberInfo
  | XGrpMemInv MemberId IntroInvitation
  | XGrpMemFwd MemberInfo IntroInvitation
  | XGrpMemInfo MemberId Profile
  | XGrpMemCon MemberId
  | XGrpMemConAll MemberId
  | XGrpMemDel MemberId
  | XGrpLeave
  | XGrpDel
  | XInfoProbe Probe
  | XInfoProbeCheck ProbeHash
  | XInfoProbeOk Probe
  | XOk
  deriving (Eq, Show)

data MsgContentType = MCText_ | MCUnknown_

instance StrEncoding MsgContentType where
  strEncode = \case
    MCText_ -> "text"
    MCUnknown_ -> "text"
  strDecode = \case
    "text" -> Right MCText_
    _ -> Right MCUnknown_
  strP = strDecode <$?> A.takeTill (== ' ')

instance FromJSON MsgContentType where
  parseJSON = strParseJSON "MsgContentType"

instance ToJSON MsgContentType where
  toJSON = strToJSON
  toEncoding = strToJEncoding

-- TODO - include tag and original JSON into MCUnknown so that information is not lost
-- so when it serializes back it is the same as it was and chat upgrade makes it readable
data MsgContent = MCText Text | MCUnknown
  deriving (Eq, Show)

msgContentText :: MsgContent -> Text
msgContentText = \case
  MCText t -> t
  MCUnknown -> unknownMsgType

toMsgContentType :: MsgContent -> MsgContentType
toMsgContentType = \case
  MCText _ -> MCText_
  MCUnknown -> MCUnknown_

instance FromJSON MsgContent where
  parseJSON (J.Object v) = do
    v .: "type" >>= \case
      MCText_ -> MCText <$> v .: "text"
      MCUnknown_ -> pure MCUnknown
  parseJSON invalid =
    JT.prependFailure "bad MsgContent, " (JT.typeMismatch "Object" invalid)

unknownMsgType :: Text
unknownMsgType = "unknown message type"

instance ToJSON MsgContent where
  toJSON mc =
    J.object $
      ("type" .= toMsgContentType mc) : case mc of
        MCText t -> ["text" .= t]
        MCUnknown -> ["text" .= unknownMsgType]
  toEncoding mc =
    J.pairs $
      ("type" .= toMsgContentType mc) <> case mc of
        MCText t -> "text" .= t
        MCUnknown -> "text" .= unknownMsgType

data CMEventTag
  = XMsgNew_
  | XFile_
  | XFileAcpt_
  | XInfo_
  | XContact_
  | XGrpInv_
  | XGrpAcpt_
  | XGrpMemNew_
  | XGrpMemIntro_
  | XGrpMemInv_
  | XGrpMemFwd_
  | XGrpMemInfo_
  | XGrpMemCon_
  | XGrpMemConAll_
  | XGrpMemDel_
  | XGrpLeave_
  | XGrpDel_
  | XInfoProbe_
  | XInfoProbeCheck_
  | XInfoProbeOk_
  | XOk_
  deriving (Eq, Show)

instance StrEncoding CMEventTag where
  strEncode = \case
    XMsgNew_ -> "x.msg.new"
    XFile_ -> "x.file"
    XFileAcpt_ -> "x.file.acpt"
    XInfo_ -> "x.info"
    XContact_ -> "x.contact"
    XGrpInv_ -> "x.grp.inv"
    XGrpAcpt_ -> "x.grp.acpt"
    XGrpMemNew_ -> "x.grp.mem.new"
    XGrpMemIntro_ -> "x.grp.mem.intro"
    XGrpMemInv_ -> "x.grp.mem.inv"
    XGrpMemFwd_ -> "x.grp.mem.fwd"
    XGrpMemInfo_ -> "x.grp.mem.info"
    XGrpMemCon_ -> "x.grp.mem.con"
    XGrpMemConAll_ -> "x.grp.mem.con.all"
    XGrpMemDel_ -> "x.grp.mem.del"
    XGrpLeave_ -> "x.grp.leave"
    XGrpDel_ -> "x.grp.del"
    XInfoProbe_ -> "x.info.probe"
    XInfoProbeCheck_ -> "x.info.probe.check"
    XInfoProbeOk_ -> "x.info.probe.ok"
    XOk_ -> "x.ok"
  strDecode = \case
    "x.msg.new" -> Right XMsgNew_
    "x.file" -> Right XFile_
    "x.file.acpt" -> Right XFileAcpt_
    "x.info" -> Right XInfo_
    "x.contact" -> Right XContact_
    "x.grp.inv" -> Right XGrpInv_
    "x.grp.acpt" -> Right XGrpAcpt_
    "x.grp.mem.new" -> Right XGrpMemNew_
    "x.grp.mem.intro" -> Right XGrpMemIntro_
    "x.grp.mem.inv" -> Right XGrpMemInv_
    "x.grp.mem.fwd" -> Right XGrpMemFwd_
    "x.grp.mem.info" -> Right XGrpMemInfo_
    "x.grp.mem.con" -> Right XGrpMemCon_
    "x.grp.mem.con.all" -> Right XGrpMemConAll_
    "x.grp.mem.del" -> Right XGrpMemDel_
    "x.grp.leave" -> Right XGrpLeave_
    "x.grp.del" -> Right XGrpDel_
    "x.info.probe" -> Right XInfoProbe_
    "x.info.probe.check" -> Right XInfoProbeCheck_
    "x.info.probe.ok" -> Right XInfoProbeOk_
    "x.ok" -> Right XOk_
    _ -> Left "bad CMEventTag"
  strP = strDecode <$?> A.takeTill (== ' ')

toCMEventTag :: ChatMsgEvent -> CMEventTag
toCMEventTag = \case
  XMsgNew _ -> XMsgNew_
  XFile _ -> XFile_
  XFileAcpt _ -> XFileAcpt_
  XInfo _ -> XInfo_
  XContact _ _ -> XContact_
  XGrpInv _ -> XGrpInv_
  XGrpAcpt _ -> XGrpAcpt_
  XGrpMemNew _ -> XGrpMemNew_
  XGrpMemIntro _ -> XGrpMemIntro_
  XGrpMemInv _ _ -> XGrpMemInv_
  XGrpMemFwd _ _ -> XGrpMemFwd_
  XGrpMemInfo _ _ -> XGrpMemInfo_
  XGrpMemCon _ -> XGrpMemCon_
  XGrpMemConAll _ -> XGrpMemConAll_
  XGrpMemDel _ -> XGrpMemDel_
  XGrpLeave -> XGrpLeave_
  XGrpDel -> XGrpDel_
  XInfoProbe _ -> XInfoProbe_
  XInfoProbeCheck _ -> XInfoProbeCheck_
  XInfoProbeOk _ -> XInfoProbeOk_
  XOk -> XOk_

cmEventTagT :: Text -> Maybe CMEventTag
cmEventTagT = eitherToMaybe . strDecode . encodeUtf8

serializeCMEventTag :: CMEventTag -> Text
serializeCMEventTag = decodeLatin1 . strEncode

instance FromField CMEventTag where fromField = fromTextField_ cmEventTagT

instance ToField CMEventTag where toField = toField . serializeCMEventTag

appToChatMessage :: AppMessage -> Either String ChatMessage
appToChatMessage AppMessage {event, params} = do
  eventTag <- strDecode $ encodeUtf8 event
  chatMsgEvent <- msg eventTag
  pure ChatMessage {chatMsgEvent}
  where
    p :: FromJSON a => J.Key -> Either String a
    p key = JT.parseEither (.: key) params
    msg = \case
      XMsgNew_ -> XMsgNew <$> p "content"
      XFile_ -> XFile <$> p "file"
      XFileAcpt_ -> XFileAcpt <$> p "fileName"
      XInfo_ -> XInfo <$> p "profile"
      XContact_ -> XContact <$> p "profile" <*> JT.parseEither (.:? "content") params
      XGrpInv_ -> XGrpInv <$> p "groupInvitation"
      XGrpAcpt_ -> XGrpAcpt <$> p "memberId"
      XGrpMemNew_ -> XGrpMemNew <$> p "memberInfo"
      XGrpMemIntro_ -> XGrpMemIntro <$> p "memberInfo"
      XGrpMemInv_ -> XGrpMemInv <$> p "memberId" <*> p "memberIntro"
      XGrpMemFwd_ -> XGrpMemFwd <$> p "memberInfo" <*> p "memberIntro"
      XGrpMemInfo_ -> XGrpMemInfo <$> p "memberId" <*> p "profile"
      XGrpMemCon_ -> XGrpMemCon <$> p "memberId"
      XGrpMemConAll_ -> XGrpMemConAll <$> p "memberId"
      XGrpMemDel_ -> XGrpMemDel <$> p "memberId"
      XGrpLeave_ -> pure XGrpLeave
      XGrpDel_ -> pure XGrpDel
      XInfoProbe_ -> XInfoProbe <$> p "probe"
      XInfoProbeCheck_ -> XInfoProbeCheck <$> p "probeHash"
      XInfoProbeOk_ -> XInfoProbeOk <$> p "probe"
      XOk_ -> pure XOk

chatToAppMessage :: ChatMessage -> AppMessage
chatToAppMessage ChatMessage {chatMsgEvent} = AppMessage {event, params}
  where
    event = serializeCMEventTag . toCMEventTag $ chatMsgEvent
    o :: [(J.Key, J.Value)] -> J.Object
    o = JM.fromList
    params = case chatMsgEvent of
      XMsgNew content -> o ["content" .= content]
      XFile fileInv -> o ["file" .= fileInv]
      XFileAcpt fileName -> o ["fileName" .= fileName]
      XInfo profile -> o ["profile" .= profile]
      XContact profile content -> o $ maybe id ((:) . ("content" .=)) content ["profile" .= profile]
      XGrpInv groupInv -> o ["groupInvitation" .= groupInv]
      XGrpAcpt memId -> o ["memberId" .= memId]
      XGrpMemNew memInfo -> o ["memberInfo" .= memInfo]
      XGrpMemIntro memInfo -> o ["memberInfo" .= memInfo]
      XGrpMemInv memId memIntro -> o ["memberId" .= memId, "memberIntro" .= memIntro]
      XGrpMemFwd memInfo memIntro -> o ["memberInfo" .= memInfo, "memberIntro" .= memIntro]
      XGrpMemInfo memId profile -> o ["memberId" .= memId, "profile" .= profile]
      XGrpMemCon memId -> o ["memberId" .= memId]
      XGrpMemConAll memId -> o ["memberId" .= memId]
      XGrpMemDel memId -> o ["memberId" .= memId]
      XGrpLeave -> JM.empty
      XGrpDel -> JM.empty
      XInfoProbe probe -> o ["probe" .= probe]
      XInfoProbeCheck probeHash -> o ["probeHash" .= probeHash]
      XInfoProbeOk probe -> o ["probe" .= probe]
      XOk -> JM.empty
