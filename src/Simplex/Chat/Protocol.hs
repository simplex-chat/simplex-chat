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
{-# LANGUAGE StandaloneDeriving #-}

module Simplex.Chat.Protocol where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util ((<$?>))

data ChatDirection (p :: AParty) where
  ReceivedDirectMessage :: Connection -> Maybe Contact -> ChatDirection 'Agent
  SentDirectMessage :: Contact -> ChatDirection 'Client
  ReceivedGroupMessage :: Connection -> GroupName -> GroupMember -> ChatDirection 'Agent
  SentGroupMessage :: GroupName -> ChatDirection 'Client
  SndFileConnection :: Connection -> SndFileTransfer -> ChatDirection 'Agent
  RcvFileConnection :: Connection -> RcvFileTransfer -> ChatDirection 'Agent
  UserContactConnection :: Connection -> UserContact -> ChatDirection 'Agent

deriving instance Eq (ChatDirection p)

deriving instance Show (ChatDirection p)

fromConnection :: ChatDirection 'Agent -> Connection
fromConnection = \case
  ReceivedDirectMessage conn _ -> conn
  ReceivedGroupMessage conn _ _ -> conn
  SndFileConnection conn _ -> conn
  RcvFileConnection conn _ -> conn
  UserContactConnection conn _ -> conn

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

fromChatEventTag :: CMEventTag -> Text
fromChatEventTag = decodeLatin1 . strEncode

toChatEventTag :: Text -> Maybe CMEventTag
toChatEventTag = rightToMaybe . strDecode . B.pack . T.unpack

rightToMaybe :: Either b a -> Maybe a
rightToMaybe = either (const Nothing) Just

instance FromField CMEventTag where fromField = fromTextField_ toChatEventTag

instance ToField CMEventTag where toField = toField . fromChatEventTag

appToChatMessage :: AppMessage -> Either String ChatMessage
appToChatMessage AppMessage {event, params} = do
  eventTag <- strDecode $ encodeUtf8 event
  chatMsgEvent <- msg eventTag
  pure ChatMessage {chatMsgEvent}
  where
    p :: FromJSON a => Text -> Either String a
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
    event = fromChatEventTag . toCMEventTag $ chatMsgEvent
    o :: [(Text, J.Value)] -> J.Object
    o = H.fromList
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
      XGrpLeave -> H.empty
      XGrpDel -> H.empty
      XInfoProbe probe -> o ["probe" .= probe]
      XInfoProbeCheck probeHash -> o ["probeHash" .= probeHash]
      XInfoProbeOk probe -> o ["probe" .= probe]
      XOk -> H.empty
