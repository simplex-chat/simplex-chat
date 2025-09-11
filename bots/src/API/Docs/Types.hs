{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Types where

import API.Docs.Syntax.Types
import API.TypeInfo
import Data.Bifunctor (second)
import Data.Char (isUpper, toLower, toUpper)
import Data.List (find, mapAccumL, sortOn)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.FileTransfer.Transport
import Simplex.FileTransfer.Types hiding (RcvFileStatus) -- the type with the same name is used in simplex-chat.
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client
import Simplex.Messaging.Crypto.File
import Simplex.Messaging.Parsers (dropPrefix, fstToLower)
import Simplex.Messaging.Protocol (BlockingInfo (..), BlockingReason (..), CommandError (..), ErrorType (..), NetworkError (..), ProxyError (..))
import Simplex.Messaging.Transport
import Simplex.RemoteControl.Types
import System.Console.ANSI.Types (Color (..))

data CTDoc = CTDoc
  { typeDef :: APITypeDef,
    typeSyntax :: Expr, -- syntax for types used in commands
    typeDescr :: Text
  }

docTypeName :: CTDoc -> String
docTypeName CTDoc {typeDef = APITypeDef name _} = name

toAPIField :: ConsName -> FieldInfo -> APIRecordField
toAPIField typeName = snd . toAPIField_ typeName (S.empty, chatTypeDefs)

chatTypeDefs :: M.Map String APITypeDef
chatTypeDefs = M.fromList $ map (\CTDoc {typeDef = td@(APITypeDef name _)} -> (name, td)) chatTypesDocs

chatTypesDocs :: [CTDoc]
chatTypesDocs = sortOn docTypeName $! snd $! mapAccumL toCTDoc (S.empty, M.empty) chatTypesDocsData
  where
    toCTDoc !tds sumTypeInfo@(STI typeName _, _, _, _, typeSyntax, typeDescr) =
      let (tds', td_) = toTypeDef tds sumTypeInfo
       in case td_ of
            Just typeDef -> (tds', CTDoc {typeDef, typeSyntax, typeDescr})
            Nothing -> error $ "Recursive type: "  <> typeName

toTypeDef :: (S.Set String, M.Map String APITypeDef) -> (SumTypeInfo, SumTypeJsonEncoding, String, [ConsName], Expr, Text) -> ((S.Set String, M.Map String APITypeDef), Maybe APITypeDef)
toTypeDef acc@(!visited, !typeDefs) (STI typeName allConstrs, jsonEncoding, consPrefix, hideConstrs, _, _) =
  let constrs = filter ((`notElem` hideConstrs) . consName') allConstrs
   in case M.lookup typeName typeDefs of
        Just td -> (acc, Just td)
        Nothing
          | S.member typeName visited -> (acc, Nothing)
          | otherwise -> case jsonEncoding of
              STRecord -> case constrs of
                [RecordTypeInfo {fieldInfos}] ->
                  let fields = fromMaybe (error $ "Record type without fields: " <> typeName) $ L.nonEmpty fieldInfos
                      ((visited', typeDefs'), fields') = mapAccumL (toAPIField_ typeName) (S.insert typeName visited, typeDefs) fields
                      td = APITypeDef typeName $ ATDRecord $ L.toList fields'
                    in ((S.insert typeName visited', M.insert typeName td typeDefs'), Just td)
                _ -> error $ "Record type with " <> show (length constrs) <> " constructors: " <> typeName
              STUnion -> if length constrs > 1 then toUnionType constrs else unionError constrs
              STUnion1 -> if length constrs == 1 then toUnionType constrs else unionError constrs
              STEnum -> if length constrs > 1 then toEnumType constrs else enumError constrs
              STEnum1 -> if length constrs == 1 then toEnumType constrs else enumError constrs
              STEnum' f
                | length constrs <= 1 -> enumError constrs
                | null consPrefix -> toEnumType_ f constrs
                | otherwise -> error $ "Enum type with custom encoding and prefix: " <> typeName
  where
    toUnionType constrs =
      let ((visited', typeDefs'), members) = mapAccumL toUnionMember (S.insert typeName visited, typeDefs) $ fromMaybe (unionError constrs) $ L.nonEmpty constrs
          td = APITypeDef typeName $ ATDUnion members
        in ((S.insert typeName visited', M.insert typeName td typeDefs'), Just td)
    toUnionMember tds RecordTypeInfo {consName, fieldInfos} =
      let memberTag = normalizeConsName consPrefix consName
        in second (ATUnionMember memberTag) $ mapAccumL (toAPIField_ typeName) tds fieldInfos
    unionError constrs = error $ "Union type with " <> show (length constrs) <> " constructor(s): " <> typeName
    toEnumType = toEnumType_ $ normalizeConsName consPrefix
    toEnumType_ f constrs =
      let members = L.map toEnumMember $ fromMaybe (enumError constrs) $ L.nonEmpty constrs
          td = APITypeDef typeName $ ATDEnum members
        in ((S.insert typeName visited, M.insert typeName td typeDefs), Just td)
      where
        toEnumMember RecordTypeInfo {consName, fieldInfos} = case fieldInfos of
          [] -> f consName
          _ -> error $ "Enum type with fields in constructor: " <> typeName <> ", " <> consName
    enumError constrs = error $ "Enum type with " <> show (length constrs) <> " constructor(s): " <> typeName

toAPIField_ :: ConsName -> (S.Set String, M.Map String APITypeDef) -> FieldInfo -> ((S.Set String, M.Map String APITypeDef), APIRecordField)
toAPIField_ typeName tds (FieldInfo fieldName typeInfo) = second (APIRecordField fieldName) $ toAPIType typeInfo
  where
    toAPIType :: TypeInfo -> ((S.Set String, M.Map String APITypeDef), APIType)
    toAPIType = \case
      TIType (ST name _) -> apiTypeForName name
      TIOptional tInfo -> second ATOptional $ toAPIType tInfo
      TIArray {elemType, nonEmpty} -> second (`ATArray`nonEmpty) $ toAPIType elemType
      TIMap {keyType = ST name _, valueType}
        | name `elem` primitiveTypes -> second (ATMap (PT name)) $ toAPIType valueType
        | otherwise -> error $ "Non-primitive key type in " <> typeName <> ", " <> fieldName
    apiTypeForName :: String -> ((S.Set String, M.Map String APITypeDef), APIType)
    apiTypeForName name
      | name `elem` primitiveTypes = (tds, ATPrim $ PT name)
      | otherwise = case M.lookup name $ snd tds of
          Just td -> (tds, ATDef td)
          Nothing -> case find (\(STI name' _, _, _, _, _, _) -> name == name') chatTypesDocsData of
            Just sumTypeInfo ->
              let (tds', td_) = toTypeDef tds sumTypeInfo -- recursion to outer function, loops are resolved via type defs map lookup
                in case td_ of
                    Just td -> (tds', ATDef td)
                    Nothing -> (tds', ATRef name)
            Nothing -> error $ "Undefined type: " <> name

data SumTypeJsonEncoding = STRecord | STUnion | STUnion1 | STEnum | STEnum1 | STEnum' (ConsName -> String)

dropPfxSfx :: String -> String -> ConsName -> String
dropPfxSfx pfx sfx = dropSuffix sfx . dropPrefix pfx

fstToUpper :: String -> String
fstToUpper "" = ""
fstToUpper (h : t) = toUpper h : t

consLower :: String -> ConsName -> String
consLower pfx = map toLower . dropPrefix pfx

consSep :: String -> Char -> ConsName -> String
consSep pfx sep = foldr (\c s -> if isUpper c then sep : toLower c : s else c : s) "" . dropPrefix pfx

dropSuffix :: String -> String -> String
dropSuffix sfx s =
  let (s', sfx') = splitAt (length s - length sfx) s
   in fstToLower $ if sfx' == sfx then s' else s

normalizeConsName :: String -> ConsName -> ConsName
normalizeConsName pfx consName
  | null pfx && uppercase = consName
  | null pfx = fstToLower consName
  | uppercase = map toUpper noPfx
  | otherwise = noPfx
  where
    uppercase = all (\c -> isUpper c || c == '_') consName
    noPfx = dropPrefix pfx consName

-- making chatDir optional because clients use CIDirection? instead of CIQDirection (the type is replaced in Types.hs)
ciQuoteType :: SumTypeInfo
ciQuoteType =
  let st@(STI _ records) = sti @(CIQuote 'CTDirect)
      optChatDir f@(FieldInfo n t) = if n == "chatDir" then FieldInfo n (TIOptional t) else f
      updateRecord (RecordTypeInfo name fields) = RecordTypeInfo name $ map optChatDir fields
   in st {recordTypes = map updateRecord records} -- need to map even though there is one constructor in this type

chatTypesDocsData :: [(SumTypeInfo, SumTypeJsonEncoding, String, [ConsName], Expr, Text)]
chatTypesDocsData =
  [ ((sti @(Chat 'CTDirect)) {typeName = "AChat"}, STRecord, "", [], "", ""),
    ((sti @JSONChatInfo) {typeName = "ChatInfo"}, STUnion, "JCInfo", ["JCInfoInvalidJSON"], "", ""),
    ((sti @JSONCIContent) {typeName = "CIContent"}, STUnion, "JCI", ["JCIInvalidJSON"], "", ""),
    ((sti @JSONCIDeleted) {typeName = "CIDeleted"}, STUnion, "JCID", [], "", ""),
    ((sti @JSONCIDirection) {typeName = "CIDirection"}, STUnion, "JCI", [], "", ""),
    ((sti @JSONCIFileStatus) {typeName = "CIFileStatus"}, STUnion, "JCIFS", [], "", ""),
    ((sti @JSONCIStatus) {typeName = "CIStatus"}, STUnion, "JCIS", [], "", ""),
    (ciQuoteType, STRecord, "", [], "", ""),
    (STI "AChatItem" [RecordTypeInfo "AChatItem" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatItem" (ti "ChatItem")]], STRecord, "", [], "", ""),
    (STI "ACIReaction" [RecordTypeInfo "ACIReaction" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatReaction" (ti "CIReaction")]], STRecord, "", [], "", ""),
    -- (STI "JSONObject" [], STRecord, "", [], "Arbitrary JSON object."),
    -- (STI "UTCTime" [], STRecord, "", [], "Timestampe in ISO8601 format as string."),
    (STI "VersionRange" [RecordTypeInfo "VersionRange" [FieldInfo "minVersion" (ti TInt), FieldInfo "maxVersion" (ti TInt)]], STRecord, "", [], "", ""),
    (sti @(ChatItem 'CTDirect 'MDSnd), STRecord, "", [], "", ""),
    (sti @(CIFile 'MDSnd), STRecord, "", [], "", ""),
    (sti @(CIMeta 'CTDirect 'MDSnd), STRecord, "", [], "", ""),
    (sti @(CIReaction 'CTDirect 'MDSnd), STRecord, "", [], "", ""),
    (sti @(ContactUserPref SimplePreference), STUnion, "CUP", [], "", ""),
    (sti @(ContactUserPreference SimplePreference), STRecord, "", [], "", ""),
    (sti @(CreatedConnLink 'CMContact), STRecord, "", [], Param "connFullLink" <> Optional "" (" " <> Param "$0") "connShortLink", ""),
    (sti @AddressSettings, STRecord, "", [], "", ""),
    (sti @AgentCryptoError, STUnion, "", ["RATCHET_EARLIER", "RATCHET_SKIPPED"], "", ""), -- TODO add fields to types
    (sti @AgentErrorType, STUnion, "", [], "", ""),
    (sti @AutoAccept, STRecord, "", [], "", ""),
    (sti @BlockingInfo, STRecord, "", [], "", ""),
    (sti @BlockingReason, STEnum, "BR", [], "", ""),
    (sti @BrokerErrorType, STUnion, "", [], "", ""),
    (sti @BusinessChatInfo, STRecord, "", [], "", ""),
    (sti @BusinessChatType, STEnum, "BC", [], "", ""),
    (sti @ChatBotCommand, STUnion, "CBC", [], "", ""),
    (sti @ChatDeleteMode, STUnion, "CDM", [], Param "type" <> Choice "self" [("messages", "")] (OnOffParam "notify" "notify" (Just True)), ""),
    (sti @ChatError, STUnion, "Chat", ["ChatErrorDatabase", "ChatErrorRemoteHost", "ChatErrorRemoteCtrl"], "", ""),
    (sti @ChatErrorType, STUnion, "CE", ["CEContactNotFound", "CEServerProtocol", "CECallState", "CEInvalidChatMessage"], "", ""),
    (sti @ChatFeature, STEnum, "CF", [], "", ""),
    (sti @ChatItemDeletion, STRecord, "", [], "", "Message deletion result."),
    (sti @ChatPeerType, STEnum, "CPT", [], "", ""),
    (sti @ChatRef, STRecord, "", [], Param "chatType" <> Param "chatId" <> Optional "" (Param "$0") "chatScope", "Used in API commands. Chat scope can only be passed with groups."),
    (sti @ChatSettings, STRecord, "", [], "", ""),
    (sti @ChatStats, STRecord, "", [], "", ""),
    (sti @ChatType, STEnum, "CT", ["CTContactRequest", "CTContactConnection"], Choice "self" [("direct", "@"), ("group", "#"), ("local", "*")] "", ""),
    (sti @ChatWallpaper, STRecord, "", [], "", ""),
    (sti @ChatWallpaperScale, STEnum, "CWS", [], "", ""),
    (sti @CICallStatus, STEnum, "CISCall", [], "", ""),
    (sti @CIDeleteMode, STEnum, "CIDM", [], "", ""),
    (sti @CIForwardedFrom, STUnion, "CIFF", [], "", ""),
    (sti @CIGroupInvitation, STRecord, "", [], "", ""),
    (sti @CIGroupInvitationStatus, STEnum, "CIGIS", [], "", ""),
    (sti @CIMention, STRecord, "", [], "", ""),
    (sti @CIMentionMember, STRecord, "", [], "", ""),
    (sti @CIReactionCount, STRecord, "", [], "", ""),
    (sti @CITimed, STRecord, "", [], "", ""),
    (sti @Color, STEnum, "", [], "", ""),
    (sti @CommandError, STUnion, "", [], "", ""),
    (sti @CommandErrorType, STUnion, "", [], "", ""),
    (sti @ComposedMessage, STRecord, "", [], "", ""),
    (sti @Connection, STRecord, "", [], "", ""),
    (sti @ConnectionEntity, STUnion, "", [], "", ""),
    (sti @ConnectionErrorType, STUnion, "", [], "", ""),
    (sti @ConnectionMode, (STEnum' $ take 3 . consLower "CM"), "", [], "", ""),
    (sti @ConnectionPlan, STUnion, "CP", [], "", ""),
    (sti @ConnStatus, (STEnum' $ consSep "Conn" '-'), "", [], "", ""),
    (sti @ConnType, (STEnum' $ consSep "Conn" '_'), "", ["ConnSndFile", "ConnRcvFile"], "", ""),
    (sti @Contact, STRecord, "", [], "", ""),
    (sti @ContactAddressPlan, STUnion, "CAP", [], "", ""),
    (sti @ContactShortLinkData, STRecord, "", [], "", ""),
    (sti @ContactStatus, STEnum, "CS", [], "", ""),
    (sti @ContactUserPreferences, STRecord, "", [], "", ""),
    (sti @CryptoFile, STRecord, "", [], "", ""),
    (sti @CryptoFileArgs, STRecord, "", [], "", ""),
    (sti @E2EInfo, STRecord, "", [], "", ""),
    (sti @ErrorType, STUnion, "", [], "", ""),
    (sti @FeatureAllowed, STEnum, "FA", [], "", ""),
    (sti @FileDescr, STRecord, "", [], "", ""),
    (sti @FileError, STUnion, "FileErr", [], "", ""),
    (sti @FileErrorType, STUnion, "", [], "", ""),
    (sti @FileInvitation, STRecord, "", [], "", ""),
    (sti @FileProtocol, (STEnum' $ consLower "FP"), "", [], "", ""),
    (sti @FileStatus, STEnum, "FS", [], "", ""),
    (sti @FileTransferMeta, STRecord, "", [], "", ""),
    (sti @Format, STUnion, "", ["Unknown"], "", ""),
    (sti @FormattedText, STRecord, "", [], "", ""),
    (sti @FullGroupPreferences, STRecord, "", [], "", ""),
    (sti @FullPreferences, STRecord, "", [], "", ""),
    (sti @Group, STRecord, "", [], "", ""),
    (sti @GroupChatScope, STUnion1, "GCS", [], "(_support" <> Optional "" (":" <> Param "$0") "groupMemberId_" <> ")", ""),
    (sti @GroupChatScopeInfo, STUnion1, "GCSI", [], "", ""),
    (sti @GroupFeature, STEnum, "GF", [], "", ""),
    (sti @GroupFeatureEnabled, STEnum, "FE", [], "", ""),
    (sti @GroupInfo, STRecord, "", [], "", ""),
    (sti @GroupInfoSummary, STRecord, "", [], "", ""),
    (sti @GroupLink, STRecord, "", [], "", ""),
    (sti @GroupLinkPlan, STUnion, "GLP", [], "", ""),
    (sti @GroupMember, STRecord, "", [], "", ""),
    (sti @GroupMemberAdmission, STRecord, "", [], "", ""),
    (sti @GroupMemberCategory, (STEnum' $ dropPfxSfx "GC" "Member"), "", [], "", ""),
    (sti @GroupMemberRef, STRecord, "", [], "", ""),
    (sti @GroupMemberRole, STEnum, "GR", [], "", ""),
    (sti @GroupMemberSettings, STRecord, "", [], "", ""),
    (sti @GroupMemberStatus, (STEnum' $ (\case "group_deleted" -> "deleted"; "intro_invited" -> "intro-inv"; s -> s) . consSep "GSMem" '_'), "", [], "", ""),
    (sti @GroupPreference, STRecord, "", [], "", ""),
    (sti @GroupPreferences, STRecord, "", [], "", ""),
    (sti @GroupProfile, STRecord, "", [], "", ""),
    (sti @GroupShortLinkData, STRecord, "", [], "", ""),
    (sti @GroupSummary, STRecord, "", [], "", ""),
    (sti @GroupSupportChat, STRecord, "", [], "", ""),
    (sti @HandshakeError, STEnum, "", [], "", ""),
    (sti @InlineFileMode, STEnum, "IFM", [], "", ""),
    (sti @InvitationLinkPlan, STUnion, "ILP", [], "", ""),
    (sti @InvitedBy, STUnion, "IB", [], "", ""),
    (sti @LinkContent, STUnion, "LC", [], "", ""),
    (sti @LinkPreview, STRecord, "", [], "", ""),
    (sti @LocalProfile, STRecord, "", [], "", ""),
    (sti @MemberCriteria, STEnum1, "MC", [], "", ""),
    (sti @MsgChatLink, STUnion, "MCL", [], "", "Connection link sent in a message - only short links are allowed."),
    (sti @MsgContent, STUnion, "MC", [], "", ""),
    (sti @MsgDecryptError, STEnum, "MDE", [], "", ""),
    (sti @MsgDirection, STEnum, "MD", [], "", ""),
    (sti @MsgErrorType, STUnion, "", [], "", ""), -- check, may be correct?
    (sti @MsgFilter, STEnum, "MF", [], "", ""),
    (sti @MsgReaction, STUnion, "MR", [], "", ""),
    (sti @MsgReceiptStatus, STEnum, "MR", [], "", ""),
    (sti @NetworkError, STUnion, "NE", [], "", ""),
    (sti @NewUser, STRecord, "", [], "", ""),
    (sti @NoteFolder, STRecord, "", [], "", ""),
    (sti @PendingContactConnection, STRecord, "", [], "", ""),
    (sti @PrefEnabled, STRecord, "", [], "", ""),
    (sti @Preferences, STRecord, "", [], "", ""),
    (sti @PreparedContact, STRecord, "", [], "", ""),
    (sti @GroupDirectInvitation, STRecord, "", [], "", ""),
    (sti @PreparedGroup, STRecord, "", [], "", ""),
    (sti @Profile, STRecord, "", [], "", ""),
    (sti @ProxyClientError, STUnion, "Proxy", [], "", ""),
    (sti @ProxyError, STUnion, "", [], "", ""),
    (sti @RatchetSyncState, STEnum, "RS", [], "", ""),
    (sti @RCErrorType, STUnion, "RCE", [], "", ""),
    (sti @RcvConnEvent, STUnion, "RCE", [], "", ""),
    (sti @RcvDirectEvent, STUnion, "RDE", [], "", ""),
    (sti @RcvFileDescr, STRecord, "", [], "", ""),
    (sti @RcvFileInfo, STRecord, "", [], "", ""),
    (sti @RcvFileStatus, STUnion, "RFS", [], "", ""),
    (sti @RcvFileTransfer, STRecord, "", [], "", ""),
    (sti @RcvGroupEvent, STUnion, "RGE", [], "", ""),
    (sti @ReportReason, (STEnum' $ dropPfxSfx "RR" ""), "", ["RRUnknown"], "", ""),
    (sti @RoleGroupPreference, STRecord, "", [], "", ""),
    (sti @SecurityCode, STRecord, "", [], "", ""),
    (sti @SimplePreference, STRecord, "", [], "", ""),
    (sti @SimplexLinkType, STEnum, "XL", [], "", ""),
    (sti @SMPAgentError, STUnion, "", [], "", ""),
    (sti @SndCIStatusProgress, STEnum, "SSP", [], "", ""),
    (sti @SndConnEvent, STUnion, "SCE", [], "", ""),
    (sti @SndError, STUnion, "SndErr", [], "", ""),
    (sti @SndFileTransfer, STRecord, "", [], "", ""),
    (sti @SndGroupEvent, STUnion, "SGE", [], "", ""),
    (sti @SrvError, STUnion, "SrvErr", [], "", ""),
    (sti @StoreError, STUnion, "SE", [], "", ""),
    (sti @SwitchPhase, STEnum, "SP", [], "", ""),
    (sti @TimedMessagesGroupPreference, STRecord, "", [], "", ""),
    (sti @TimedMessagesPreference, STRecord, "", [], "", ""),
    (sti @TransportError, STUnion, "TE", [], "", ""),
    (sti @UIColorMode, STEnum, "UCM", [], "", ""),
    (sti @UIColors, STRecord, "", [], "", ""),
    (sti @UIThemeEntityOverride, STRecord, "", [], "", ""),
    (sti @UIThemeEntityOverrides, STRecord, "", [], "", ""),
    (sti @UpdatedMessage, STRecord, "", [], "", ""),
    (sti @User, STRecord, "", [], "", ""),
    (sti @UserContact, STRecord, "", [], "", ""),
    (sti @UserContactLink, STRecord, "", [], "", ""),
    (sti @UserContactRequest, STRecord, "", [], "", ""),
    (sti @UserInfo, STRecord, "", [], "", ""),
    (sti @UserProfileUpdateSummary, STRecord, "", [], "", ""),
    (sti @UserPwdHash, STRecord, "", [], "", ""),
    (sti @XFTPErrorType, STUnion, "", [], "", ""),
    (sti @XFTPRcvFile, STRecord, "", [], "", ""),
    (sti @XFTPSndFile, STRecord, "", [], "", "")

    -- (sti @DatabaseError, STUnion, "DB", [], "", ""),
    -- (sti @ChatItemInfo, STRecord, "", [], "", ""),
    -- (sti @ChatItemVersion, STRecord, "", [], "", ""),
    -- (sti @ChatListQuery, STUnion, "CLQ", [], "", ""),
    -- (sti @ChatName, STRecord, "", [], "", ""),
    -- (sti @ChatPagination, STRecord, "CP", [], "", ""),
    -- (sti @ConnectionStats, STRecord, "", [], "", ""),
    -- (sti @GroupSndStatus, STUnion, "GSS", [], "", ""),
    -- (sti @MemberDeliveryStatus, STRecord, "", [], "", ""),
    -- (sti @MemberReaction, STRecord, "", [], "", ""),
    -- (sti @MsgContentTag, (STEnum' $ dropPfxSfx "MC" '_'), "", ["MCUnknown_"], "", ""),
    -- (sti @NavigationInfo, STRecord, "", [], "", ""),
    -- (sti @PaginationByTime, STRecord, "", [], "", ""),
    -- (sti @RcvQueueInfo, STRecord, "", [], "", ""),
    -- (sti @RcvSwitchStatus, STEnum, "", [], "", ""), -- incorrect
    -- (sti @SendRef, STRecord, "", [], "", ""),
    -- (sti @SndQueueInfo, STRecord, "", [], "", ""),
    -- (sti @SndSwitchStatus, STEnum, "", [], "", ""), -- incorrect
 ]

data SimplePreference = SimplePreference {allow :: FeatureAllowed} deriving (Generic)

data RoleGroupPreference = RoleGroupPreference {enable :: GroupFeatureEnabled, role :: Maybe GroupMemberRole} deriving (Generic)

deriving instance Generic (Chat c)
deriving instance Generic (ChatItem c d)
deriving instance Generic (CIFile d)
deriving instance Generic (CIMeta c d)
deriving instance Generic (CIQuote d)
deriving instance Generic (CIReaction c d)
deriving instance Generic (ContactUserPref p)
deriving instance Generic (ContactUserPreference p)
deriving instance Generic (CreatedConnLink m)
deriving instance Generic AddressSettings
deriving instance Generic AgentCryptoError
deriving instance Generic AgentErrorType
deriving instance Generic AutoAccept
deriving instance Generic BlockingInfo
deriving instance Generic BlockingReason
deriving instance Generic BrokerErrorType
deriving instance Generic BusinessChatInfo
deriving instance Generic BusinessChatType
deriving instance Generic ChatBotCommand
deriving instance Generic ChatDeleteMode
deriving instance Generic ChatError
deriving instance Generic ChatErrorType
deriving instance Generic ChatFeature
deriving instance Generic ChatItemDeletion
deriving instance Generic ChatPeerType
deriving instance Generic ChatRef
deriving instance Generic ChatSettings
deriving instance Generic ChatStats
deriving instance Generic ChatType
deriving instance Generic ChatWallpaper
deriving instance Generic ChatWallpaperScale
deriving instance Generic CICallStatus
deriving instance Generic CIDeleteMode
deriving instance Generic CIForwardedFrom
deriving instance Generic CIGroupInvitation
deriving instance Generic CIGroupInvitationStatus
deriving instance Generic CIMention
deriving instance Generic CIMentionMember
deriving instance Generic CIReactionCount
deriving instance Generic CITimed
deriving instance Generic Color
deriving instance Generic CommandError
deriving instance Generic CommandErrorType
deriving instance Generic ComposedMessage
deriving instance Generic Connection
deriving instance Generic ConnectionEntity
deriving instance Generic ConnectionErrorType
deriving instance Generic ConnectionMode
deriving instance Generic ConnectionPlan
deriving instance Generic ConnStatus
deriving instance Generic ConnType
deriving instance Generic Contact
deriving instance Generic ContactAddressPlan
deriving instance Generic ContactShortLinkData
deriving instance Generic ContactStatus
deriving instance Generic ContactUserPreferences
deriving instance Generic CryptoFile
deriving instance Generic CryptoFileArgs
deriving instance Generic E2EInfo
deriving instance Generic ErrorType
deriving instance Generic FeatureAllowed
deriving instance Generic FileDescr
deriving instance Generic FileError
deriving instance Generic FileErrorType
deriving instance Generic FileInvitation
deriving instance Generic FileProtocol
deriving instance Generic FileStatus
deriving instance Generic FileTransferMeta
deriving instance Generic Format
deriving instance Generic FormattedText
deriving instance Generic FullGroupPreferences
deriving instance Generic FullPreferences
deriving instance Generic Group
deriving instance Generic GroupChatScope
deriving instance Generic GroupChatScopeInfo
deriving instance Generic GroupFeature
deriving instance Generic GroupFeatureEnabled
deriving instance Generic GroupInfo
deriving instance Generic GroupInfoSummary
deriving instance Generic GroupLink
deriving instance Generic GroupLinkPlan
deriving instance Generic GroupMember
deriving instance Generic GroupMemberAdmission
deriving instance Generic GroupMemberCategory
deriving instance Generic GroupMemberRef
deriving instance Generic GroupMemberRole
deriving instance Generic GroupMemberSettings
deriving instance Generic GroupMemberStatus
deriving instance Generic GroupPreference
deriving instance Generic GroupPreferences
deriving instance Generic GroupProfile
deriving instance Generic GroupShortLinkData
deriving instance Generic GroupSummary
deriving instance Generic GroupSupportChat
deriving instance Generic HandshakeError
deriving instance Generic InlineFileMode
deriving instance Generic InvitationLinkPlan
deriving instance Generic InvitedBy
deriving instance Generic JSONChatInfo
deriving instance Generic JSONCIContent
deriving instance Generic JSONCIDeleted
deriving instance Generic JSONCIDirection
deriving instance Generic JSONCIFileStatus
deriving instance Generic JSONCIStatus
deriving instance Generic LinkContent
deriving instance Generic LinkPreview
deriving instance Generic LocalProfile
deriving instance Generic MemberCriteria
deriving instance Generic MsgChatLink
deriving instance Generic MsgContent
deriving instance Generic MsgDecryptError
deriving instance Generic MsgDirection
deriving instance Generic MsgErrorType
deriving instance Generic MsgFilter
deriving instance Generic MsgReaction
deriving instance Generic MsgReceiptStatus
deriving instance Generic NetworkError
deriving instance Generic NewUser
deriving instance Generic NoteFolder
deriving instance Generic PendingContactConnection
deriving instance Generic PrefEnabled
deriving instance Generic Preferences
deriving instance Generic PreparedContact
deriving instance Generic GroupDirectInvitation
deriving instance Generic PreparedGroup
deriving instance Generic Profile
deriving instance Generic ProxyClientError
deriving instance Generic ProxyError
deriving instance Generic RatchetSyncState
deriving instance Generic RCErrorType
deriving instance Generic RcvConnEvent
deriving instance Generic RcvDirectEvent
deriving instance Generic RcvFileDescr
deriving instance Generic RcvFileInfo
deriving instance Generic RcvFileStatus
deriving instance Generic RcvFileTransfer
deriving instance Generic RcvGroupEvent
deriving instance Generic ReportReason
deriving instance Generic SecurityCode
deriving instance Generic SimplexLinkType
deriving instance Generic SMPAgentError
deriving instance Generic SndCIStatusProgress
deriving instance Generic SndConnEvent
deriving instance Generic SndError
deriving instance Generic SndFileTransfer
deriving instance Generic SndGroupEvent
deriving instance Generic SrvError
deriving instance Generic StoreError
deriving instance Generic SwitchPhase
deriving instance Generic TimedMessagesGroupPreference
deriving instance Generic TimedMessagesPreference
deriving instance Generic TransportError
deriving instance Generic UIColorMode
deriving instance Generic UIColors
deriving instance Generic UIThemeEntityOverride
deriving instance Generic UIThemeEntityOverrides
deriving instance Generic UpdatedMessage
deriving instance Generic User
deriving instance Generic UserContact
deriving instance Generic UserContactLink
deriving instance Generic UserContactRequest
deriving instance Generic UserInfo
deriving instance Generic UserProfileUpdateSummary
deriving instance Generic UserPwdHash
deriving instance Generic XFTPErrorType
deriving instance Generic XFTPRcvFile
deriving instance Generic XFTPSndFile

-- deriving instance Generic DatabaseError
-- deriving instance Generic ChatItemInfo
-- deriving instance Generic ChatItemVersion
-- deriving instance Generic ChatListQuery
-- deriving instance Generic ChatName
-- deriving instance Generic ChatPagination
-- deriving instance Generic ConnectionStats
-- deriving instance Generic Group
-- deriving instance Generic GroupSndStatus
-- deriving instance Generic MemberDeliveryStatus
-- deriving instance Generic MemberReaction
-- deriving instance Generic MsgContentTag
-- deriving instance Generic NavigationInfo
-- deriving instance Generic PaginationByTime
-- deriving instance Generic RcvQueueInfo
-- deriving instance Generic RcvSwitchStatus
-- deriving instance Generic SendRef
-- deriving instance Generic SndQueueInfo
-- deriving instance Generic SndSwitchStatus
