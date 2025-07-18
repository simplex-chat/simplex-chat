{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Docs.Types where

import API.TypeInfo
import Data.Text (Text)

data CTDoc = CTDoc {consName :: ConsName, typeDescr :: Text, typeInfo :: [RecordTypeInfo]}

instance ConstructorName CTDoc where consName' CTDoc {consName} = consName

chatTypesDocs :: [CTDoc]
chatTypesDocs = map toCTDoc chatTypesDocsData
  where
    toCTDoc (consName, typeDescr, typeInfo) = CTDoc {consName, typeDescr, typeInfo}

primitiveTypes :: [ConsName]
primitiveTypes = ["Bool", "Int", "Int64", "String"]

chatTypesDocsData :: [(ConsName, Text, [RecordTypeInfo])]
chatTypesDocsData =
  [ ("ACIReaction", "", []),
    ("AChat", "", []),
    ("AChatInfo", "", []),
    ("AChatItem", "", []),
    ("AConnectionLink", "", []),
    ("ACreatedConnLink", "", []),
    ("AddressSettings", "", []),
    ("CIDeleteMode", "", []),
    ("ChatDeleteMode", "", []),
    ("ChatItemDeletion", "", []),
    ("ChatItemInfo", "", []),
    ("ChatListQuery", "", []),
    ("ChatName", "", []),
    ("ChatPagination", "", []),
    ("ChatRef", "", []),
    ("ChatSettings", "", []),
    ("ComposedMessage", "", []),
    ("ConnectionPlan", "", []),
    ("ConnectionStats", "", []),
    ("Contact", "", []),
    ("CreatedConnLink 'CMContact", "", []),
    ("CreatedConnLink 'CMInvitation", "", []),
    ("FileTransferMeta", "", []),
    ("Group", "", []),
    ("GroupInfo", "", []),
    ("GroupInfoSummary", "", []),
    ("GroupLink", "", []),
    ("GroupMember", "", []),
    ("GroupMemberRole", "", []),
    ("GroupProfile", "", []),
    ("MemberReaction", "", []),
    ("MsgContentTag", "", []),
    ("MsgFilter", "", []),
    ("MsgReaction", "", []),
    ("NavigationInfo", "", []),
    ("NewUser", "", []),
    ("PaginationByTime", "", []),
    ("PendingContactConnection", "", []),
    ("Preferences", "", []),
    ("Profile", "", []),
    ("RcvFileTransfer", "", []),
    ("ReportReason", "", []),
    ("SendRef", "", []),
    ("SndFileTransfer", "", []),
    ("UpdatedMessage", "", []),
    ("User", "", []),
    ("UserContactLink", "", []),
    ("UserContactRequest", "", []),
    ("UserInfo", "", []),
    ("UserProfileUpdateSummary", "", [])
  ]
