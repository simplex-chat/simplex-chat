{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Events where

import API.Docs.Types
import API.TypeInfo
import Data.List (find)
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Messaging.Parsers (dropPrefix)

data CECategory = CECategory
  { categoryName :: String,
    categoryDescr :: String,
    mainEvents :: [CEDoc],
    otherEvents :: [CEDoc]
  }

data CEDoc = CEDoc
  { consName :: ConsName,
    eventType :: ATUnionMember,
    eventDescr :: String
  }

instance ConstructorName CEDoc where consName' CEDoc {consName} = consName

chatEventsDocs :: [CECategory]
chatEventsDocs = map toCategory chatEventsDocsData
  where
    toCategory (categoryName, categoryDescr, mainEvts, otherEvts) =
      CECategory {categoryName, categoryDescr, mainEvents = map toEvt mainEvts, otherEvents = map toEvt otherEvts}
    toEvt (consName, eventDescr)
      | consName == "CEvtChatError" =
          let field = toAPIField consName $ FieldInfo "chatError" (ti "ChatError")
              eventType = ATUnionMember (dropPrefix "CEvt" consName) [field]
           in CEDoc {consName, eventType, eventDescr}
      | otherwise = case find ((consName ==) . consName') chatEventsTypeInfo of
          Just RecordTypeInfo {fieldInfos} ->
            let fields = map (toAPIField consName) fieldInfos
                eventType = ATUnionMember (dropPrefix "CEvt" consName) fields
            in CEDoc {consName, eventType, eventDescr}
          Nothing -> error $ "Missing event type info for " <> consName

deriving instance Generic ChatEvent

chatEventsTypeInfo :: [RecordTypeInfo]
chatEventsTypeInfo = recordTypesInfo @ChatEvent

chatEventsDocsData :: [(String, String, [(ConsName, String)], [(ConsName, String)])]
chatEventsDocsData =
  [ ( "Contact connection events", -- which event should be processed by a bot that has business address. Maybe needs a separate category.
      "Bots must use these events to process connecting users.\n\n\
      \Most bots enable auto-accept and don't need to accept connections via commands.\n\n\
      \You may create bot SimpleX address manually via CLI or desktop app or from bot code with these commands:\n\
      \- [APIShowMyAddress](./COMMANDS.md#apishowmyaddress) to check if address exists,\n\
      \- [APICreateMyAddress](./COMMANDS.md#apicreatemyaddress) to create address,\n\
      \- [APISetAddressSettings](./COMMANDS.md#apisetaddresssettings) to enable auto-access.",
      [ ( "CEvtContactConnected", "This event is sent after a user connects via bot SimpleX address (not a business address).")
      ],
      [
        ("CEvtContactUpdated", ""),
        ("CEvtContactDeletedByContact", ""),
        ("CEvtReceivedContactRequest", "received - needs to be accepted"),
        ("CEvtNewMemberContactReceivedInv", "only needs to be processed to associate contact with group"),
        ("CEvtContactSndReady", "") -- JOINED - can send messages
      ]
    ),
    ( "Message events",
      "Bots must use these events to process received messages.",
      [ ("CEvtNewChatItems", "")
      ],
      [ ("CEvtChatItemReaction", ""),
        ("CEvtChatItemsDeleted", ""),
        ("CEvtChatItemUpdated", ""),
        ("CEvtGroupChatItemsDeleted", ""),
        ("CEvtChatItemsStatusesUpdated", "")
      ]
    ),
    ( "Group events",
      "Bots may use these events to manage users' groups and business address groups.\n\n\
      \*Please note*: programming groups is more complex than programming direct connections",
      [ ("CEvtReceivedGroupInvitation", ""),
        ("CEvtUserJoinedGroup", ""),
        ("CEvtGroupUpdated", ""),
        ("CEvtJoinedGroupMember", ""),
        ("CEvtMemberRole", ""),
        ("CEvtDeletedMember", ""),
        ("CEvtLeftMember", ""),
        ("CEvtDeletedMemberUser", ""),
        ("CEvtGroupDeleted", "")
      ],
      [ ("CEvtConnectedToGroupMember", ""),
        ("CEvtMemberAcceptedByOther", ""),
        ("CEvtMemberBlockedForAll", ""),
        ("CEvtGroupMemberUpdated", "")
      ]
    ),
    ( "File events",
      "Bots that send or receive files may process these events to track delivery status and to process completion.\n\n\
      \Bots that need to receive or moderate files (e.g., based on name, size or extension), \
      \can use relevant commands (e.g., [ReceiveFile](./COMMANDS.md#receivefile) or \
      \[APIDeleteMemberChatItem](./COMMANDS.md#apideletememberchatitem)) \
      \when processing [NewChatItems](#newchatitems) event.\n\n\
      \Bots that need to send files should use [APISendMessages](./COMMANDS.md#apisendmessages) command.",
      [ ("CEvtRcvFileDescrReady", ""),
        ("CEvtRcvFileComplete", ""),
        ("CEvtSndFileCompleteXFTP", "")
      ],
      [ ("CEvtRcvFileStart", "file reception started (happens when FD is received)"),
        ("CEvtRcvFileSndCancelled", "sender cancelled sending file"),
        ("CEvtRcvFileAccepted", "when file auto-accepted - not recommended"),
        ("CEvtRcvFileError", ""),
        ("CEvtRcvFileWarning", ""),
        ("CEvtSndFileError", ""),
        ("CEvtSndFileWarning", "")
      ]
    ),
    ( "Connection progress events",
      "Bots may use these events to track progress of connections for monitoring or debugging.",
      [ ("CEvtAcceptingContactRequest", ""),
        ("CEvtAcceptingBusinessRequest", ""),
        ("CEvtContactConnecting", ""), -- CONF
        ("CEvtBusinessLinkConnecting", ""), -- CONF
        ("CEvtJoinedGroupMemberConnecting", ""), -- MSG
        ("CEvtSentGroupInvitation", ""), -- INV
        ("CEvtGroupLinkConnecting", "") -- CONF
      ],
      []
    ),
    ( "Error events",
      "Bots may log these events for debugging. \
      \There will be many error events - this does NOT indicate a malfunction - \
      \e.g., they may happen because of bad network connectivity, \
      \or because messages may be delivered to deleted chats for a short period of time \
      \(they will be ignored).",
      [ ("CEvtMessageError", ""),
        ("CEvtChatError", ""), -- only used in WebSockets API, Haskell code uses Either, with error in Left
        ("CEvtChatErrors", "")
      ],
      []
    )
  ]

undocumentedEvents :: [ConsName]
undocumentedEvents =
  [ "CEvtAcceptingGroupJoinRequestMember",
    "CEvtAgentConnsDeleted",
    "CEvtAgentRcvQueuesDeleted",
    "CEvtAgentUserDeleted",
    "CEvtBusinessRequestAlreadyAccepted",
    "CEvtCallAnswer",
    "CEvtCallEnded",
    "CEvtCallExtraInfo",
    "CEvtCallInvitation",
    "CEvtCallOffer",
    "CEvtChatInfoUpdated",
    "CEvtChatItemDeletedNotFound",
    "CEvtChatItemNotChanged",
    "CEvtChatSuspended",
    "CEvtConnectionDisabled",
    "CEvtConnectionInactive",
    "CEvtContactAndMemberAssociated",
    "CEvtContactAnotherClient",
    "CEvtContactDisabled",
    "CEvtContactPQEnabled",
    "CEvtContactRatchetSync",
    "CEvtContactRequestAlreadyAccepted",
    "CEvtContactsDisconnected",
    "CEvtContactsMerged",
    "CEvtContactsSubscribed",
    "CEvtContactSubError",
    "CEvtContactSubSummary",
    "CEvtContactSwitch",
    "CEvtCustomChatEvent",
    "CEvtGroupMemberRatchetSync",
    "CEvtGroupMemberSwitch",
    "CEvtHostConnected",
    "CEvtHostDisconnected",
    "CEvtNetworkStatus",
    "CEvtNetworkStatuses",
    "CEvtNewRemoteHost",
    "CEvtNoMemberContactCreating",
    "CEvtNtfMessage",
    "CEvtRcvFileAcceptedSndCancelled", -- only sent with legacy SMP files when they are cancelled
    "CEvtRcvFileProgressXFTP",
    "CEvtRcvStandaloneFileComplete",
    "CEvtRemoteCtrlFound",
    "CEvtRemoteCtrlSessionCode",
    "CEvtRemoteCtrlStopped",
    "CEvtRemoteHostConnected",
    "CEvtRemoteHostSessionCode",
    "CEvtRemoteHostStopped",
    "CEvtSndFileComplete", -- legacy SMP files
    "CEvtSndFileProgressXFTP",
    "CEvtSndFileRcvCancelled", -- legacy SMP files
    "CEvtSndFileRedirectStartXFTP",
    "CEvtSndFileStart", -- legacy SMP files
    "CEvtSndStandaloneFileComplete",
    "CEvtSubscriptionEnd",
    "CEvtTerminalEvent",
    "CEvtTimedAction",
    "CEvtUnknownMemberAnnounced",
    "CEvtUnknownMemberBlocked",
    "CEvtUnknownMemberCreated",
    "CEvtUserAcceptedGroupSent", -- repeat group invitation after it was accepted by the user
    "CEvtUserContactSubSummary"
  ]
