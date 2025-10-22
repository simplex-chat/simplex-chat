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
        ("CEvtContactUpdated", "Contact profile of another user is updated."),
        ("CEvtContactDeletedByContact", "Bot user's connection with another contact is deleted (conversation is kept)."),
        ("CEvtReceivedContactRequest", "Contact request received.\n\nThis event is only sent when auto-accept is disabled.\n\nThe request needs to be accepted using [APIAcceptContact](./COMMANDS.md#apiacceptcontact) command"),
        ("CEvtNewMemberContactReceivedInv", "Received invitation to connect directly with a group member.\n\nThis event only needs to be processed to associate contact with group, the connection will proceed automatically."),
        ("CEvtContactSndReady", "Connecting via 1-time invitation or after accepting contact request.\n\nAfter this event bot can send messages to this contact.") -- JOINED
      ]
    ),
    ( "Message events",
      "Bots must use these events to process received messages.",
      [ ("CEvtNewChatItems", "Received message(s).")
      ],
      [ ("CEvtChatItemReaction", "Received message reaction."),
        ("CEvtChatItemsDeleted", "Message was deleted by another user."),
        ("CEvtChatItemUpdated", "Message was updated by another user."),
        ("CEvtGroupChatItemsDeleted", "Group messages are deleted or moderated."),
        ("CEvtChatItemsStatusesUpdated", "Message delivery status updates.")
      ]
    ),
    ( "Group events",
      "Bots may use these events to manage users' groups and business address groups.\n\n\
      \*Please note*: programming groups is more complex than programming direct connections",
      [ ("CEvtReceivedGroupInvitation", ""),
        ("CEvtUserJoinedGroup", "Bot user joined group. Received when connection via group link completes."),
        ("CEvtGroupUpdated", "Group profile or preferences updated."),
        ("CEvtJoinedGroupMember", "Another member joined group."),
        ("CEvtMemberRole", "Member (or bot user's) group role changed."),
        ("CEvtDeletedMember", "Another member is removed from the group."),
        ("CEvtLeftMember", "Another member left the group."),
        ("CEvtDeletedMemberUser", "Bot user was removed from the group."),
        ("CEvtGroupDeleted", "Group was deleted by the owner (not bot user).")
      ],
      [ ("CEvtConnectedToGroupMember", "Connected to another group member."),
        ("CEvtMemberAcceptedByOther", "Another group owner, admin or moderator accepted member to the group after review (\"knocking\")."),
        ("CEvtMemberBlockedForAll", "Another member blocked for all members."),
        ("CEvtGroupMemberUpdated", "Another group member profile updated.")
      ]
    ),
    ( "File events",
      "Bots that send or receive files may process these events to track delivery status and to process completion.\n\n\
      \Bots that need to receive or moderate files (e.g., based on name, size or extension), \
      \can use relevant commands (e.g., [ReceiveFile](./COMMANDS.md#receivefile) or \
      \[APIDeleteMemberChatItem](./COMMANDS.md#apideletememberchatitem)) \
      \when processing [NewChatItems](#newchatitems) event.\n\n\
      \Bots that need to send files should use [APISendMessages](./COMMANDS.md#apisendmessages) command.",
      [ ( "CEvtRcvFileDescrReady",
          "File is ready to be received.\n\n\
          \This event is useful for processing sender file servers and monitoring file reception progress.\n\n\
          \[ReceiveFile](./COMMANDS.md#receivefile) command can be used before this event."
        ),
        ("CEvtRcvFileComplete", "File reception is competed."),
        ("CEvtSndFileCompleteXFTP", "File upload is competed.")
      ],
      [ ("CEvtRcvFileStart", "File reception started. This event will be sent after [CEvtRcvFileDescrReady](#rcvfiledescrready) event."),
        ("CEvtRcvFileSndCancelled", "File was cancelled by the sender. This event may be sent instead of [CEvtRcvFileDescrReady](#rcvfiledescrready) event."),
        ("CEvtRcvFileAccepted", "This event will be sent when file is automatically accepted because of CLI option."),
        ("CEvtRcvFileError", "Error receiving file."),
        ("CEvtRcvFileWarning", "Warning when receiving file. It can happen when CLI settings do not allow to connect to file server(s)."),
        ("CEvtSndFileError", "Error sending file."),
        ("CEvtSndFileWarning", "Warning when sending file.")
      ]
    ),
    ( "Connection progress events",
      "Bots may use these events to track progress of connections for monitoring or debugging.",
      [ ("CEvtAcceptingContactRequest", "Automatically accepting contact request via bot's SimpleX address with auto-accept enabled."),
        ("CEvtAcceptingBusinessRequest", "Automatically accepting contact request via bot's business address."),
        ("CEvtContactConnecting", "Contact confirmed connection.\n\nSent when contact started connecting via bot's 1-time invitation link or when bot connects to another SimpleX address."), -- CONF
        ("CEvtBusinessLinkConnecting", "Contact confirmed connection.\n\nSent when bot connects to another business address."), -- CONF
        ("CEvtJoinedGroupMemberConnecting", "Group member is announced to the group and will be connecting to bot."), -- MSG
        ("CEvtSentGroupInvitation", "Sent when another user joins group via bot's link."), -- INV
        ("CEvtGroupLinkConnecting", "Sent when bot joins group via another user link.") -- CONF
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
    "CEvtContactSwitch",
    "CEvtCustomChatEvent",
    "CEvtGroupMemberRatchetSync",
    "CEvtGroupMemberSwitch",
    "CEvtHostConnected",
    "CEvtHostDisconnected",
    "CEvtSubscriptionStatus",
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
    "CEvtConnectionsDiff",
    "CEvtSubscriptionEnd",
    "CEvtTerminalEvent",
    "CEvtTimedAction",
    "CEvtUnknownMemberAnnounced",
    "CEvtUnknownMemberBlocked",
    "CEvtUnknownMemberCreated",
    "CEvtUserAcceptedGroupSent" -- repeat group invitation after it was accepted by the user
  ]
