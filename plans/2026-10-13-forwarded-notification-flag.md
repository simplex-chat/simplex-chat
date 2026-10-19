# Notification flag of forwarded messages

Date: 2026-10-13.

A message forwarded by a chat relay or by a forwarding group host is sent with the SMP notification flag when its original event has the flag by the rule of `hasNotification`.

References are to `/code/simplex-chat-4`, branch `master`, commit `2f4a443dd`. Haskell paths without a directory are in `src/Simplex/Chat/`. `SMQ/` is `dist-newstyle/src/simplexmq-f058c38b0eecb6c0/src/Simplex/Messaging/`. `NSE` is `apps/ios/SimpleX NSE/NotificationService.swift`.

## 1. Present behaviour
Path of a forwarded message:
```
received    Library/Subscriber.hs:1014         MSG of a member connection; the flag of the received SMP message is ignored
            Library/Subscriber.hs:1069-1144    processEvent: a NewMessageDeliveryTask is returned for each event to forward
task        Library/Subscriber.hs:1022         created when the user is the relay of a channel, or an admin or owner of a group (:3800), and the author is not blocked by an admin
            Library/Subscriber.hs:1158-1176    createDeliveryTasks; createMsgDeliveryTask (Store/Delivery.hs:78) is called at :1165 and :1172 only
            Store/Delivery.hs:137-170          the task is read with its message: msg_body is decoded to verifiedMsg, the original event
job         Library/Subscriber.hs:4233-4240    DJDeliveryJob: accepted tasks of one scope (Store/Delivery.hs:178) are batched into one job (Messages/Batch.hs:84)
            Library/Subscriber.hs:4246-4251    DJRelayRemoved: one job for one task
            Library/Commands.hs:3256-3265      leaveChannelRelay: one job with the x.grp.leave of the relay
            Store/Delivery.hs:285-328          the job is read as scope, sender ids, body and cursor; its events are stored only as the encoded body
sent        Library/Subscriber.hs:4379-4423    channel: for each recipient, overflow profile batches, then the body; MsgFlags False (:4416)
            Library/Subscriber.hs:4424-4487    channel support scope and regular group: deliver; MsgFlags False (:4483)
```
Types (`Delivery.hs`):
```
DeliveryTaskContext :48
  jobScope :: DeliveryJobScope
  sentAsGroup :: ShowGroupAsSender
DeliveryJobScope :54
  DJSGroup {jobSpec :: DeliveryJobSpec}
  DJSMemberSupport {supportGMId :: GroupMemberId}
DeliveryJobSpec :60
  DJDeliveryJob {includePending :: Bool}
  DJRelayRemoved
NewMessageDeliveryTask :122
  messageId :: MessageId
  taskContext :: DeliveryTaskContext
MessageDeliveryTask :128
  taskId :: Int64
  jobScope :: DeliveryJobScope
  senderGMId :: GroupMemberId
  fwdSender :: FwdSender
  brokerTs :: UTCTime
  verifiedMsg :: VerifiedMsg 'Json
MessageDeliveryJob :161
  jobId :: Int64
  jobScope :: DeliveryJobScope
  senderGMIds :: [GroupMemberId]
  body :: ByteString
  cursorGMId_ :: Maybe GroupMemberId
```
Tables (`Store/SQLite/Migrations/M20250813_delivery_tasks.hs:54, :103`, `M20260529_delivery_job_senders.hs:17`; the same columns in Postgres):
```
both tables
  group_id
  worker_scope
  job_scope_spec_tag
  job_scope_include_pending
  job_scope_support_gm_id
  failed
  created_at
  updated_at
delivery_tasks
  delivery_task_id
  sender_group_member_id
  message_id                 -- messages.chat_msg_event: the tag; messages.msg_body: the original event
  message_from_channel
  task_status
  task_err_reason
delivery_jobs
  delivery_job_id
  sender_group_member_ids
  body
  cursor_group_member_id
  job_status
  job_err_reason
```
The flag, from the forwarder to the notification:
```
forwarder agent        SMQ/Agent.hs:2186-2203, 2563         one flag for each MsgReq, stored with the sent message; a body shared by VRRef is sent with the flag of each request
                       SMQ/Agent.hs:2637-2644               the flag is a parameter of SEND
SMP server             SMQ/Server.hs:2038-2039              a notification is stored for a flagged message to a queue with notifier credentials (:2131-2136), and for such a message only
                       SMQ/Server.hs:361-414                the stored notification is sent as NMSG to the subscriber of the notifier, the notification server
notification server    SMQ/Notifications/Server.hs:545-553  NMSG: a push notification with the last notifications of the device token is sent to the push provider (:693-702)
recipient              Library/Subscriber.hs:890            the notification subscription of a member connection is created unless the chat is muted, MFNone (Library/Commands.hs:1922-1929; SMQ/Agent.hs:3345-3361)
                       SMQ/Agent.hs:2093-2144, 3741         the agent gets the message of the notified queue and passes MSG with the flag; forwarded content is opaque to the agent
                       Library/Subscriber.hs:1014           the core ignores the flag; the item is created from the forwarded event (:4008-4064)
```
iOS notification extension, for a forwarded message:
```
entity key        NSE:489-498      id of the notified connection: "#<groupId> @<groupMemberId>" of the relay or the host (apps/ios/SimpleXChat/ChatTypes.swift:3624-3632, :3258)
item key          NSE:1025-1029    the member of the item, its author, for groupRcv; "#<groupId>" for channelRcv
forwarded item    NSE:173-187      the keys are different: the notification of the item is added to droppedNotifications, outside the entity of the notified connection
shown             NSE:1196-1216    the default of the connection: MFAll: title "#<group> <relay>:", text "message received" (apps/ios/SimpleXChat/Notifications.swift:102); MFMentions: empty content
```
- With the flag alone the push notification is sent, and the message is received by the core of the NSE: the item is created. The shown notification is the default one. The NSE change of section 7 is required for a notification with the message text.

Regular groups, the same message sent to a member by its author and by the host:
```
author    Library/Internal.hs:2847-2865      member with a ready connection: sent to the member; otherwise forwarded by the host (MSAForwarded) or stored as pending
host      Library/Subscriber.hs:4447-4458    job recipients: members whose relation with the sender is MRIntroduced
both      sent while the author is connected to the member and the relation at the host is MRIntroduced
          the second copy is a duplicate: one item; the recipient sends x.grp.mem.con (Library/Internal.hs:2939-2945, 2958-2968), the host stops forwarding (Library/Subscriber.hs:3724-3729)
rule      each copy of x.msg.new is flagged: two notifications for one item in that interval, one otherwise
```

## 2. Rule
```
forwarded message    flag = hasNotification of the tag of its original event (Protocol.hs:1338)
                     the flag of the received SMP message is unused
SMP message          notification = True when the flag of any forwarded message of its body is True, as for sent events (Library/Internal.hs:2248, 2748)
job                  one value for the body, computed when the job is created; the same for every recipient, scope and cursor page
channel pieces       body (id 0) and extBody, profiles with the body (id 1): the job value
                     overflow profile batches (id 2 and above): False; their elements are x.grp.mem.new of the relay (Library/Subscriber.hs:4277)
```
Events (handlers at `Library/Subscriber.hs:1080-1132`; forwarded events are those of `isForwardedGroupMsg`, `Protocol.hs:528`):
```
event                 flag
x.msg.new             True     every content message, with or without a file
x.grp.del             True     forwarded by the relay-removed job (:1118)
x.msg.file.descr      False
x.msg.update          False
x.msg.del             False
x.msg.react           False
x.file.cancel         False
x.info                False
x.grp.relay.new       False
x.grp.mem.new         False
x.grp.mem.role        False
x.grp.mem.restrict    False
x.grp.mem.del         False    also the relay-removed job of a removed relay or host (:3746)
x.grp.leave           False    also the job of a relay that leaves (Library/Commands.hs:3263)
x.grp.info            False
x.grp.prefs           False
x.grp.roster          False
x.file                True     handled without a delivery task (:1096)
x.grp.mem.fwd         True     handled without a delivery task (:1110)
x.grp.inv             True     handled without a delivery task (:1124)
x.call.inv            True     handled without a delivery task (:1132)
x.contact             True     handled without a delivery task (:1132)
```

## 3. Core changes
```
Protocol.hs
  msgHasNotification              new, next to hasNotification (:1338): VerifiedMsg e -> Bool
                                    hasNotification of the tag of the event of the message (verifiedChatMsg, :1583)
Delivery.hs
  MessageDeliveryJob (:161)       notification :: Bool     -- new, after body
Store/Delivery.hs
  createMsgDeliveryJob (:251)     new parameter notification :: Bool, before body; written to delivery_jobs.notification
  MessageDeliveryJobRow (:283)    BoolInt added after the body
  getNextDeliveryJob (:309-328)   notification selected after body; set in MessageDeliveryJob
Library/Subscriber.hs
  processDeliveryTask (:4235)     notification: True when msgHasNotification of verifiedMsg is True for any of acceptedTasks; computed next to senderGMIds, passed at :4237
                      (:4250)     msgHasNotification verifiedMsg of the task is passed
  processDeliveryJob (:4331)      notification is bound from the job
  recipientBodyPieces (:4417)     each piece is returned with its flag: overflow batch False; extBody and body: notification
  addPiece (:4410-4416)           MsgFlags {notification = the flag of the piece}
  deliver (:4483)                 MsgFlags {notification}
Library/Commands.hs
  leaveChannelRelay (:3263)       hasNotification XGrpLeave_ is passed
Library/Internal.hs
  sendFwdMemberMessage (:2903)    MsgFlags {notification = msgHasNotification verifiedMsg}
                                    False for both callers: x.grp.roster (:1307), x.grp.mem.del (Library/Subscriber.hs:3798)
```
Sites with a fixed flag, unchanged:
```
Library/Internal.hs:1428    sendHistory: history for a new member, with the welcome message of a group; MsgFlags False (open question 1)
Library/Internal.hs:2459    sendGroupMemberMessages: events of the user to one member; notification = True
Library/Internal.hs:2910    sendPendingGroupMessages: pending messages of the user to one member; notification = True
tests/ChatTests/Groups.hs:5681, 10907, 13320, 13452, 13608, 13701    elements sent by tests through the agent; MsgFlags False
```
- Every other `MsgFlags` value of the repository is built for events of the user, from `hasNotification` of their tags: `Library/Internal.hs:2248, 2552, 2748`; `Library/Commands.hs:2672, 4131`; `Library/Subscriber.hs:356`.
- Forwarded content is sent as `>` elements only (`encodeFwdElement`, `Messages/Batch.hs:125`; used at `:98`, `Library/Subscriber.hs:4248`, `Library/Internal.hs:1423, 2902`). `XGrpMsgForward` events are only received (`Library/Subscriber.hs:1127`).

## 4. Storage
Migration `M20261012_delivery_job_notification`, SQLite and Postgres, registered in `Store/SQLite/Migrations.hs` (`:180`, `:358`), `Store/Postgres/Migrations.hs` (`:57`, `:112`) and `simplex-chat.cabal` (`:172`, `:351`). It is one `ADD COLUMN`; indexes are unchanged.
```
delivery_jobs
  notification INTEGER NOT NULL DEFAULT 0    -- SMALLINT in Postgres
```
- Down migration: the column is dropped. `delivery_tasks` is unchanged: the event of a task is read with its message.
- Applied migrations must be a prefix of the list sorted by name (`SMQ/Agent/Store/Migrations.hs:23-34`; `Store/SQLite/Migrations.hs:363`). The name is ordered after `20261010_invite_member_via_group` and before the unreleased `20261013_chat_layout` and `20261013_msg_priority` of branch `ep/llm-ux` (open question 3).
- The schema dumps and `chat_query_plans.txt` are regenerated by the tests "verify and overwrite schema dump" (`tests/SchemaDump.hs:58`, `tests/PostgresSchemaDump.hs:29`) and "verify and overwrite query plans" (`tests/SchemaDump.hs:165`); they are never edited by hand.

## 5. Compatibility and cost
```
chat protocol               unchanged; the flag is a parameter of SMP SEND (SMQ/Protocol.hs:598)
recipient                   every version; the flag is used by the SMP server only
earlier relay or host       sends without the flag, as now; a channel member is notified by each relay of this release
job of an earlier release   notification = 0: sent without the flag
downgrade                   the column is dropped
traffic                     count and size of SMP messages unchanged
notifications               one for each flagged SMP message to a queue with notifier credentials: a device with a notification token and the chat set to MFAll or MFMentions
                            one job is one flagged SMP message for a recipient, for all its events
channel                     a post is sent by each relay to each member (Library/Internal.hs:2843-2845; Store/Delivery.hs:352)
                            with N members and R relays up to N × R notifications are sent for a post; one item is created (Library/Internal.hs:2958-2959)
                            shown on iOS: one notification for a post; a duplicate copy is shown with empty content (section 7)
profile overflow batches    unflagged
```

## 6. Tests
`tests/ChatTests/Groups.hs`, helpers next to `relayConnIdToMember` (`:13140`):
```
agentSendBody cc connId body   body sent to the connection by the agent of cc, MsgFlags False; a failure is a test failure
lastSndMsgFlags cc connId n    msg_flags, "T" or "F", of the last n sent rows of type A_MSG ("M") of agent table messages for the connection, in sending order; withCCAgentTransaction (tests/ChatTests/Utils.hs:675)
lastJobNotification cc         notification of the last row of delivery_jobs; withCCTransaction
```
- The agent row of a sent message is kept after delivery until its receipt (`SMQ/Agent/Store/AgentStore.hs:1318-1349`; `SMQ/Agent.hs:2866-2869`): recipients run `/set receipts all off` before the checked messages (as `:1561`).

New tests:
```
group message forwarding (:191)
  testGroupMsgForwardNotification           createGroup3, setupGroupForwarding (:5802); bob sends a message: alice to cath "T", job True; bob edits it: "F", job False
relay delivery, single relay (:283)
  testChannels1RelayDeliverNotification     createChannel1Relay (:9523); alice posts: bob to cath, dan and eve "T", job True; cath adds a reaction: bob to dan and eve "F", job False
deliver member profiles via relay (:295)
  testChannelOverflowProfileNotification    the scenario of testChannelLargeProfileFits (:9956) with an image of 10000 characters and a message of 7000 characters, so that the profile is sent in an overflow batch: bob to dan, the rows added by the message: "F" for the overflow profile batch, "T" for the message
```
- One sender and one message give one job.
Assertions added to present tests, on `delivery_jobs` only (the connections and their agent rows are deleted after the job, `Library/Subscriber.hs:4328`):
```
testGroupMsgForwardGroupDeletion (:6135)    alice: job True, x.grp.del
testChannelDeleteGroupSigned (:11031)       bob: job True, x.grp.del
testGroupMsgForwardAdminRemoval (:6096)     alice: job False, x.grp.mem.del
testChannelRelayLeave (:11220)              bob: job False, x.grp.leave
```
Present tests of delivery jobs, unchanged: "group message forwarding" (`:191-206`), "group scoped messages" (`:255-264`), "relay delivery" (`:282-292`), "deliver member profiles via relay" (`:295-301`), `testRelayBatchAllLarge` and `testRelayBatchSingletonOverflow` (`tests/MessageBatching.hs:164, 186`).

## 7. iOS notification of a forwarded message and of a duplicate copy
Event, `Controller.hs:1048`, next to `CEvtUnknownMemberAnnounced`:
```
CEvtDuplicateGroupMessage       JSON type duplicateGroupMessage
  user :: User
  member :: GroupMember         -- the member whose connection delivered the copy
```
Emission, `Library/Internal.hs`, before the present handling of the duplicate, only when the chat is started with `main=off` (the NSE and the share extension):
```
isMainApp :2976                 subscriptionMode is SMSubscribe; startChatController sets SMOnlyCreate for main=off (Library/Commands.hs:235)
saveGroupRcvMsg :2925           direct copy; member: the member of the connection; every SEDuplicateGroupMessage
                                x.grp.mem.con to the forwarding member when the stored copy was forwarded, as now (:2940-2944)
saveGroupFwdRcvMsg :2959        channel; member: the forwarding relay; Nothing is returned, as now
                   :2960-2969   group; member: the forwarding host; x.grp.mem.con and the error, as now
```
Consumers:
```
terminal view       View.hs:471: no output, as CEvtNtfMessage (:547)
API docs            bots/src/API/Docs/Events.hs:205: in undocumentedEvents, as CEvtUnknownMemberCreated
apps, CLI, bots     not emitted: started with main=on
NSE                 decoded: case duplicateGroupMessage (SimpleX NSE/NSEAPITypes.swift:91)
```
Swift types, `apps/ios/SimpleXChat/ChatTypes.swift`:
```
CIMeta.forwardedByMember :4253     Int64?, after editable, as in core CIMeta (Messages.hs:525); omitted in JSON for Nothing
groupMemberEntityId :3447          (groupId, groupMemberId) -> "#<groupId> @<groupMemberId>"; used by GroupMember.id (:3258) and the NSE
CIDirection.groupMemberId :4224    groupMemberId of the member of groupRcv; nil for other directions
```

NSE, `apps/ios/SimpleX NSE/NotificationService.swift`:
```
receivedMsgNtf (:1035-1039)
  newChatItems              key of a group item: groupMemberEntityId of the group and of the member whose connection delivered the item:
                              forwardedByMember, otherwise chatDir.groupMemberId; the id of the connection entity (ChatTypes.swift:3624-3632)
                            other items, and channelRcv without forwardedByMember: chatInfo.id, as now
  duplicateGroupMessage     key: member.id; notification: .noNtf
processReceivedNtf (:586-588): the best attempt of the entity is read from notificationEntities[id] and replaced when ntf.replaces(bestAttempt)
NSENotificationData.replaces (:77-85), new notification, best attempt:
  .noNtf, .connectionEvent  true: the default of the connection
  .noNtf, other             false
  .callInvitation, any      true
  other, .callInvitation    false
  other, other              true
```
- `.noNtf` is used: `.connectionEvent` is created only as the default (`defaultBestAttemptNtf`, `:1208-1229`). A received item, a contact event and a call are never replaced by `.noNtf`.
- The best attempt is read from `notificationEntities[id]`: `processDroppedNotifications` (`:514-529`) passes the entity copied before its notifications are processed.

Results:
```
forwarded post, MFAll         the text of the post (createMessageReceivedNtf, apps/ios/SimpleXChat/Notifications.swift:66)
forwarded post, MFMentions    the text for a mention or a reply; empty content otherwise
duplicate copy                empty content, unless an item of the same push notification is shown
mention, then other item      the mention is shown; before this patch, .noNtf of the other item replaced it
```
Tests, `tests/ChatTests/Groups.hs`; helpers next to `lastJobNotification`:
```
duplicateMessageEventsCfg    testCfg with an eventHook (Controller.hs:257) that records the member names of CEvtDuplicateGroupMessage
setMainAppOff cc             subscriptionMode of cc set to SMOnlyCreate, as by /_start main=off; called after the group is created
```
Copies are sent through the agent of the sender by `agentSendBody`, as in `testGroupMsgForwardOwnMembershipRejected` (`:5666`), so that the order of copies is fixed:
```
group message forwarding (:191)
  testGroupMsgForwardDuplicateEvent    createGroup3; bob's message is received by cath directly; alice's agent sends a forwarded copy
                                         (Library/Internal.hs:2960-2969): cath "duplicate group message, group id: 1"; no event
                                       setMainAppOff cath; alice's agent sends the copy again: the same line; event member alice
  testGroupMsgDirectDuplicateEvent     alice's agent sends a forwarded copy; setMainAppOff cath; bob's agent sends the direct copy
                                         (Library/Internal.hs:2939-2946): cath "duplicate group message, group id: 1"; event member bob
relay delivery, single relay (:283)
  testChannelDuplicateEvent            createChannel1Relay; alice posts; setMainAppOff cath; bob's agent sends a second forwarded copy
                                         (Library/Internal.hs:2959): event member bob; cath without output
```
- The NSE and the Swift types are not compiled on this machine. Manual checks on iOS: a post in a channel with 2 relays, with MFAll and with MFMentions; a forwarded message in a group; a mention followed by an ordinary message in one push notification.

## Interfaces
```
core functions    msgHasNotification :: VerifiedMsg e -> Bool
                  createMsgDeliveryJob, new parameter notification
                  isMainApp :: CM Bool
core types        MessageDeliveryJob.notification
core events       CEvtDuplicateGroupMessage, JSON type duplicateGroupMessage
storage           delivery_jobs.notification
                  migration M20261012_delivery_job_notification
iOS               CIMeta.forwardedByMember
                  groupMemberEntityId
                  CIDirection.groupMemberId
                  NSEChatEvent.duplicateGroupMessage
                  NSENotificationData.replaces
test helpers      agentSendBody
                  lastSndMsgFlags
                  lastJobNotification
                  duplicateMessageEventsCfg
                  setMainAppOff
```
- `eventHasNotification :: ChatMsgEvent e -> Bool` of `2026-10-13-message-priority.md` is accepted by one change: `msgHasNotification` is computed as `eventHasNotification` of the event of the message. Its three uses, the job column and `hasNotification XGrpLeave_` in `leaveChannelRelay` are unchanged.
- With that change a forwarded `x.msg.update` that raises priority is flagged. The line of the priority plan, section 4, "delivery jobs of relays and forwarding hosts, MsgFlags False for every message" is replaced by this plan.

## Open questions
1. History and the welcome message (`sendHistory`), flagged until commit `93831a0f2` (2026-07-17) and unflagged since: apply the rule? Recommended: unchanged, sent without the flag.
2. The job flag as a column, or computed by the job worker from the decoded body, without a migration? Recommended: the column, computed where the events of the tasks are read.
3. Release order with `ep/llm-ux`. Recommended: this correction first, with date `20261012` in the migration name; otherwise a date after the last released migration.
