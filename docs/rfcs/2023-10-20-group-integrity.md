# Group integrity

3 level of DAGs:

Owner
  - group profile and permissions, admin invites and removals
  - in case of gap vote before applying event

Admin
  - member invites and removals
  - prohibit to add and remove admins
  - in case of gap most destructive wins
  - link to owner dag

Messages
  - in case of gap show history according to local graph, correct when owner or admin dag changes
  - link to both admin and owner dags

```haskell
-- protocol
data MsgParent = MsgParent
  { memberId :: MemberId,
    memberName :: String,         -- recipient can use to display message if they don't have member introduced;
                                  -- optional?
    sharedMsgId :: SharedMsgId,
    msgHash :: ByteString,
    msgBody :: String?            -- recipient can use to display message in case parent wasn't yet received;
                                  -- sender can pack as many parents as fits into block
    stored :: Bool                -- whether sender has message stored, and it can be requested
  }

data MsgIds = MsgIds              -- include into chat event
  { sharedMsgId :: SharedMsgId,
    ownerDAGMsgId :: SharedMsgId, -- list of parents?
    adminDAGMsgId :: SharedMsgId,
    parents :: [MsgParent]
  }

-- model
data OwnerDAGEventParent
  = ODEPKnown {eventId :: ?}      -- DB id? sharedMsgId?
  | ODEPUnknown {eventId :: ?}

data OwnerDAGEvent = DAGEvent
  { eventId :: ?,
    parents :: [OwnerDAGEventParent]
  }

data AdminDAGEventParent
  = ADEPKnown {eventId :: ?}
  | ADEPUnknown {eventId :: ?}

data AdminDAGEvent = DAGEvent
  { eventId :: ?,
    ownerDAGEventId :: ?,         -- [OwnerDAGEventParent] - parentIds? ?
    parents :: [AdminDAGEventParent]
  }

data MessagesDAGEventParent
  = MDEPKnown {eventId :: ?}
  | MDEPUnknown {eventId :: ?}

data MessagesDAGEvent = DAGEvent
  { eventId :: ?,
    ownerDAGEventId :: ?,         -- [OwnerDAGEventParent] - parentIds? ?
    adminDAGEventId :: ?,         -- [AdminDAGEventParent] - parentIds? ?
    parents :: [MessagesDAGEventParent]
  }
```

How to restore from destructive messages?
Even if all message parents are known, destructive logic of message should be applied after other members refer it.

How to workaround members maliciously referring non-existent parents?
For example, this can lead to an owner preventing group updates.

```
-- should dag be maintained in memory? older events to be removed
-- read on event?
-- how long into past to get dag?

ClassifiedEvent = OwnerEvent | AdminEvent | MsgEvent

def processEvent(e: Event) =
  classifiedEvent <- classifyEvent(e)
  case classifiedEvent of
    OwnerEvent oe -> processOwnerEvent(oe)
    AdminEvent ae -> processAdminEvent(ae)
    MsgEvent me -> processMsgEvent(me)

def classifyEvent(e: Event) -> ClassifiedEvent? =
  case e of
    XMsgNew         -> MsgEvent
    XMsgFileDescr   -> Nothing       -- different per member
    XMsgFileCancel  -> MsgEvent
    XMsgUpdate      -> MsgEvent
    XMsgDel         -> MsgEvent
    XMsgReact       -> MsgEvent
    XFile           -> MsgEvent
    XFileCancel     -> MsgEvent
    XFileAcptInv    -> Nothing       -- different per member
    XGrpMemNew      -> OwnerEvent    -- sent by owner, new member is admin or owner
                       or AdminEvent -- sent by admin (or by owner and new member role is less than admin?)
                       -- problem: if member role changes, members can add event to different dags
                       -- what should define member role?
    XGrpMemIntro    -> Nothing       -- received only by invitee
    XGrpMemInv      -> Nothing       -- received only by host
    XGrpMemFwd      -> Nothing       -- different per member; not received by invitee
    XGrpMemRole     -> OwnerEvent    -- sent by owner about owner or admin
                       or AdminEvent -- sent by admin (or by owner about member with role less than admin?)
    XGrpMemDel      -> OwnerEvent    -- sent by owner about owner or admin
                       or AdminEvent -- sent by admin (or by owner about member with role less than admin?)
    XGrpLeave       -> MsgEvent
    XGrpDel         -> OwnerEvent
    XGrpInfo        -> OwnerEvent
    XGrpDirectInv   -> Nothing       -- received by single member
    XInfoProbe      -> Nothing       -- per member
    XInfoProbeCheck -> Nothing       -- per member
    XInfoProbeOk    -> Nothing       -- per member
    BFileChunk      -> Nothing       -- could be MsgEvent?
    _               -> Nothing       -- not supported in groups

-- # owner events

def processOwnerEvent(oe: OwnerEvent) =
  process every owner event after owners reach consensus

// def processOwnerEvent(oe: OwnerEvent) =
//   addOwnerDagEvent(oe)
//   applyOwnerDagEvent(oe)
// 
// def addOwnerDagEvent(oe: OwnerEvent) =
//   if (any parent of oe not in dag):
//     buffer until all parents are in ownerDag
//   else
//     add oe to ownerDag
// 
// def applyOwnerDagEvent(oe: OwnerEvent) =
//   case oe of
//     -- process XGrpMemNew, XGrpMemRole, XGrpMemDel same as for admin dag (see below), or should vote for all events?
//     XGrpMemNew -> ...
//     XGrpMemRole -> ...
//     XGrpMemDel -> ...
//     -- how to vote - to depend on action (group - manual, update - automatic?);
//     -- wait for voting always, or if event has unknown parents? (gaps in dag)
//     -- how to treat delayed integrity violation - owner sending message to select members
//     XGrpDel ->
//       -- create "pending group deletion", wait for confirmation from majority of owners?
//       -- new protocol requiring user action from other owners?
//     XGrpInfo ->
//       -- create "unconfirmed group profile update", remember prev group profile
//       -- remove from "unconfirmed group profile update" when this event is in dag and not a leaf?
//       -- if another group profile update event is received, revert "unconfirmed" event, don't apply new
//       -- so if more than one update is received while dag is not merged to single vertice, all updates are not applied
//       --   - this would likely lock out owners from any future updates
//       --   - merge to new starting point after some time passes?
//       --   - mark parents that are never received and so always block graph merging as special type?

-- # admin events

def processAdminEvent(ae: AdminEvent) =
  lookup in owner dag - does member still have permission?
  addAdminDagEvent(ae)
  applyAdminDagEvent(ae)

def addAdminDagEvent(ae: AdminEvent) =
  if (any parent of ae not in dag):
    buffer until all parents are in adminDag
  else
    add ae to adminDag

def applyAdminDagEvent(ae: AdminEvent) =
  case ae of
    XGrpMemNew ->
      -- handles case where messages from 2 admins about member addition and deletion arrive out of order
      if member is not in "unconfirmed member deletions":
        add member
    XGrpMemRole ->
      add role change to "unconfirmed role change"
      -- remove from "unconfirmed role change" when this event is in dag and not a leaf?
      if another role change already in "unconfirmed role change":
        if new role is less than role in "unconfirmed role change":
          change role -- role change applies in direction of lower role
    XGrpMemDel ->
      add member to "unconfirmed member deletions"
      -- remove from "unconfirmed member deletions" when this event is in dag and not a leaf?
      if member found by memberId:
        delete member

-- ^ problem: if later admin event turns out to fail integrity check, how to revert it?
-- member deletion: don't apply until in graph and not a leaf
-- role change: remember previous role and revert
-- member addition: delete member

-- # message events

def processMsgEvent(me: MsgEvent) =
  lookup points in owner and admin dag?
  - does member have permission to send event? (role changed/removed)
  addMsgDagEvent(me)
  applyMsgEvent(me)

def addMsgDagEvent(me: MsgEvent) =
  for me.parents not in msgDag:
    add MDEPUnknown parent to msgDag
  add me to msgDag

def applyMsgEvent(me: MsgEvent) =
  case me of
    XMsgNew         -> message to view
                       -- start process waiting for missing parents; if parents are not received:
                       --   can be shown as integrity violation if parents are not received
                       -- can be shown as integrity violation if other members don't refer it?
    XMsgFileCancel  -> cancel file immediately
                       -- wait for missing parents / referrals similarly to XMsgNew
                       -- restart file reception on integrity violation?
    XMsgUpdate      -> update to view -- same as XMsgNew
    XMsgDel         -> mark deleted, don't apply full delete until parents/referrals are received?
    XMsgReact       -> to view -- same as XMsgNew
    XFile           -> -- deprecate?
    XFileCancel     -> cancel -- same as XMsgFileCancel
    XGrpLeave       -> mark member as left, don't delete member connection immediately
                       -- member may try to maliciously remove connections selectively
                       -- wait for integrity check
```

# Admin blockchain

Suppose admin DAG is replaced with blockchain, with a conflict resolution protocol to provide consistency of membership changes. Take Simplex (not to confuse with SimpleX chat) protocol (https://simplex.blog/). To reach BFT consensus and make progress, 2n/3 votes on block proposals are required, and it's assumed `f < n/3` where f is number of malicious actors. In a highly asynchronous setting of decentralized groups operated by mobile devices, progress seems unlikely or very slow. Should "admin participation" be hosted?
