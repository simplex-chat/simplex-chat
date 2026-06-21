# Acknowledgments for async command responses

## Problem

Continuations for asynchronous commands can be forever lost if their execution fails, e.g. due to a crash. This can result in failures in establishing connections, sending post-connection auto-reply, etc. depending on other applications of asynchronous commands.

## Solution

An idea is to persist events in agent until chat acknowledges their processing, and replay them on next start of command processing.

### Agent persistence

Save response on command before notifying chat (event received by chat via subQ).

```sql
ALTER TABLE commands ADD COLUMN event BLOB;
```

Type is `AEvent`, requires encoding for To and FromField instances.

Application of chat continuations is very limited, so not all events need to be saved. In fact currently we only need 2 types of events to be recorded - see below. This breaks separation between chat and agent (agent knows which events to record), however that abstraction has long been violated. This can be a contract between agent and chat - which events to keep and acknowledge.

TBC separate type for storing only necessary events:

```haskell
data AEventDB where
  ... -- only necessary constructors

-- AEventDB encoding, instances

toDBEvent :: AEvent -> AEventDB

fromDBEvent :: AEventBD -> AEvent
```

Alternatively, we can save all events and require chat to acknowledge all events. This seems like an overkill and unnecessary work and generalization.

### Agent event processing

Currently agent deletes command records after processing. Instead it will:
- keep records until receiving acknowledgement on event;
- delete command record when receiving acknowledgment on event from chat;
- when retrieving next command to process filter out already processed commands (that have event saved);
- replay to chat unacknowledged events on starting async command processing (`resumeAllCommands`?).

Same correlation id that is used for command can be used for acknowledging event.

Agent API:

```haskell
ackCommandEvent :: AgentClient -> UserId -> ACorrId -> AE ()
```

### Command continuations

Chat uses command continuations on following events:
- INV in group connection - XGrpMemIntro continuation (send XGrpMemInv with created connection link);
- JOINED in both direct and group (business chat) connections - send auto-reply.

So it is enough for agent to record only INV and JOINED events, and for chat to acknowledge processing only for these events. However, as agent doesn't discriminate which INVs to save, chat should acknowledge all INVs. Another alternative is for chat to inform agent whether event should be kept when making command, e.g.:

```haskell
createConnectionAsync :: AgentClient -> ... -> Bool -> ...

-- Bool is flag whether to keep INV event for this command until acknowledged
```

Group relay protocol may add new continuations, for example for owner on adding relay link to group link (new async version of setConnShortLink api).

Chat continuations should be idempotent.
- More important for INV event, to not repeatedly send XGrpMemIntro.
- For JOINED in worst case auto-reply would be re-sent which is not ideal but not very damaging.
- Chat can track additional state to help identify which part of event processing to replay.
- E.g. for group INV continuation chat can track that XGrpMemIntro was sent on group record. TBC per continuation.
