# Ephemeral conversations with existing contacts

Ephemeral conversation inside existing conversation with stricter security properties.

- additional level of encryption for message bodies & text, chat items content & text (more?) inside chat db

- ephemeral conversation key not persisted - only stored in-memory of current chat session, cleared on exiting to background

- separate tables for chat items and messages to avoid gaps in ids? TBC db pages on deletion

- don't persist chat items and messages at all? if yes - how to support multiple ephemeral conversations with different contacts in the same session - holding chat items in memory may become expensive. though only "not yet seen" items may have to be held in memory - after opening ephemeral conversation no longer keep them in memory; in this case closing ephemeral conversation screen (not fully exiting but keeping it in list of current ephemeral conversations) and re-opening also does not restore chat items, though connection and key are preserved

- if multiple ephemeral conversations are allowed - how to know they have new messages - should there be notifications for them? only local or push notifications too? not a regular notification but indication in chat list?

- disabled features? e.g. "reply" if messages aren't persisted. voice messages, files, etc.? if files are supported they are deleted exiting, should also be a part of chat start cleanup process (see below)

- contact is required to be verified to start ephemeral conversation - improves guarantee that the key for ephemeral conversation is agreed in a secure context

- no visibility of contact profile in UI

- separated with a blank screen / transition from a main conversation to prevent them appearing in the same screen

- new entity - not a contact?

- new chat type & direction, or additional dimension?

- api to start new and open existing (limited by chat session lifespan) ephemeral conversation

- api to join - also requires verified connection? one party can have contact verified, second not - prohibit until verified?

- join via special chat item? join via same button that is used to start? allow both? chat item and negotiation messages should be automatically deleted on end or on cleanup

- api to end - any side can initiate, both sides client cooperate and delete?

- a new connection is created for the conversation, deleted upon end, incognito mode doesn't affect - no profile is shared at all

- on chat start - deletes ephemeral conversations that were not ended, due to crash or another reason (get synchronously before starting?)

- controller has state of all "active" ephemeral conversations, saved are not loaded - what if one party crashes and not ends, then creates a new ephemeral conversation - previous is ended for another party?

## Design

\***

Track current ephemeral conversations in ChatController.

``` haskell
data ChatController = ChatController {
  ...
  currentECs :: TMap ContactId EphemeralConversation
  ...
}

data EphemeralConversation = EphemeralConversation
  { chatItemId :: Int64,
    ecState :: ECState
  }
  deriving (Show)

data ECState
  = ECInvitationSent { localDhPrivKey :: C.PrivateKeyX25519 }
  | ECInvitationReceived { localDhPubKey :: C.PublicKeyX25519 }
  | ECAcptSent { sharedKey :: Maybe C.Key }
  | ECAcptReceived { sharedKey :: Maybe C.Key }
  | ECNegotiated { sharedKey :: Maybe C.Key }

data ECStateTag
  = ECSTInvitationSent
  | ECSTInvitationReceived
  | ECSTAcptSent
  | ECSTAcptReceived
  | ECSTNegotiated

ecStateTag :: ECState -> ECStateTag
```

\***

Protocol messages:

- `XECInv C.PublicKeyX25519` - invite to ephemeral conversation, other properties except key? ECInvitation type to contain properties?

  - on send: add to Controller's `currentECs` in state `ECInvitationSent` and create `CIECInvitation` chat item.

  - on receive: `processXECInv` - add to Controller's `currentECs` in state `ECInvitationReceived` and create `CIECInvitation` chat item.

- `XECAcpt C.PublicKeyX25519 ConnReqInvitation` - accept ephemeral conversation, send link to join.

  - on send: update Controller's `currentECs` record to state `ECAcptSent`, update chat item.

  - on receive: `processXECAcpt` - update Controller's `currentECs` record to state `ECAcptReceived`, update chat item.

- `XECEnd` - message to end ephemeral conversation. Send in main connection or new one? Main may be better as it may signal cancel as well if ephemeral conversation wasn't yet accepted/negotiated.

Race condition if both parties send `XECInv` simultaneously - if `XECInv` is received when there is ephemeral conversation in `currentECs` in state `ECInvitationSent`, just remove it and signal error `CEECNegotiationError`.

\***

APIs:

- `APIStartEC ContactId` - sends `XECInv`, in UI ephemeral chat view is opened, disabled/progress indicator until ephemeral conversation is negotiated.
- `APIJoinEC ContactId` - sends `XECAcpt`, in UI ephemeral chat view is opened, disabled/progress indicator until ephemeral conversation is negotiated.
- `APIOpenEC ContactId` - loads chat items (?) for current ephemeral conversation, opens ephemeral chat view.
- api to reject? or just allow to delete chat item?
- `APIEndEC ContactId` - sends `XECEnd`, deletes connection, ephemeral conversation entity and chat items, removes from `currentECs` state, deletes `CIECInvitation` chat item, in UI chat is closed.
- terminal counterparts

ChatResponses (mirroring chat item updates for terminal):

- `CRECInvitationSent {contact :: Contact}`
- `CRECInvitationReceived {contact :: Contact}`
- `CRECAccepted {contact :: Contact}`
- `CRECAcceptReceived {contact :: Contact}`
- `CRECEnded {contact :: Contact}`

ChatErrors:

- `CEECNegotiationError {contactId :: ContactId}` - both sent `XECInv`, failed to establish connection, etc.
- `CENoCurrentEC` - on trying to open, accept, end.
- `CEECState {currentECState :: ECStateTag}` - on state errors

\***

Chat item content:

``` haskell
data CIContent (d :: MsgDirection) where
  ...
  CIRcvECInvitation ECStateTag -> CIContent 'MDRcv
  CISndECInvitation ECStateTag -> CIContent 'MDSnd
  ...
```

is there a need for more detailed `CIECStatus` or state tag will suffice? (see CICallStatus)

\***

New chat type, direction, chat info:

``` haskell
data ChatType = ... | CTEphemeral ...

-- new ChatType requires processing cases on ChatRef in all APIs based on it, which may be a good thing
-- e.g. automatically requires separate APISendMessage api

data ChatInfo (c :: ChatType) where
  ...
  EphemeralChat :: ChatInfo 'CTEphemeral -- no additional information required?
  ...

data CIDirection (c :: ChatType) (d :: MsgDirection) where
  ...
  CIEphemeralSnd :: CIDirection 'CTEphemeral 'MDSnd
  CIEphemeralRcv :: CIDirection 'CTEphemeral 'MDRcv
  ...

-- same for `data CIQDirection (c :: ChatType)`

-- same for `data SChatType (c :: ChatType)`
```

Maybe it should be a new dimension and not ChatType, though it may have to be drastically different for groups and easier expressed as a separate `CTEphemeralGroup` ChatType.

Pros of not having it as contact's flag/dimension:

- no special casing in loading chat previews
- no special casing in APIs, instead it's fully fledged ChatType
- separate table for entity - cleaner deletion, no gaps
- can have separate ConnectionEntity, though it may be a con

\***

New ConnectionEntity?

- `RcvDirectEphemeralMsgConnection {entityConnection :: Connection}`

can allow to easily prohibit many protocol messages, e.g. calls, groups, etc.

\***

Database changes:

```sql
CREATE TABLE ephemeral_conversations(
  ephemeral_conversation_id INTEGER PRIMARY KEY,
  contact_id INTEGER REFERENCES contacts(contact_id) ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

-- separate ec_messages and ec_chat_items tables?

-- or foreign_keys to ephemeral_conversations in existing messages and chat_items tables?

-- if files allowed: same question

-- don't save chat items and messages at all? see above. if it's separate ConnectionEntity it's not hard

ALTER TABLE connections ADD COLUMN ephemeral_conversation_id INTEGER DEFAULT NULL
  REFERENCES ephemeral_conversations (ephemeral_conversation_id) ON DELETE CASCADE;

-- add logic on loading entities, e.g. for subscriptions
```

If tables for chat items and messages are separate - logic for saving encrypted message/item content, text, etc. doesn't affect existing queries and code. If it's a new connection entity type it's separate cases in api/processing anyway.

If tables for chat items and messages are reused - To/FromField don't auto convert using To/FromJSON, instead saved/loaded as string, decrypted based on flag?
