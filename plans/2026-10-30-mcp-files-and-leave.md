# SimpleX MCP server 0.2.0: files, /leave and library methods

Status: draft for review. Design agreed on 2026-10-22; C1-C3 decided on 2026-10-30; C4 is open. Code references: master `d7d180a42`; library references: `ep/bot-api` (#7808).

Prerequisites, in order:
1. #7808 merged: bot API commands documented, the Node library typed (`packages/simplex-chat-nodejs/src/api.ts`).
2. `simplex-chat` and `@simplex-chat/types` published with those methods; `package.json` of the MCP server pins them.
3. Image processing in the core: merged in #7941.

Phases:
1. Library methods, except the wait at stop, and `/leave CHAT`; `ep/bot-api` at `45c77f6aa` is merged into `ep/mcp-files`.
2. Files: received files, `download`, `send` with `file`.
3. The wait at stop, after C4.

## Decisions

1. Library methods only:
   - the 15 command strings of the server are replaced by typed methods
   - the wait at stop uses item status events, without `/sql`
2. `/leave CHAT` is an owner command in the agent control chat; it acts at once.
3. A received file reaches the agent at once, in the message's `file` field, with status `offered`.
4. `download` is a new action tool; the owner approves each download, or all files of one chat.
5. A download card shows the file's own preview and names the file's servers.
6. `/receive CHAT N` makes the server download the file and forward it into the agent control chat; the card stays pending.
7. Downloads go to `DATA/files/<chat>/`, one folder per chat; the core picks unique names.
8. `send` takes `file`, an absolute path; `text` becomes the caption.
9. Images are sent with a core-made preview and a core-resized copy; other files unchanged.
10. Paths of private keys and of the server's own secrets are refused; the refusal is posted to the agent control chat.
11. C1 (a): the download card names unknown servers before approval, from the item's `fileXftpServers` (`apiGetChatItemInfo`, `Messages.hs:1362`) and the user's XFTP servers (`APIGetUserServers`); both methods are added to #7808.
12. C2 (a): a GIF or WebP, which the core does not decode (`Image.hs:62-70`), is sent as an image with a fixed preview, the file unchanged.
13. C3 (a): a video's preview and duration come from `ffmpeg` and `ffprobe` when installed, otherwise a fixed preview and duration 0.

Open choices:
- C4, the wait at stop: a core command for pending deliveries, or a final message whose sent status is awaited; see "Library methods"

## Library methods

```
place                       now                                     method
serve.ts:90                 /network socks=off smp-proxy=always …    apiGetNetworkConfig, apiSetNetworkConfig
serve.ts:170-188            /sql chat, /sql agent                    removed: wait on item status events
messages.ts:178-180         /_get chat REF PAGINATION                apiGetChat with ChatPagination
messages.ts:182-184         /_read chat items REF IDS                apiChatItemsRead
tools.ts:471                /_connect plan USER TARGET               apiConnectPlan, whole response
tools.ts:539                /_connect USER incognito=… LINK          apiConnect, whole response
tools.ts:527                /_prepare group …                        apiPrepareGroup
tools.ts:535                /_prepare contact …                      apiPrepareContact
tools.ts:520-521, :530      /_connect contact @N, /_connect group #N apiConnectPreparedContact, apiConnectPreparedGroup
tools.ts:533                /_connect contact USER … CONTACT         apiConnectContactViaAddress
tools.ts:702                /_get chats USER pcc=off count=N         apiGetChats
tools.ts:771                /_info @N                                apiContactInfo
```

Phase 1 changes:
```
file          function           change
serve.ts      startAgent         apiGetNetworkConfig, then apiSetNetworkConfig with smpProxyMode always and smpProxyFallback allowProtected
messages.ts   chatType           new: the chat type of a chat reference
              getChat            apiGetChat; pagination a T.ChatPagination or a count
              markRead           apiChatItemsRead
              chatCommand        removed
tools.ts      plan               apiConnectPlan
              classifyPlan       input: the result of apiConnectPlan
              join               apiPrepareGroup
                                 apiPrepareContact
                                 apiConnectPreparedContact
                                 apiConnectPreparedGroup
                                 apiConnectContactViaAddress
                                 apiConnectPreparedLink: APIConnect, the whole response
              connectPrepared    input: the connect call; the prepared chat is deleted when it fails, as now
              prepare            removed
              connectResponse    removed
              domainParam        removed
              performSend        chatType
              agentChats         apiGetChats with {type: "last", count: 1000000}
              incognitoName      apiContactInfo
```

Changed command strings:
```
now                                                                after
/_connect contact @N incognito=off[ json MSG]                      /_connect contact @N[ json MSG]
/_connect group #N incognito=off[ json MSG]                        /_connect group #N[ json MSG]
/_connect contact USER incognito=off N                             /_connect contact USER N
/_connect USER incognito=off LINK                                  /_connect USER LINK
/_get chats USER pcc=off count=N                                   /_get chats USER count=N {"type":"filters","favorite":false,"unread":false}
/network socks=off smp-proxy=always smp-proxy-fallback=protected   /network
                                                                   /_network JSON
```
- `incognito=off` and `pcc=off` are the parser defaults (`Library/Commands.hs:7110`, `:6750`); the query is `clqNoFilters` (`Controller.hs:1169`), the default at `:6752`.
- `/network socks=off …` sets every field of `SimpleNetCfg` except the timeout (`netCfgP`, `Library/Commands.hs:7324-7335`). The library starts the core with the same values (`defaultSimpleNetCfg`, `Controller.hs:1329-1340`; `Mobile.hs:292`; `Chat.hs:274`), except `hostMode`: onionViaSocks in place of public. Without a SOCKS proxy, both select the same hosts (simplexmq `8bc4d3601`, `Client.hs:531-543`, `Agent/Client.hs:1263-1265`).

Wait at stop, C4:
- An edit of a card already sent emits no status event: the SENT of the edit's message reaches `updateGroupItemsStatus` (`Library/Subscriber.hs:4152-4172`) through `insertChatItemMessage_` (`Store/Messages.hs:2818`), and `updateGroupMemSndStatus'` reports no change for a member already at `sent`.
- (a) a core command for the pending deliveries of a group's connections, documented in the bot API; `stop` polls it, at most 10 s, as `deliveryTimeout` now (`serve.ts:16`)
- (b) after the outcome edits, `stop` posts "The server stopped." in the agent control chat and waits for `chatItemsStatusesUpdated` with group status `sent` for every member, at most 10 s; the agent sends a queue's messages oldest first, one at a time (`getPendingQueueMsg`, simplexmq `Agent/Store/AgentStore.hs:1115-1131`), so the edits before it are sent

## /leave CHAT

```
command        who               effect
/leave @N      approving owner   deletes the contact, as the leave tool (tools.ts:646-651)
/leave #N      approving owner   leaves the group and deletes the chat
```

- Pending actions for the chat end as rejected by that owner; their calls return.
- Reply: `Left group X (#3), requested by Alice.` or `Deleted contact X (@5), requested by Alice.`
- The same line goes to the log.
- Both modes.

Phase 1 changes:
```
file         function        change
control.ts   OwnerRequest    {type: "leave", chat}
             ownerRequest    /leave CHAT, read as /revoke CHAT
             ownerCommand    leave: Control.leave
             leave           new
             chatName        showName parameter, safeName by default
             commandsReply   /leave added
tools.ts     leaveChat       performLeave, public, by chat reference; perform calls it
actions.ts   leaveVerbs      input: {chat}
```

`Control.leave(sender, CHAT)`:
```
CHAT not a chat of the agent: reply "No chat CHAT. /list shows the agent's chats."
tools.leaveChat(CHAT)
log the line, names on one line
each pending action of CHAT with a card: reject, decidedBy the sender
reply the line, names as in cards
```
- An error of `leaveChat` is logged to stderr, as the errors of other owner commands; no reply.
- `Commands: /approve, /reject, /revoke, /leave, /pending, /list. Each card shows its commands.`

## Received files

The `file` field of a message (`messageSchema`, `tools.ts:119-139`; `toMessage`, `messages.ts:138-155`):
```
field     value
name      fileName of the item's CIFile
size      bytes
type      image | video | voice | file, from the msgContent type
status    offered | received | rejected | failed
path      full path; with status received
error     with status failed
```

Status from `CIFile.fileStatus`:
```
fileStatus                                   status
rcvInvitation                                offered
rcvAccepted, rcvTransfer                     offered, while a download action performs
rcvComplete                                  received
rcvCancelled, rcvAborted, rcvError           failed
```

- `rejected`: the last download action of the item was rejected or expired; kept in memory until a restart.
- Instructions and the `send`, `receive` and `connect` descriptions: "When a message has a file with status offered, call download when your user's task needs it."

## download

Input:
```
field   value
chat    chat reference
item    item ID of a message with a file
wait    seconds to wait for the download after approval; default 60
```

Output:
```
field    value
action   action number
status   done | failed | rejected | expired
path     with done
error    with failed
```

Card in the agent control chat, mode requests:
- image: an image message with the sender's preview, the card text as caption
- video: a video message with the sender's preview and duration, the card text as caption
- voice and file: a text message
- card text:
  ```
  I want to download a file from Alice in group Builders (#3):
  `report.pdf`, 2.4 MB
  Servers: known.
  /'approve #3 7', /'approve #3 files', /'receive #3 7' or /'reject #3 7'
  ```
- unknown servers: `Servers: xftp.example.com, unknown: downloading shows it this computer's IP address.`

Owner commands:
```
command              effect
/approve CHAT N      downloads the file for the agent
/approve CHAT files  approves this and all later downloads in CHAT; later downloads post no card
/receive CHAT N      the server downloads the file and sends it into the agent control chat; the card stays pending
/reject CHAT N       the call returns status rejected
/revoke CHAT files   downloads in CHAT need a card again; /revoke CHAT keeps covering messages only
```

- The "files" approval is stored in the chat's custom data, next to the standing approval of messages (`approvalOf`, `control.ts:554-557`).
- `/list` shows it per chat.
- Mode host: the host confirms the call (`anthropic/requiresUserInteraction`, `tools.ts:361`); no card.

Perform:
- `apiReceiveFile(fileId, "DATA/files/<chat>/")`, which approves unknown servers (`api.ts:546-547` on `ep/bot-api`)
- `rcvFileComplete` for the file ends it with its path; `rcvFileError` or `rcvFileSndCancelled` fail it

Restart: undecided cards show "Cancelled: the server stopped"; their files stay offered.

## Sending files

Input of `send`:
```
field   value
file    absolute path of a file the server can read
as      file: send an image or video unchanged as a file
text    caption; required without file
```

Message type, by extension, unless `as` is `file`:
```
extension            type    preview                        file sent
.jpg .jpeg .png      image   chatResizeImageUri, 14000      chatResizeImage, 261120 bytes, in DATA/files/sent/
.gif .webp           image   fixed                          unchanged
.mp4 .mov            video   ffmpeg, else fixed             unchanged
other                file    none                           unchanged
```

- A copy in `DATA/files/sent/` is deleted after `sndFileCompleteXFTP` or `sndFileError`.
- The send card adds the file's name, size and full path; an image card is an image message with the preview.

Refused paths, checked after resolving symbolic links:
```
path
~/.ssh/**
~/.gnupg/**
files named id_*, *.pem, *.key, *.p12, *.pfx
the server's data folder, except DATA/files/
```

- The refusal is posted to the agent control chat and returned to the agent as a tool error.

## Tests

- Unit: the `file` field from each `fileStatus`; type by extension; refused paths, symbolic links included; card texts.
- End-to-end, with the local SMP and XFTP servers of `TESTING.md`:
  - a received image: card with the preview, `/approve`, `download` returns the path
  - `/approve CHAT files`: a later download posts no card
  - `/receive`: the file arrives in the control chat; the card stays pending
  - `/reject` and expiry
  - sending a PNG: the peer receives an image of at most 261120 bytes with a preview
  - a refused path: the refusal in the control chat
  - `/leave #N` with a pending send for #N
  - stop with an unsent card edit: the edit is delivered before close

Phase 1 tests:
- `test/unit/fixtures.ts`: `FakeChat` takes the library methods of the server from `api.ChatApi.prototype`; they send through its `sendChatCmd`, so the tests check the command strings. `/_get chats` is read with its query.
- `test/unit/messages.test.ts`: the changed strings; `classifyPlan` with the result of `apiConnectPlan`.
- `test/unit/commands.test.ts`, `/leave`:
  - `/leave #N` with a pending send and a pending leave for #N: the group is left and deleted; both actions are rejected by the sender; a send to another chat stays pending; the reply and the log line
  - `/leave @N`: the contact is deleted
  - names as code in the reply and on one line in the log
  - an unknown chat, the agent control chat, a member who is not an approver, mode host
- `test/e2e/scenarios.test.ts`, `/leave #N` with a pending send for #N: the call returns rejected; the card ends with "Rejected by Alice."; the reply and the log line; the group's owner sees the agent leave; `list` leaves out the chat.
- Development: `simplex-chat` and `@simplex-chat/types` built from this branch (`packages/simplex-chat-nodejs`, `packages/simplex-chat-client/types/typescript`) and installed in the MCP package with `npm install --no-save`.

Phase 1 docs:
- `PLAN.md`: the network configuration at start; the commands with `/leave`.
- `REFERENCE.md`: the commands with `/leave`.
