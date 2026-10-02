# Group file stuck on "waiting for sender to complete upload"

## Problem

In a regular (non-relay) group, a received file can stay forever on "waiting for sender to complete upload", while the sender shows it as uploaded and later files from the same sender arrive normally.

Observed in a 3-member group (A host, B receiver, C a member who had just joined):

| Time | Receiver B got | Path |
|---|---|---|
| 18:55:49 | `x.grp.mem.new` (C joined) | from host A |
| 18:55:54 | `x.grp.mem.fwd` (introduction to C) | from host A |
| 18:56:00 | `x.msg.new` with file 2 from C | forwarded by host A |
| 18:56:03 | own connection to C ready, `x.grp.mem.con` sent | direct |
| 18:57:11 / 18:57:14 | `x.msg.new` + `x.msg.file.descr` for file 3 | direct from C |
| 18:57:26 / 18:57:34 | `x.msg.new` + `x.msg.file.descr` for file 4 | direct from C |

No `x.msg.file.descr` for file 2 ever arrived, neither directly nor forwarded. In B's database, file 2's description row is empty (part 0, not complete), the file is `accepted` and no agent download exists.

## Cause

The description of a group XFTP file is sent only after the upload completes, separately from the file invitation:

1. When the sender sends the file, it creates per-member transfer records only for members with a ready connection: `xftpSndFileTransfer` in `Commands.hs` ("we are not sending files to pending members"). B was not connected to C yet, so B got no record. The host forwarded the invitation to B, because the host forwards a member's messages to the members it is not yet connected to.
2. When the upload completes (`SFDONE` in `Subscriber.hs`), the sender sends descriptions only to members that have a record and a ready connection (`memberFTs`). The spare descriptions are saved in `extra_xftp_file_descriptions`, which nothing reads.
3. The host forwards the description it received to members not connected to the sender (`testGroupMsgForwardFile` covers this). But when B and C connected (18:56:03), `x.grp.mem.con` made the host mark them connected (`xGrpMemCon` → `setMemberVectorRelationConnected`), so it stopped forwarding C's messages to B. That included the description sent after the upload.

If the two members connect between sending the file and completing its upload, the description takes neither path, and the receiver waits forever.

## Options considered

1. **Receiver: mark and revive (this change).** When the receiver's own connection to the author becomes ready, mark the author's forwarded files that have no description part as unavailable. Receiver-only, no protocol or schema change. Does not deliver the file, but never leaves it hanging.
2. **Sender: send the spare description** directly to members that became connected after sending. Delivers the file, but if the host also forwarded its own description in the race, the receiver can get two descriptions. For multi-part descriptions, `appendRcvFD` can then join parts from both, so this needs a receiver-side guard too.
3. **Host: keep forwarding the description** to members it forwarded the invitation to, even after they connected. Breaks the rule that the host forwards only between unconnected members, needs per-file tracking of forward recipients, and helps only once hosts upgrade.

## Change

1. `getForwardedRcvFilesWithoutDescr` (`Store/Files.hs`) returns the member's received XFTP files in the group that:
   - were not cancelled, with status `new` or `accepted`,
   - have a chat item with `forwarded_by_group_member_id` set, and no message from the author with this shared message ID recorded as received directly (`messages.forwarded_by_group_member_id` cleared, see change 5); this is a NOT EXISTS, so it still marks files whose `messages` rows were deleted by the 30-day message cleanup,
   - have no description part (no row, or part 0).
2. `markFwdFilesUnavailable` (`Library/Internal.hs`) sets these files to `CIFSRcvError (FileErrOther "file was sent before you connected to the sender")` and sends `CEvtChatItemUpdated`. The apps already show this error state ("Error: …" on iOS). `rcv_files` status, `cancelled` and `to_receive` are kept, so a later description can still be processed.
3. It is called in `Subscriber.hs` when the connection with an introduced member (`GCPreMember`/`GCPostMember`) becomes ready, next to `notifyMemberConnected`, in regular groups only (not `useRelays'`). Errors are reported and do not interrupt the connection handling.
4. Revive in `processFDMessage`, when a description arrives after marking:
   - accepted file: the existing `receiveViaCompleteFD` → `startReceivingFile` path sets the item status to receiving;
   - file not accepted (`RFSNew`) and marked unavailable: when the description completes, the item status is reset to an invitation, so the user can receive it. The file status and the marker are checked again (`unmarkFwdFile`) and only the item status is changed.
5. When the author's direct copy of a forwarded `x.msg.new` arrives (`saveGroupRcvMsg`, duplicate forwarded message, from the same author), `messages.forwarded_by_group_member_id` of the stored message is cleared, and the file is unmarked if it was already marked. The author sent to the user directly only if its connection was ready (`ConnSndReady` is enough) when sending, so it has the transfer record and sends the description when the upload completes, possibly much later, unless its connection to the user is inactive at that moment (see Limitations). In an introduction, the new member creates the invitation (`xGrpMemIntro`, received only from its host) and the existing member joins it (`xGrpMemFwd`), so only the existing member becomes `ConnSndReady` before its CON. When the author is the existing member and the user the new one, the author can send directly as soon as it joined, and its messages follow its confirmation, so the direct copy can arrive before the user's CON and before marking; this is why it is recorded on the message rather than only unmarking. When the author is the new member (the observed case), it sends directly only after its own CON. The item status goes back to accepted or invitation, matching `rcv_files` status. `messages.forwarded_by_group_member_id` is otherwise read only by deduplication, where "received directly" is the correct meaning.
6. Accepting a marked file (`acceptFileReceive`: user, bot, app auto-receive, or `startReceiveUserFiles` for files flagged by the iOS notification service) records the acceptance and keeps the error status, so a later description starts receiving and an acceptance after marking does not bring back the endless wait. This is a conditional update in `acceptRcvFT_` (`CASE WHEN ci_file_status = <marker>`), atomic with marking on both SQLite and Postgres; other accept paths never have this status.
7. `CancelFile` of a marked XFTP file resets it to the marker instead of an invitation, so cancelling and accepting again does not bring back the endless wait.
8. Marking, unmarking and the revive change the marker under the file lock, the lock that `ReceiveFile`, `SetFileToReceive` and `CancelFile` hold, so their read-then-write cannot interleave on Postgres either. The lock order is the group lock, then the file lock, as in `processAgentMsgRcvFile`. `startReceiveUserFiles` takes no lock; accepting is protected by the conditional update (change 6), and it only accepts files flagged by the iOS notification service, which uses SQLite.
9. `cancelFilesInProgress` does not treat this error as ended, so deleting or moderating the item still cancels the file, as it did before marking; otherwise a description arriving afterwards would start receiving the file of a deleted item.

## Why the connection moment

- Before the connection, the receiver cannot tell whether a description will follow. Forwarded descriptions are normal: the host forwards its own description to unconnected members, and group history sends a forwarded invitation followed by forwarded description parts. `GrpMsgForward` carries no marker that would tell history from live forwarding.
- After the connection, the author's messages come directly, and a description for a file forwarded before the connection can still arrive in two cases:
  - the host forwarded it before processing `x.grp.mem.con` (its delivery job reads member relations when it runs, so with the host offline this can take long) — the revive covers it;
  - the author's side of the connection was ready before the user's (`ConnSndReady`), so it sent the file directly as well as via the host, and will send the description directly when the upload completes — the direct duplicate prevents or reverts marking (change 5), and the revive covers the description.
- History is sent by the host after `introduceToAll`, on the same connection to the new member. Description parts are sent only for files with a complete description (`invCompleteDescr`); a signed item is replayed with the author's original invitation even without one, and such a file is marked at connection and revived if a description is forwarded later. Usually it is processed before the new member connects with the introduced authors, which takes several round trips through the host, but this is not guaranteed for long history split into many batches. If the connection comes between a history invitation and its description, the revive covers it.

## Limitations

- Marking, unmarking and the revive emit `CEvtChatItemUpdated`, as the existing "file digest" and "redirect not allowed" file errors do. Android and desktop apps show a notification for an updated item of a user that is not active, so such a user can get a notification for the old message.
- The CLI shows a marked or unmarked item as an updated message without the file status (`viewItemUpdate` ignores it, as for other file status updates); API clients get the status.

- The file is not delivered: the user sees an error instead of an endless wait. Delivering it needs option 2 or 3.
- A forwarded invitation that arrives after the receiver's own connection to the author is already ready (forwarded by the host before it learned of the connection, delivered late) is not marked, and keeps waiting as before. This is a smaller window than the one fixed here.
- Files from authors whose connection was ready before this change are not revisited: marking happens only when a connection becomes ready.
- A multi-part description of which only some parts were forwarded before the host stopped forwarding (`part_no > 0`, incomplete) is not marked, and keeps waiting as before. This needs a description longer than one part (very large files) and the connection report to be processed between the parts.
- If the author's connection to the user is inactive (quota) when its upload completes, `memberFTs` skips it and the description is not sent, although the transfer record exists and the direct `x.msg.new` (sent later from the pending queue) unmarks the file. The file then waits, as it did before this change; this is a sender-side loss.
- A file the revive resets to an invitation is not auto-received by Android and desktop apps, which auto-receive only new items; iOS still receives it if the notification service flagged it (`to_receive`).

## Compatibility

No protocol, API or database schema changes. The marker is an existing file status with an error text, so all app versions display it.
