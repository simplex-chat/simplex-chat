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
   - have a chat item with `forwarded_by_group_member_id` set,
   - have no description part (no row, or part 0).
2. `markFwdFilesUnavailable` (`Library/Internal.hs`) sets these files to `CIFSRcvError (FileErrOther "file was sent before you connected to the sender")` and sends `CEvtChatItemUpdated`. The apps already show this error state ("Error: …" on iOS). Only the item's file status changes: `rcv_files` status and `cancelled` are kept.
3. It is called in `Subscriber.hs` when the connection with an introduced member (`GCPreMember`/`GCPostMember`) becomes ready, next to `notifyMemberConnected`, in regular groups only (not `useRelays'`). Errors are reported and do not interrupt the connection handling.
4. Revive in `processFDMessage`, for when the host forwarded the description concurrently with the connection report:
   - accepted file: the existing `receiveViaCompleteFD` → `startReceivingFile` path sets the item status to receiving;
   - file not accepted (`RFSNew`) and marked unavailable: when the description completes, the status is reset to an invitation (`resetRcvCIFileStatus`), so the user can receive it.

## Why the connection moment

- Before the connection, the receiver cannot tell whether a description will follow. Forwarded descriptions are normal: the host forwards its own description to unconnected members, and group history sends a forwarded invitation followed by forwarded description parts. `GrpMsgForward` carries no marker that would tell history from live forwarding.
- After the connection, the author's messages come directly. A description for a file forwarded before the connection can still arrive only in the short race where the host forwarded it before processing `x.grp.mem.con`. The revive covers that.
- History is sent by the host after `introduceToAll`, on the same connection to the new member. The new member therefore processes history, including description parts, before it can connect with the introduced authors, which takes several round trips through the host.

## Limitations

- The file is not delivered: the user sees an error instead of an endless wait. Delivering it needs option 2 or 3.
- A forwarded invitation that arrives after the receiver's own connection to the author is already ready (forwarded by the host before it learned of the connection, delivered late) is not marked, and keeps waiting as before. This is a smaller window than the one fixed here.
- Files from authors whose connection was ready before this change are not revisited: marking happens only when a connection becomes ready.

## Compatibility

No protocol, API or database schema changes. The marker is an existing file status with an error text, so all app versions display it.
