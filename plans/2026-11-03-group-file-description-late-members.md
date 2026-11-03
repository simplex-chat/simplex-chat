# Group file stuck waiting for the sender's upload

## Problem

In a regular group, a received file can stay forever on "Waiting for file" ("File will be received when your contact completes uploading it."), while the sender shows it as uploaded.

## Cause

The sender sends the description of a group XFTP file to existing members once, when the upload completes (`SFDONE`, `Subscriber.hs`). It goes only to members with a transfer record and a ready connection (`memberFTs`); transfer records are created only for members whose connection was ready when the file was sent (`saveMemberFD`, `Commands.hs`).

A member whose file message was forwarded by the host, or queued as pending, gets the description only if the host forwards it. The host stops forwarding when the two members connect (`x.grp.mem.con`). If they connect before the upload completes, the description is never delivered.

## Fix

On upload completion the sender also sends the description to each member that:
- has a send status for the item other than an error,
- is not among the members with a transfer record and a ready connection,
- has not left or been removed, and has no ended connection (`connEnded`, extracted from `memberSendAction`).

Delivery:
- forwarded member (`snd_forwarded`): the same messages as its forwarding member (`forwardingMemberId`, extracted from `memberSendAction`), if that member gets a description; otherwise a spare description;
- other member: a spare description;
- sent now when the connection is ready and the member has no pending messages; otherwise saved as a pending group message, sent by `sendPendingGroupMessages` when the connection becomes ready or resumes.

Recipients of the same description text share its messages. Remaining spares are saved in `extra_xftp_file_descriptions` as before.

## Scope

- Regular and relay groups; direct chats unchanged.
- Members who join later get the file through group history only with its complete description (`invCompleteDescr`).
