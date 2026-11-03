# Group file stuck waiting for the sender's upload

## Problem

In a regular group, a received file can stay forever on "Waiting for file" ("File will be received when your contact completes uploading it."), while the sender shows it as uploaded.

## Cause

The sender sends the description of a group XFTP file to existing members once, when the upload completes (`SFDONE`, `Subscriber.hs`). It goes only to members with a transfer record and a ready connection (`memberFTs`); transfer records are created only for members whose connection was ready when the file was sent (`saveMemberFD`, `Commands.hs`).

A member whose file message was forwarded depends on the host forwarding the description, and the host stops forwarding when the two members connect (`x.grp.mem.con`). A member whose file message was queued as pending has no transfer record and gets no description.

## Fix

While the user is a current member, on upload completion the sender also sends the description to each member that:
- has a send status for the item other than an error,
- is not among the members with a transfer record and a ready connection,
- has not left, been removed or rejected, and has no ended connection (`connEnded`, extracted from `memberSendAction`).

Delivery:
- forwarded member (`snd_forwarded`): the description text of its forwarding member (`forwardingMemberId`, extracted from `memberSendAction`) if that member gets one, otherwise a spare description; sent now when its connection is ready, otherwise saved as a pending group message;
- other member: a spare description; sent now when its connection is ready and it has no pending messages, otherwise saved as a pending group message.

Pending group messages are sent by `sendPendingGroupMessages` when the connection becomes ready or resumes. Forwarded members sharing a text share one set of messages, separate from the forwarding member's messages. Remaining spares are saved in `extra_xftp_file_descriptions` as before.

## Scope

- Regular and relay groups; direct chats unchanged.
- Members who joined after the send: unchanged.
