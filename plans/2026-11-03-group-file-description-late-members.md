# Group file stuck on "waiting for sender to complete upload"

## Problem

In a regular group, a received file can stay forever on "waiting for sender to complete upload", while the sender shows it as uploaded.

## Cause

The description of a group XFTP file is sent once, when the upload completes (`SFDONE`, `Subscriber.hs`):

- it goes only to members with a transfer record and a ready connection (`memberFTs`);
- transfer records are created only for members whose connection was ready when the file was sent (`saveMemberFD`, `Commands.hs`).

A member whose invitation was forwarded by the host, or queued as pending, gets the description only if the host forwards it. The host stops forwarding when the two members connect (`x.grp.mem.con`). If they connect before the upload completes, the description is never delivered.

## Fix

On upload completion the sender also sends the description to every current member with a send status for the item and without a transfer record and ready connection:

- member with a ready connection: sent now;
- other member: saved as a pending group message, sent by `sendPendingGroupMessages` when its connection becomes ready.

A member whose status is `snd_forwarded` gets the description text sent to its forwarding member (`forwardingMemberId`, extracted from `memberSendAction`), so parts forwarded by the host and parts sent by the author are identical. Other members get a spare description; the rest is saved in `extra_xftp_file_descriptions` as before.

## Scope

- Regular and relay groups; direct chats unchanged.
- Not covered: a member who joined after the file was sent and got it through group history.
