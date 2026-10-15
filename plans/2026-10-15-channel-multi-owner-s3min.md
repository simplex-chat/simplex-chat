# Channel multi-owner (S3-min)

A channel can have several equal owners. Any owner can do what the single owner does today, a member becomes an owner only after it accepts, owners can remove each other, and the channel keeps working after any owner, the creator included, leaves. Concurrent owner actions never lose, overwrite or revert each other.

The implementation plan is `2026-10-15-channel-multi-owner-s3min-implementation.md`. The earlier designs are kept as `…-original.md` (the full design at `596d5f98a`) and `…-s1.md` (the minimal scope at `661adb5f6`).

## What owners can do

| Action | How it works for users |
|---|---|
| Post, moderate, edit the channel, manage relays, share the link | as the creator does today, from any owner's device |
| Make a member an owner | the owner first verifies the member's security code; the member gets an invitation in its "chat with admins" and becomes an owner when it accepts |
| Change member, moderator and admin roles | as today; with several owners each change takes 1–3 s, because it is first recorded on the channel's link server |
| Remove an owner | any owner can remove another; the removed device can no longer change the link at once, and relays stop accepting its owner actions as soon as they learn of the removal (normally seconds) |
| Leave | any owner can leave while another owner remains; in a channel with several owners the last one cannot leave |
| Destroy the channel | any owner, as today |
| See the members | a new owner downloads the member list from a relay in the background, without profile pictures |
| Subscriber count | keeps updating after the creator leaves; it may lag joins by up to a minute |

Single-owner channels keep working as today.

## How it works

The channel's link data lives on one SMP server, the link server, which already orders every write to it. The design makes it order everything the owners share:

- **One new server command, `LSWP`.** An owner reads the link data, makes its one change, and writes it back with the hash of what it read. The server accepts the write only if the data is still what the owner read, and in the same step replaces the list of owner keys allowed to write. If another owner wrote in between, the owner reads again and re-applies its change. No write is lost, and adding or removing an owner changes the data and the keys together. The server also refuses the old plain write on a link with several owners, so an old or delayed single-owner write cannot erase them.
- **Role changes are numbered on the link server.** The link data records the latest roster version and who made it. An owner claims the next number with the same compare-and-swap before it sends the change. Each number then has one author who had already applied all earlier changes, which is what members and relays expect today; they need no change.
- **Consent.** The promoter sends an invitation in the member's support chat; the member accepts; then one write adds it as an owner. The security code check stops a relay that substituted the member's key, because such a relay could also sign the acceptance with the substituted key.
- **Owner changes are checked against the link.** Relays accept a new owner, an owner's removal or the channel's deletion only after reading the link data, so a device that was removed cannot use them.
- **Member list and count.** A new owner pulls the member list from a relay page by page, at its own pace (an owner's queue holds 128 messages and a relay drops messages to a full queue, so pushing would lose data). The first current owner in the link's owner list publishes the subscriber count, at most once a minute.

## Decisions

| Decision | Choice | Alternative not taken |
|---|---|---|
| Role changes need the link server | accepted: 1–3 s per change in a channel with several owners | a sequencer-free roster (≈1,100 more lines, two new tables) |
| Consent and key-substitution defence | verified security code plus an invitation in the channel; works for any member | a request over a direct contact (only between contacts) |
| Member list for a new owner | pulled from one relay, no profile pictures | relays pushing it (loses data on a full queue) |
| Optional parts | left out: invitation cancel, field-by-field profile merge, equal-version tie-break, retiring the root key | — |
| Server command shape | sends only the new user data and keys; the server keeps the fixed part | resending the fixed part, which limits a channel to about 6 owners |
| Old single-owner writes | refused by the server on a link with several owners | waiting for them, which a crash or a downgraded app escapes |

## Trade-offs users notice

- **Owner actions need the link server.** Role changes, removal, leaving and promotion need it reachable; while it is down an owner can still post but not leave (deleting the channel remains). A role change right after another owner's waits a few seconds ("catching up, try again").
- **Edits over a changed profile are refused.** If another owner changed the profile after this owner opened the editor, the edit is refused and the current profile shown.
- **A new owner's device connects to the link server directly** for its owner actions and, when it is the first owner, for the count. This reveals its IP address to that server's operator. The invitation says so and names the server; if the user's settings route such traffic privately, accepting needs an explicit confirmation, and settings that prohibit direct connections block owner writes. Link reads stay private.
- **The link server learns when owners act.**
- **Old apps and servers.** Promotion needs every relay on the new version and the link server on the new SMP version. Subscribers on older apps ignore new owners until they upgrade, so they reject a new owner's signed actions.
- **A new owner's profile reaches every subscriber**; the member is told before it accepts.

## Residual risks

None of these is a race between honest owners:

- **A link server that ignores the compare-and-swap** can make honest owners lose writes or diverge on roles. It cannot forge anything.
- **Until a relay learns of a removal** (the remover's announcement, normally seconds, or the relay's link check every 15 minutes), the removed device can still post and send role changes through it. It cannot add or remove owners or delete the channel there.
- **A removed owner that kept a copy of its key**, or a removed creator that kept the root key, can serve forged link data to readers only with a colluding link server.
- **Any current owner can destroy the channel or lock the others out.** Owners are trusted as the single owner is today; removal is administration, not a defence against a malicious owner.
- **A role change can be abandoned** in two failure cases: its author's device is lost in the second between recording and sending it, or it reached only a relay that then failed. Nobody received it. While a lost author is still an owner the others cannot change roles until they remove it; after that, role changes wait up to a day before the lost version is abandoned.
- **An admin removing a member at the moment an owner promotes it** ends with the member removed, once the member's device or an owner sees both. If the member's device is gone and every owner saw the promotion first, the member stays listed as an owner until an owner removes it, like any lost owner device.
- **A remover's device that crashes in the instant after the server accepted the removal** sends no announcement; every device applies the removal at its next read of the link, and relays at their next check.
- **Blocking and unblocking a member is not ordered** between owners and moderators, as today.
- **Downgrades.** A downgraded server loses link data written since its last start (owner keys are kept), and multi-owner writes stop until it is upgraded. A creator downgraded to an older app cannot change the link.

## Not verified

- The iOS and Android/desktop code is not compiled in this environment; the user builds and checks it on the host.
- Postgres server and client tests run only on the host.
- The command's size headroom (52 owner keys) is computed from the encoders; the first slices test it.
- Member catch-up time for very large channels (about 2,000 messages for 100,000 members) is an estimate.

## Size

About 2,300 lines of production Haskell: ~470 in simplexmq (server command and refusal, three queue stores, store log, client, agent) and ~1,850 in chat. About 400 lines per app platform. Five new chat events and one changed; five optional link-data fields; eight columns and no new table; one SMP command and one SMP error.
