# Channel multi-owner (S3-min)

A channel can have several equal owners. Any owner can do what the single owner does today, a member becomes an owner only after it accepts, owners can remove each other, and the channel keeps working after any owner, the creator included, leaves. Concurrent owner actions never lose, overwrite or revert each other.

The implementation plan is `2026-10-15-channel-multi-owner-s3min-implementation.md`. The earlier designs are kept as `…-original.md` (the full design at `596d5f98a`) and `…-s1.md` (the minimal scope at `661adb5f6`).

## What owners can do

| Action | How it works for users |
|---|---|
| Post, moderate, edit the channel, manage relays, share the link | as the creator does today, from any owner's device |
| Make a member an owner | the owner must first verify the member's security code; the member gets an invitation in its "chat with admins" and becomes an owner when it accepts |
| Change member, moderator and admin roles | as today; in a channel with several owners it takes 1–3 s, because each change is first recorded on the channel's link server |
| Remove an owner | any owner can remove another; the removed device loses its owner rights at once and shows that it was removed |
| Leave | any owner can leave while another owner remains; the last owner cannot leave |
| Destroy the channel | any owner, as today |
| See the members | a new owner downloads the member list from a relay in the background, without profile pictures |
| Subscriber count | keeps updating after the creator leaves; it may lag joins by up to a minute |

Single-owner channels keep working as today.

## How it works

The channel's link data lives on one SMP server, the link server, which already orders every write to it. The design makes it order everything the owners share:

- **One new server command, `LSWP`.** An owner reads the link data, makes its one change, and writes it back with the hash of what it read. The server accepts the write only if the data is still what the owner read, and in the same step replaces the list of owner keys allowed to write. If another owner wrote in between, the owner reads again and re-applies its change. So no write is lost, and adding or removing an owner changes the data and the keys together.
- **Role changes are numbered on the link server.** The link data records the latest roster version and who made it. An owner claims the next number with the same compare-and-swap before it sends the change. Every number then has one author who had already applied all earlier changes, which is exactly what members and relays expect today. They need no change.
- **Consent.** The promoter sends an invitation in the member's support chat; the member accepts; then one write adds the member as an owner. The security code check stops a relay that substituted the member's key, because such a relay could also sign the acceptance with the substituted key.
- **Member list and count.** A new owner pulls the member list from one relay page by page, at its own pace (a relay cannot push it: an owner's queue holds 128 messages, and a relay drops messages to a full queue). The first current owner in the link's owner list publishes the subscriber count, at most once a minute.

## Decisions

| Decision | Choice | Alternative not taken |
|---|---|---|
| Role changes need the link server | accepted: 1–3 s per change in a channel with several owners | a sequencer-free roster (≈1,100 more lines, two new tables) |
| Consent and key-substitution defence | verified security code plus an invitation in the channel; works for any member | a request over a direct contact (works only between contacts) |
| Member list for a new owner | pulled from one relay, no profile pictures | relays pushing it (loses data on a full queue) |
| Optional parts | left out: invitation cancel, field-by-field profile merge, equal-version tie-break, retiring the root key | — |
| Server command shape | sends only the new user data and keys; the server keeps the fixed part | resending the fixed part like `LSET`, which limits a channel to about 6 owners |

## Trade-offs users notice

- **Role changes wait for the link server** in channels with several owners, and fail if it is unreachable. A role change right after another owner's waits a few seconds until this device has received that change ("catching up, try again").
- **Edits over a changed profile are refused.** If another owner changed the profile since this owner opened it, the edit is refused and the current profile shown, so nothing is overwritten unseen.
- **The link server learns when owners act.** A promoted owner's device connects to it directly for the owner's own actions and, once it is the first owner, for the subscriber count. The member agrees to this when it accepts the invitation. Link reads stay proxied.
- **Old apps.** Promotion needs every relay on the new version and the link server on the new SMP version. Subscribers on older apps ignore new owners until they upgrade, so they reject a new owner's signed actions.
- **A new owner's profile reaches every subscriber**, because subscribers must be able to verify it. The member is told before it accepts.

## Residual risks

None of these is a race between honest owners:

- **A link server that ignores the compare-and-swap** can make honest owners lose writes or diverge on roles. It cannot forge anything.
- **A removed owner that kept a copy of its key**, or a removed creator that kept the root key, can, with a colluding link server, serve forged link data to readers. Without the link server it can do nothing: its key is removed from the link queue in the same write that removes it.
- **Any current owner can destroy the channel or lock the others out.** Owners are trusted as the single owner is today; removal is administration, not a defence against a malicious owner.
- **A role change can be abandoned** in two failure cases: its author's device is lost in the second between recording the change and sending it, or the change reached only a relay that then failed for a day. Nobody received it, so no device undoes something it showed. While a lost author is still an owner, the others cannot change roles until they remove it.
- **An admin removing a member at the moment an owner promotes it** ends with the member removed if any owner saw the removal first. If every owner saw the promotion first, devices that applied the removal first keep it removed until an owner removes it.
- **A remover's device that crashes in the instant after the server accepted the removal** sends no announcement. Every device applies the removal at its next read of the link data; relays keep an idle connection to the removed owner.
- **Blocking and unblocking a member is not ordered** between owners and moderators, as today.
- **Server downgrade.** An older server ignores the new store-log records and loses key changes made since its last start.

## Not verified

- The iOS and Android/desktop code is not compiled in this environment; the user builds and checks it on the host.
- Postgres server and client tests run only on the host.
- The transmission size headroom (about 50 owner keys in one command) is computed from the encoders; the first slice tests it.
- Member catch-up time for very large channels (about 2,000 messages for 100,000 members) is an estimate.

## Size

About 2,100 lines of production Haskell: ~450 in simplexmq (server command, both queue stores, store log, client, agent) and ~1,650 in chat. About 350 lines per app platform. Five new chat events, one changed; five optional link-data fields; seven columns and no new table; one SMP command and one SMP error.
