# Channel multi-owner (S3-min)

A channel can have several equal owners. Any owner can do what the single owner does today, a member becomes an owner only after it accepts, owners can remove each other, and the channel keeps working after any owner, the creator included, leaves. Owners trust each other as the single owner is trusted today; the design protects them against malicious relays and against a link server that withholds or serves stale data. Concurrent owner actions never lose, overwrite or revert each other.

The implementation plan is `2026-10-15-channel-multi-owner-s3min-implementation.md`. The earlier designs are kept as `…-original.md` (the full design at `596d5f98a`) and `…-s1.md` (the minimal scope at `661adb5f6`).

## What owners can do

| Action | How it works for users |
|---|---|
| Post, moderate, edit the channel, manage relays, share the link | as the creator does today, from any owner's device |
| Make a member an owner | the owner first verifies the member's security code; the member gets an invitation in its "chat with admins" and becomes an owner when it accepts |
| Change member, moderator and admin roles | as today; with several owners each change takes 1–3 s, because it is first recorded on the channel's link server |
| Remove an owner | any owner can remove another; the removed device can no longer change the link at once; it, relays and members apply the removal as they receive it (normally seconds) |
| Leave | any owner can leave while another owner remains; in a channel with several owners the last one cannot leave |
| Destroy the channel | any owner, as today; with several owners it needs the link server reachable |
| See the members | a new owner downloads the member list from a relay in the background, without profile pictures |
| Subscriber count | keeps updating after the creator leaves; it may lag joins by up to a minute |

Single-owner channels keep working as today.

## How it works

The channel's link data lives on one SMP server, the link server, which already orders every write to it. The design makes it order everything the owners share:

- **One new server command, `LSWP`.** An owner reads the link data, makes its one change, and writes it back with the hash of what it read. The server accepts the write only if the data is unchanged, and in the same step replaces the owner keys allowed to write; otherwise the owner reads again and re-applies its change. No write is lost, and owners and keys change together. The old plain write is refused on a link with several owners.
- **Role changes are numbered on the link server.** An owner claims the next roster version with the same compare-and-swap before sending the change, so each version has one author who had applied all earlier ones, as members and relays expect today. They only keep applying a role change whose author left before it arrived.
- **Consent.** The promoter sends an invitation in the member's support chat; the member accepts; then one write adds it as an owner. The security code check stops a relay that substituted the member's key, since such a relay could also sign the acceptance.
- **Background work survives restarts.** What an owner does without a user action (finishing a promotion, publishing relays and the count, repairing the link after a race, re-sending its roster) runs as agent commands that the agent stores and retries; each step works out what is left from the latest link data and the database.
- **Member list and count.** A new owner pulls the member list from a relay page by page (a relay drops messages to an owner's full queue, so pushing would lose data). The first current owner in the link's list publishes the subscriber count, at most once a minute.

## Decisions

| Decision | Choice |
|---|---|
| Role changes need the link server | accepted: 1–3 s per change in a channel with several owners |
| Consent and key-substitution defence | verified security code plus an invitation in the channel; works for any member |
| Member list for a new owner | pulled from one relay, no profile pictures |
| Optional parts | left out: invitation cancel, field-by-field profile merge, equal-version tie-break, retiring the root key |
| Server command shape | sends only the new user data and keys (room for about 52 owners); refuses the old plain write on a link with several owners |
| Relays and owner actions | as today, not checked against the link: owners are trusted, and relays cannot forge their actions |
| A removal at the moment of a promotion | the removal wins everywhere |

## Trade-offs users notice

- **Owner actions need the link server.** Role changes, removal, leaving, promotion and destroying the channel need it reachable; while it is down an owner can still post, and background work completes once it is back. A role change right after another owner's waits a few seconds ("catching up, try again").
- **Edits over a changed profile are refused.** If another owner changed the profile after this owner opened the editor, the edit is refused and the current profile shown.
- **A new owner's device connects to the link server directly** for its owner actions and, as the first owner, for the count, revealing its IP address to that server's operator. The invitation says so and names the server; private-routing settings need an explicit confirmation to accept, and settings that prohibit direct connections block owner writes. Link reads stay private.
- **The link server learns when owners act.**
- **An admin's removal wins over a promotion made at the same moment.** For a day after a device learns that a member became an owner, it accepts an admin's removal of that member.
- **Old apps and servers.** Promotion needs every relay on the new version and the link server on the new SMP version. Subscribers on older apps ignore new owners until they upgrade, so they reject a new owner's signed actions.
- **A new owner's profile reaches every subscriber**; the member is told before it accepts.

## Residual risks

None of these is a race between honest parties:

- **A link server that ignores the compare-and-swap** can make honest owners lose writes or diverge on roles. It cannot forge anything.
- **A removed owner running modified software**, until a relay learns of the removal (the remover's announcement, normally seconds, or the relay's link check every 15 minutes), can still act through that relay as an owner, including sending role changes numbered so high that later ones are dropped there. Afterwards the relay accepts from it only its departure and the role changes it made while still an owner. With a colluding link server it can serve forged link data to readers.
- **Any current owner can destroy the channel or lock the others out.** Owners are trusted as the single owner is today; removal is administration, not a defence against a malicious owner.
- **An admin running modified software** can remove an owner within a day of that owner's promotion.
- **A role change can be abandoned** when nobody received it: its author's device is lost in the second between recording and sending it, its author is removed before any relay got it, or it reached only a relay that then failed. While a lost author is still an owner the others cannot change roles until they remove it; then role changes wait up to a day before the lost version is abandoned. An owner that also missed that author's earlier changes drops them too, although some devices received them.
- **A remover's device that crashes in the instant after the server accepted the removal** sends no announcement; every device applies the removal at its next read of the link, and relays at their next check.
- **Blocking and unblocking a member is not ordered** between owners and moderators, as today.
- **Downgrades.** A downgraded server loses link data written since its last start (owner keys are kept), and multi-owner writes stop until it is upgraded. A creator downgraded to an older app cannot change the link.

## Not verified

App code is not compiled and Postgres tests do not run in this environment (both on the host). The 52-key headroom is computed from the encoders (the first slices test it); catch-up for 100,000 members (about 2,000 messages) is an estimate.

## Size

About 2,150 lines of production Haskell (~480 in simplexmq, ~1,670 in chat) and about 380 per app platform. Five new chat events and one changed; five optional link-data fields; eight columns and no new table; one SMP command and one SMP error.
