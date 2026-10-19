# Channel multi-owner (S3-min)

A channel can have several equal owners. Any owner can do what the single owner does today, a member becomes an owner only after it accepts, owners can remove each other, and the channel keeps working after any owner, the creator included, leaves. Owners trust each other as the single owner is trusted today; the design protects them against malicious relays and against a link server that withholds or serves stale data. Concurrent owner actions never lose, overwrite or revert each other.

The implementation plan is `2026-10-15-channel-multi-owner-s3min-implementation.md`. The earlier designs are kept as `…-original.md` (the full design at `596d5f98a`) and `…-s1.md` (the minimal scope at `661adb5f6`).

## What owners can do

| Action | How it works for users |
|---|---|
| Post, moderate, edit the channel, manage relays, share the link | as the creator does today, from any owner's device |
| Make a member an owner | after verifying its security code; the member gets an invitation in its "chat with admins" and becomes an owner when it accepts |
| Change member, moderator and admin roles | as today; with several owners each change takes 1–3 s, first recorded on the link server |
| Remove an owner | any owner; the removed device can no longer change the link at once, and it, relays and members apply the removal on receipt |
| Leave | while another owner remains; with several owners the last one cannot leave |
| Destroy the channel | any owner; with several owners it needs the link server reachable |
| See the members | a new owner downloads the member list from a relay, without profile pictures |
| Subscriber count | keeps updating after the creator leaves, lagging joins by up to a minute |

Single-owner channels, and channels whose owner invitations nobody accepted, keep working as today.

## How it works

The channel's link data lives on one SMP server, the link server, which already orders every write to it. The design makes it order everything the owners share:

- **One new server command, `LSWP`.** An owner reads the link data, makes its one change, and writes it back with the hash of what it read; the server accepts it only if the data is unchanged, replacing the owner keys in the same step. Otherwise the owner reads again and re-applies its change.
- **Role changes are numbered on the link server**, claimed with the same command from the complete current roster, so each version has one author who had all earlier ones, as members and relays expect today.
- **Consent.** The promoter invites the member in its support chat; the member accepts; one write adds it. The verified security code stops a relay that substituted the member's key.
- **Background work** (finishing a promotion, publishing relays and the count, repairing the link after a race, re-sending a roster) runs as agent commands the agent stores and retries; each step works out what is left from the latest link data.
- **Member list and count.** A new owner pulls the member list from a relay page by page. The first current owner in the link's list publishes the count, at most once a minute.

## Decisions

| Decision | Choice |
|---|---|
| Role changes need the link server | accepted: 1–3 s per change in a channel with several owners |
| Consent and key-substitution defence | verified security code plus an invitation in the channel |
| Member list for a new owner | pulled from one relay, no profile pictures |
| Optional parts | left out: invitation cancel, field-by-field profile merge, equal-version tie-break, retiring the root key |
| Server command | sends only the new user data and keys (room for about 50 owners) |
| Relays and owner actions | as today, not checked against the link: owners are trusted, and relays cannot forge their actions |
| A removal at the moment of a promotion | the promotion wins: devices that applied the removal follow the link |
| Blocking and unblocking | not ordered, as in groups today |

## Trade-offs users notice

- **Owner actions need the link server** (role changes, removal, leaving, promotion, destroying); while it is down owners can still post, and background work completes once it is back. A role change right after another owner's can be refused for a few seconds ("catching up, try again").
- **Edits over a changed profile are refused**, and the current profile is shown.
- **A new owner's device connects to the link server directly**, revealing its IP address to that server's operator; the invitation says so, and private-routing settings need an explicit confirmation. Link reads stay private.
- **The link server learns when owners act and how many there are.**
- **A promotion wins over a removal at the same moment**: devices that applied the removal first take the member as an owner at their next link read. If the member's own device applied it first, it has left, and the member stays listed as an owner without a device until another owner removes it.
- **Old apps and servers.** Promotion needs every relay and the link server upgraded; subscribers on older apps reject a new owner's actions until they upgrade.
- **A new owner's profile reaches every subscriber**; the member is told before it accepts.

## Residual risks

None of these is a race between honest parties:

- **A link server that ignores the compare-and-swap** can make honest owners lose writes or diverge on roles. It cannot forge anything.
- **A removed owner running modified software**, until a relay learns of the removal (the remover's announcement, normally seconds, or the relay's 15-minute link check), can act through that relay as an owner, including role changes numbered so high that later ones are dropped there and at its members; afterwards the relay accepts from it only its departure and role changes up to the link's roster version. With a colluding link server it can serve forged link data.
- **Any current owner can destroy the channel or lock the others out**: removal is administration, not a defence.
- **A role change can be abandoned** when no relay serves it: its author's device is lost in the second after recording it, its author is removed before any relay got it, or only a relay that then failed got it. While a lost author is an owner the others cannot change roles until they remove it; then they wait up to a day, and devices that applied part of the lost version return to the abandoning owner's roster. An owner that also missed that author's earlier changes drops them too.
- **A remover crashing right after its removal landed** sends no announcement; devices apply it at their next link read, relays at their next check.
- **Blocking and unblocking a member is not ordered**, as today.
- **A creator restored from a backup older than its first promotion** acts as the single owner until it leaves or destroys the channel, and its role changes can collide with another owner's.
- **Downgrades.** A downgraded server loses link data written since its last start (owner keys are kept), and multi-owner writes stop until it is upgraded. A creator downgraded to an older app cannot change the link.

## Not verified, size

App code and Postgres tests run only on the host. The 50-owner headroom is computed from the encoders (the first slices test it); catch-up for 100,000 members (about 2,000 messages) is an estimate. Five new chat events and one changed; five optional link-data fields; eight columns, no new table; one SMP command and one error.
