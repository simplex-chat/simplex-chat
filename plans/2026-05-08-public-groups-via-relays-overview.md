# Public groups via relays — plan summary

A third kind of group: relay-mediated like channels, but every member can post like a
secret group. Resolves the scale ceiling of full-mesh groups without the broadcast-only
governance of channels. Two orthogonal axes, already in the model:

| `useRelays` | `groupType`  | Name         |
|-------------|--------------|--------------|
| false       | (none)       | Secret group |
| true        | `GTChannel`  | Channel      |
| true        | `GTGroup`    | Public group | ← new
| true        | `GTUnknown`  | refuse       | ← older client sees this for `"group"`

`useRelays` is transport; `groupType` is the governance model (broadcast vs
participatory). The joiner role is a per-group value the owner sets at creation,
carried on the (owner-signed) channel profile, so every relay derives the same role for
the same group — not from a relay-side global config and not from `groupType`. The
blocker is narrow: no path produces `GTGroup` today, and the channel profile carries
no joiner-role field yet.

## Shape of the work

Backend: wire/version bump, type helpers, create command, owner-configured joiner-role
field on the channel profile, relay role derivation from that field. Clients (iOS +
Kotlin mirror): model, audit splitting transport-vs-governance call sites, unified
create flow with a Channel/Public-group toggle that picks the joiner-role default,
views, connect-plan messaging.

## Threat model deltas vs. channels

**Relay can fabricate content as any member** (broader than channels, where it could
only forge as owners). Same deniability property as channels by design; future fix is
opt-in content signing.

Everything else in the channel threat model carries over unchanged. Out of scope for
now: member-to-member DMs in relay-mediated groups — deferred, not killed.

## Sequencing & boundary

Hard prerequisite: the member-profile dissemination plan
(`2026-04-29-member-profile-sending-channels.md`) lands first. Then backend → iOS →
Kotlin; platforms ship independently; older clients refuse to join. Owner→relay
role/rejection-rule communication and owner-signature verification on the channel
profile by relays are not planned here — both apply to channels equally; neither blocks
Public groups.
