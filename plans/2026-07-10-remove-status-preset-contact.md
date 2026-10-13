# Remove the SimpleX Status preset contact

Branch: `nd/remove-status-preset-contact` · PR #7231 (supersedes #7200)

## 1. Problem statement

Every new profile gets a "SimpleX Status" preset contact card. It is too confusing for non-technical users: it looks like a person or a chat, but it is an automated broadcast bot, and tapping it opens a connect dialog rather than content. On top of that, its stored contact link is the old `simplex:/contact/#/...` invitation address; the intended replacement is a channel, which a preset *contact* card cannot deliver (see §3).

## 2. Solution summary

Delete the preset entirely:

- `simplexStatusContactProfile` removed from `Library/Internal.hs` (14 lines incl. inline logo).
- Its `createContact` call removed from `createPresetContactCards` in `Library/Commands.hs` (one line; the "Ask SimpleX Team" preset stays).
- Test expectations updated mechanically (see §5).

No migration for existing profiles: preset cards are only ever created inside user record creation (`APICreateActiveUser` → `createPresetContactCards`), so existing profiles keep their stored card, matching how presets have always behaved. Users can still reach the bot via https://status.simplex.chat and the blog.

## 3. Why not fix the link instead (the #7200 dead end)

#7200 swapped the preset's link to a channel link (`https://smp5.simplex.im/c#...`). That cannot work: preset cards are contacts, and tapping one runs `APIConnectContactViaAddress` → `prepareContact`/`joinContact` — the direct-contact handshake. Channel short links (`/c#` decodes to `CCTChannel`) must be joined via `APIConnectPreparedGroup`; `APIConnect` explicitly rejects them ("channel links must be connected via APIConnectPreparedGroup"), but the contact-card path has no such guard and silently performs the wrong handshake. Making the status channel a preset would require a preset *prepared channel* — `APIPrepareGroup` needs a full connReq plus `GroupShortLinkData`, run after the user is active — a substantially larger change than wanted, for a card that confuses the users it is shown to.

## 4. Scope of effect

- **New profiles**: get only the "Ask SimpleX Team" card.
- **Existing profiles**: unchanged; their stored SimpleX Status contact remains until the user deletes it.
- **Discovery**: this removes the status bot's only in-app discovery path for new users — deliberate, since the card was doing more harm (confusion) than good (discovery).

## 5. Test impact

Removing the card shifts contact ids allocated after `/create user` down by one:

- 10 chat-list / `hasContactProfiles` expectations lose the status entry (`Direct.hs`, `Profiles.hs`).
- 24 numeric contact refs shift: `@6`→`@5` in `Direct.hs`; `@5`→`@4` and `@6`→`@5` in `Profiles.hs`, plus the id-arithmetic comments.
- `configureTimedMessages alice bob "6" "3"` → `"5"` — the contact id travels as a bare string argument, invisible to `@N`-pattern search; caught only by running the tests.

Verified unaffected: group ids (`#N`), pending-connection ids (`:N` — preset cards create no connection rows), user ids, unread/`/users` counts (cards create no chat items), `MobileTests`/`RemoteTests`/`Local.hs` (harness users bypass presets), display-name allocation (per-user).

## 6. Verification

All 8 non-timing-sensitive affected tests pass locally. The 3 TTL/timed-message tests (`testUsersDifferentCIExpirationTTL`, `testUsersRestartCIExpiration`, `testUsersTimedMessages`) fail identically on the base commit `6bb1da9e8` in the same environment — pre-existing cleanup-manager timing flakes (cf. `d7010d527`), not regressions. The packaged desktop build was verified to contain no "SimpleX Status" strings in `libsimplex.so` while retaining "Ask SimpleX Team".
