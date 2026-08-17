# Remove CLI help entries for long-removed commands

Branch: `nd/fix-cli-outdated-help` · file `src/Simplex/Chat/Help.hs`.

## 1. Problem statement

Typing `/get stats` in the terminal CLI does nothing useful — it is documented in `/help` but no parser accepts it, so it fails to parse. Investigation found this is not isolated: four documented commands no longer exist in the parser.

## 2. Solution summary

Remove the four stale entries (five lines, including one continuation note) from `Help.hs`:

- `/pq @<name> on/off` + its "(both have to enable…)" note — `contactsHelpInfo`
- `/pq on/off` — `settingsInfo`
- `/get stats` — `settingsInfo`
- `/reset stats` — `settingsInfo`

The stats pair were the tail of `settingsInfo`, so the now-orphaned trailing comma on the preceding `/(un)mute #<group>` element is also dropped to keep the list literal valid.

No replacement text is added: PQ has no command (it is automatic), and the stats functionality has no argument-compatible successor (see §4).

## 3. Root cause

Both removals were core changes that deleted parser, handler, and command constructor but left `Help.hs` untouched:

- **`/pq` (both forms)** — commit `756779186` "core: enable PQ encryption for contacts (#4049)", 2024-04-22. It removed the parsers `"/pq @" *> (SetContactPQ …)` and `"/pq " *> (APISetPQEncryption …)`; post-quantum encryption for contacts became automatic, so the manual toggle was obsolete. `SetContactPQ` and `APISetPQEncryption` no longer exist in `src/`.
- **`/get stats` / `/reset stats`** — commit `5907d8bd0` "core: remove legacy agent stats (#4375)", 2024-07-01. It removed the parsers `"/get stats" $> GetAgentStats` and `"/reset stats" $> ResetAgentStats`, their handlers, the `GetAgentStats`/`ResetAgentStats` constructors in `Controller.hs`, and the `View.hs` rendering — but its diff touched `Chat.hs`, `Controller.hs`, `View.hs`, `cabal.project`, `sha256map.nix`, not `Help.hs`.

In both cases the help text became a promise the binary could no longer keep.

## 4. Scope verification — no other stale entries, no replacements documented

All 120 commands documented across every section of `Help.hs` were extracted and matched against the parser string literals in `Library/Commands.hs` (`chatCommandP`). Every entry resolves to a live parser except the four above. ~10 entries that a naive prefix match flagged were manually confirmed valid: incognito-suffix forms parsed by `incognitoP` (`/accept incognito`, `/connect incognito`, `/simplex incognito`), usage examples (`/file bob ./photo.jpg`, `/group team`), and inline sub-alternatives (`/start remote host new`, `/stop remote host new`, `/switch remote host local`, `/chats all`).

Why no replacement text:

- **PQ** — there is no command; encryption is negotiated automatically. Documenting nothing is correct.
- **Stats** — the nearest live commands are `/get servers summary <userId>` and `/reset servers stats`, but they require a `userId` argument and return the agent servers summary, not the old argument-less usage statistics. They were never in CLI help; adding them is a separate documentation enhancement, deliberately out of scope for a "remove what no longer exists" fix.

## 5. Why this shape

Pure deletion of dead documentation — no behavioral change, smallest diff that makes `/help` truthful. Comma handling is the only subtlety: the `/pq @<name>` and `/pq on/off` removals sit before comma-bearing neighbors (a `""` separator and `/network` respectively) and need no adjustment; the `/get stats` + `/reset stats` removal makes `/(un)mute #<group>` the last `settingsInfo` element, so its trailing comma is removed to avoid a dangling-comma parse error before `]`.
