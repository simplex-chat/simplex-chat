# Parental controls — high-level design (iOS first)

Status: design for discussion. Not an implementation plan yet.
Platform: design and build on **iOS first**, then port the same UI to Kotlin
(Android/Desktop). No changes to the Haskell core for the basic version.

## What we are building

Three things, matching the requested direction:

1. A **parental-controls screen**: a list of on/off switches for functions a
   parent wants turned off on a child's device.
2. A **separate parent PIN** that protects that screen (and a few sensitive
   settings) — independent from the existing app lock.
3. A **"who is this device for?"** step in onboarding — *for me* or *for a
   child* — like Android's setup-for-someone-else.

## Scope and limitations

The person using the device is the child, and they physically control it, so
this cannot stop a determined, technically capable child. They could reinstall
the app (local settings reset), export-and-import the database into a clean
install, factory-reset, or observe the parent entering the PIN. This is the
same limitation Apple's Screen Time has.

The goal is therefore **casual-access prevention and a clear signal of intent**,
not a security boundary, and the feature should not be presented as one. A
separate **Hardening** section below adds *obstacles* (level 2), which raise the
bar but remain defeatable.

## Privacy boundary: disabling, not monitoring

Parental controls here only **turn capabilities off**. They never read, copy,
or report the child's messages. This matches SimpleX's published position that
privacy is the safety mechanism. A few candidate functions (preventing the
child from deleting messages, forcing disappearing-messages off) sit close to
that line — they are **off by default and flagged for decision**, because they
lean toward "let the parent inspect," which is monitoring.

## How it works (architecture)

- **Parent PIN** reuses the existing passcode machinery. The app already has a
  self-destruct passcode — "a second passcode protecting a second thing." The
  parent PIN is exactly that pattern: a new stored PIN, the existing PIN-pad
  screens reused as-is, and a separate "parent unlocked" state.
- **Restrictions are app-level capability gates.** Each switch is a stored
  on/off value; the app reads it at the point where the action starts (the scan
  button, the "create group" button, the file-download action) and hides or
  blocks it. This is the same pattern the app already uses for settings like
  "auto-accept images."
- **Not** the per-conversation feature negotiation (`ChatFeature` /
  `GroupFeature`). Those are negotiated *with the contact*, are per-chat, and
  are the wrong layer for a one-sided parental block.
- **No Haskell core / protocol changes** for the basic set. The only functions
  that *would* need the backend are explicitly excluded (see the table notes).
- Switch values are stored where the **notification and share extensions** can
  also read them, so blocks like "don't download files" hold everywhere, not
  just in the main app.

## Connecting: a PIN-to-allow-once exception

If "connect to new people" is off, the child still needs to reach the parent
and any approved new contact. So **only the connect action** has an exception:
when it is blocked, an alert offers to allow it once after the parent PIN is
entered, and that **single** connection then proceeds. Everything else, when
off, stays off — to permit something else temporarily, the parent opens the
PIN-protected settings screen and changes it there.

We do **not** build a "set up contacts then finalize" wizard: a parent can
connect to the child normally before turning restrictions on, which needs no
new code.

## The functions a parent can turn off

Legend — **Complexity**: rough build cost (Low / Med / High).
**Intrusiveness**: how much it limits the child's normal use and privacy
(Low / Med / High) — the autonomy trade-off, for discussion.

### Protective — the primary purpose

| Function | What "off" does / how | Complexity | Intrusiveness |
|---|---|---|---|
| Connect to new people (scan QR, open/paste link, create own address, 1-time invite, accept request) | Hide/disable these entry points. Child can still reach parent/approved people via the connect-PIN exception (below). **Highest-value control.** | Med (several entry points + the allow-once PIN piece) | Med |
| Create / join groups & channels | Hide "create"; block joining via link. | Low–Med | Med |
| Audio / video calls | Hide call buttons (outgoing); auto-decline incoming. | Med (incoming needs handling) | Med |
| Send files, images, videos | Remove attach options from the composer. | Low | Low–Med |
| Send voice messages | Remove the voice-record button. | Low | Low |
| Receive / open files & media | Turn off auto-download + block the download/open action — **in the app and the notification/share extensions.** Client-side only. (Truly stopping the file *offer* from arriving = backend + wrong layer = excluded.) | Med–High (multi-surface) | Med |
| Incognito mode | Hide incognito toggles so all connections use the known profile. | Low | Low |

### Integrity locks — prevent the child from bypassing the controls

Without these, the child could disable the protective switches or reconfigure
the app to get around them. These are a **fixed set of settings gated behind
the parent PIN** whenever parental mode is active — not individual parent
toggles, since their purpose is to prevent bypass. (They could be surfaced as a
single "lock important settings" switch if parent visibility is wanted.) The
parent still reaches them by entering the PIN.

| Protected setting / action | Why it's locked | Complexity |
|---|---|---|
| Parental-controls screen + parent PIN | The control itself. | Low (it's the gate) |
| Privacy & security settings (incl. app lock, developer tools) | Changing the app lock could lock the parent out; dev tools expose internals. | Low |
| Network & servers (custom servers) | Re-routing around restrictions. | Low |
| Database export / import / transfer to another device | Copies data into an unrestricted install (bypass + exfiltration). | Low |
| Create / hide additional profiles | A hidden profile bypasses every restriction. | Low–Med |
| Audio & video call settings | Reconfiguring call routing and servers. | Low |

Deliberately left open (no need to lock): **Appearance**, **Notifications**.

### For decision — high intrusiveness, off by default

These edge toward monitoring. Listed for the decision, not recommended for
"basic."

| Function | Note | Complexity | Intrusiveness |
|---|---|---|---|
| Prevent deleting messages (local + delete-for-everyone) | Lets a parent inspect history → monitoring-adjacent. | Med | High |
| Force disappearing-messages off | Also a *negotiated* feature → would need the backend, wrong layer. | Med (backend) | High |
| Prevent clearing / deleting chats | Monitoring-adjacent. | Low | High |

*Anything not in these tables that is wanted, we add as a row with the same
three judgements before building it.*

## Onboarding "who is this for?"

A new early onboarding step with two choices. Picking **for a child** routes
the parent into the parental setup (set the parent PIN, confirm a pre-checked
default set of restrictions), then continues the normal flow and hands the
device over. **for me** is the normal flow unchanged. The same setup is also
reachable later from Settings → Privacy & security → Parental controls, so
existing installs and later changes are covered. The onboarding step stays
small because it reuses the settings setup screen.

*(Working assumption — change to a lighter "configure in Settings" nudge if
preferred.)*

## Hardening (level 2 — separate, optional)

Obstacles that raise the bar further. Still defeatable; build after the basic
version works.

- **Survive reinstall (iOS).** App settings (`UserDefaults`) are wiped when the
  app is deleted, which resets every restriction — the main bypass. The iOS
  **keychain**, however, persists across uninstall/reinstall on the same device
  (it already holds the app passcode). So store the parental configuration
  (parent PIN + chosen restrictions) in the keychain rather than in settings. On
  launch, if that configuration is present, the app restores the restrictions
  and requires the parent PIN before they can be changed or removed. Still
  defeatable — "Erase All Content and Settings" or a device wipe clears the
  keychain — and the Android/Desktop port needs a different approach, since
  Android usually clears app data on uninstall.
- (Future, out of scope) A real boundary would need authority held on a
  **separate parent device** via SimpleX's remote-control/linking — a new
  subsystem, worth its own RFC if ever wanted.

## Suggested build order

1. Parent PIN + PIN-gated parental-controls screen (reusing existing passcode
   UI) + the integrity locks.
2. Protective switches and their gates — connect first (highest value), then
   the rest.
3. The connect "allow-once" PIN exception.
4. Onboarding "who is this for?" step.
5. Hardening (level 2).
6. Port everything to Kotlin (pure UI; no core changes).

## Open decisions

- The contents of the **default restriction set** pre-checked for "for a child."
- Which **for-decision** rows (if any) are in scope.
- Protected settings: locked automatically as a fixed set *(assumed)* vs a
  single visible "lock important settings" switch.
- Onboarding behaviour: **(a)** route into setup *(assumed)* vs **(b)** light
  nudge.
- Confirm connect exception = **PIN-to-allow-once, connect only** *(assumed)*.
