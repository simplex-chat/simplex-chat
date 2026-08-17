# Fix "error store invalidMention" and preserve mention bindings across in-place edits

## Symptom

Rare "Error sending message: error store invalidMention" when sending a group
message that contains an @mention.

## Root cause

When the user picks a member from the picker, the client inserts `@Name` into
the message text and records `mentions[Name] = memberId` in the compose
state. The original `removeUnusedMentions` only pruned that map when the
parsed `@name` count was **strictly less** than the map size:

```kotlin
if (usedMentions.size < composeState.value.mentions.size) { ... }
```

If the user **edits the inserted token in place** (e.g. fixes a typo, deletes
one character) without re-picking from the picker, the parser sees one
mention with the new name while the map still contains the old key. Both
have size 1, so the guard does not fire and the stale entry is sent. The
core's `getCIMentions` (`Internal.hs:267-271`) requires every key in the
client-supplied mentions map to also appear as a parsed `@name` token in the
message text, and throws `SEInvalidMention` otherwise.

A naive fix (prune whenever any key is missing from the parsed names) stops
the crash but also drops the binding the instant a letter is removed — so
typing the letter back leaves an unresolved `@name` rather than a real
mention.

## Approach

Stop mutating the compose-state mentions map while editing. Treat the map
as a sticky cache of `name → memberId` bindings recorded by the picker, and
filter against the currently-parsed text only at the points where a "current
set of mentions" is actually needed: sending, picker UX, name
disambiguation. This both eliminates the `SEInvalidMention` failure and
lets a round-trip edit (`@Usernama` → `@Usernam` → `@Usernama`) re-resolve
the original member.

## Changes

### 1. Drop `removeUnusedMentions`

- `apps/multiplatform/.../GroupMentions.kt`: delete the function and its call
  in `messageChanged`.
- `apps/ios/.../GroupMentions.swift`: same.

### 2. Filter at send time via `memberMentions`

`ComposeState.memberMentions` intersects the cached map with the names
currently parsed in `parsedMessage`. All sends already route through this
getter, so this is the single chokepoint that determines what reaches the
core.

### 3. Picker "max reached" uses parsed-mention count

`GroupMentions.kt:213,231` and the Swift equivalent at `GroupMentions.swift:50`
switch from `composeState.mentions.size` to a count of `Format.Mention`
entries in `parsedMessage`. This keeps the limit aligned with what the
server will see, regardless of how many stale entries the cache holds.

### 4. `mentionMemberName` disambiguation uses parsed names

`ComposeView.kt`'s `mentionMemberName` walks the set of parsed mention names
instead of `mentions.containsKey`. Same for Swift. A stale cache entry no
longer pushes a fresh pick to an awkward `_1` suffix, and a picker tap on a
broken `@alic` cleanly replaces it with `@alice`.

### 5. Editing-an-existing-item path

`ComposeState(editingItem, ...)` seeds `mentions = editingItem.mentions`.
That stays as-is — it is already a sticky cache and the new `memberMentions`
getter handles filtering.

## Manual reproduction (original bug)

1. In a group, type `@` and pick a member. Text becomes `@Name ` and the
   mentions map gets `{Name → memberId}`.
2. Place the cursor inside the inserted name and add or delete one character
   (`@Nam`, `@Names`, etc.) — do not use the picker.
3. Tap send. Before the fix: send fails with `error store invalidMention`.
   After the fix: message is sent.

## Edge cases to test

- Pick `Usernama`, delete last `a`, retype it → send: mention resolves to
  the original member (restoration behaviour).
- Pick `Usernama`, delete the whole token → send: no mention. Cache still
  holds a stale `Usernama` entry but `memberMentions` is empty.
- Pick `alice` (member A), delete the `@alice` text, then pick a different
  `alice` (member B) → inserted as `@alice` (the cleaner, visible name);
  cache key rebinds A→B as an explicit user action.
- Two members with same display name, pick A, edit `@alice` → `@alic` →
  `@alice` by typing only → A's binding is restored (typing never mutates
  the cache; only picker taps do).
- Pick 3 members, delete one of the tokens — picker should not show
  "max reached"; only 2 mentions are now in the text.
- Edit an existing sent message: original mentions render, surviving edits
  re-bind, removed ones disappear from `memberMentions` on send.

## Out of scope

- No core changes. `getCIMentions` (`Internal.hs:267-271`) stays as-is;
  this is purely client-side cache lifecycle.
- No change to the @-picker trigger heuristic in `messageChanged`.
