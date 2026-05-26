# Fix: "error store invalidMention" when an inserted @mention is edited in place

## Symptom

Rare "Error sending message: error store invalidMention" when sending a group
message that contains an @mention.

## Root cause

`removeUnusedMentions` in both `GroupMentions.kt` and `GroupMentions.swift`
prunes the compose-state `mentions` map only when the number of parsed
`@name` tokens is **strictly less** than the number of entries in the map:

```kotlin
if (usedMentions.size < composeState.value.mentions.size) { ... }
```

When the user picks a member from the picker, the client inserts `@Name` into
the message text and records `mentions[Name] = memberId`. If the user then
**edits the inserted token in place** (e.g. fixes a typo, deletes one
character) without re-picking from the picker, the parser sees one mention
with the new name while the map still contains the old name. Both have size 1,
so the guard does not fire and the stale entry is sent to the core.

The core's `getCIMentions` (`Internal.hs:267-271`) requires every key in the
client-supplied mentions map to also appear as a parsed `@name` token in the
message text, and throws `SEInvalidMention` otherwise.

## Fix

Prune whenever any map key is missing from the set of parsed mention names,
regardless of count. After pruning, the edited `@name` token becomes a plain
formatted mention with no member binding — exactly the desired behaviour for a
token the user has modified away from the originally picked member.

- `apps/multiplatform/.../GroupMentions.kt:321`
- `apps/ios/.../GroupMentions.swift:176`

Both clients had the identical bug; both are patched.

## Manual reproduction

1. In a group, type `@` and pick a member from the picker. Text becomes
   `@Name ` and the mentions map gets `{Name → memberId}`.
2. Place the cursor inside the inserted name and add or delete one character
   (`@Nam`, `@Names`, etc.) — do not use the picker.
3. Tap send. Before the fix: send fails with `error store invalidMention`.
   After the fix: message is sent, the modified token appears as a plain
   formatted `@` token with no member link.
