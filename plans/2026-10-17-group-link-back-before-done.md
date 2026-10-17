# Group info keeps the old group link after leaving the "Group link" page early

Follow-up of #7726 (builds on its branch).

## Problem

- Group info → "Create group link" (the page creates the link at once) → Back before the link is created: the info still offers "Create group link" although the group now has a link, until it is shown again.
- "Group link" → "Delete link" → Back before the deletion finishes: the info still shows "Group link" although the link is gone.
- In a group where you are incognito, the info's "Invite" button opens the group link page. A link created there never reached the info, which kept offering "Create group link" (tapping it then creates a server queue and fails with a duplicate link error). Deleting the link there had the same problem in the other direction.

Reproduced on master with the GUI. Not reproduced: changing the link's initial role and going Back at once (the page shows the new role when opened again).

## Cause

`ModalManager.end` composes only its top modal, so the group info is disposed while the "Group link" page is open and composed anew after Back, starting from the link the info had loaded (`preloadedLink`) at that moment. Creating and deleting a link finish in the background (`withBGApi`); when Back came first, the result was written to the state of the info composed before and to the plain `preloadedLink` variable, which the new info had already read.

The info's "Invite" button for incognito members opened the page through `openGroupLink`, which passes no `onGroupLinkUpdated`, so nothing reported the change back.

## Fix

- `preloadedLink` becomes Compose state owned by the info modal (`mutableStateOf`, created with the preload), and the info shows `if (linkLoaded) preloadedLink.value else null` instead of keeping its own copy. A late result written to it reaches the info that is shown. The write keeps #7726's guard: only while the loaded values belong to the group the update came from.
- The incognito "Invite" button opens the info's own group link page (`manageGroupLink`, the same page as the "Group link" row), which reports changes back. Like #7726's channel "Link" button, it is available once the link is loaded, because the page creates a link when it gets none.

Alternatives considered:
- Give `openGroupLink` an `onGroupLinkUpdated` parameter and pass the info's callback through the layout to the button. Rejected: more parameters threaded through, for the same result.
- Keep Back disabled while the link is being created or deleted. Rejected: changes the page's behaviour for a background operation.

Not changed (same before):
- Tapping "Create group link" (or "Invite") again in the moment between Back and the late result opens the page without a link, which creates one again and fails with a duplicate link error.
- Closing the info and opening it again while a link is being created can load the link before it is saved; the late result goes to the closed info.
- A result that arrives while the same group's info reloads its members is dropped; the reload's own result is shown.

## Verification

Desktop, #7726's branch head (before) vs this branch (after), the same test plan on fresh profiles (two contacts; two groups the user admins; a member-only group; an incognito group created in the app):

| # | Check | before | after |
|---|---|---|---|
| C1 | "Create group link" → Back at once (0.2–0.5 s), wait 5 s | "Create group link" while the link exists | "Group link" |
| C2 | "Delete link" → confirm → Back at once | depends on timing: stale "Group link" until the info is shown again when the delete finishes after Back (reproduced on master) | "Group link", then "Create group link" when the delete finishes |
| C3 | Incognito group, link deleted → "Invite" → link created → Back | "Create group link" while the link exists | "Group link" |
| C3-del | Incognito "Invite" → "Delete link" → Back | "Group link" while there is no link | "Create group link" |

Identical on both: first open (a group with a link shows "Group link", a member-only group no link row); create a link and go Back after it is made → "Group link", the same link when opened again; delete and go Back after it is done → "Create group link"; initial role change persists; with the info or the link page open, switching groups shows each group's own row; "Invite" as a non-incognito admin opens the add-members screen. No crashes; no `E:` log lines other than `apiGetGroupLink … groupLinkNotFound` for groups without a link.

Android (Waydroid debug APKs, #7726's head vs this branch, each check from the same app snapshot): "Create group link" → Back at once: before "Create group link" while the link exists, after "Group link"; incognito "Invite" → link created → Back: before "Create group link", after "Group link". Identical on both: create and delete with Back after they finish, the non-incognito "Invite" opens the add-members screen, initial role change persists. No app crashes.
