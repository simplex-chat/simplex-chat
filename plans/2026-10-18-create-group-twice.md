# A double click on "Create group" creates two groups

Follow-up to #7725, stacked on its branch.

## Problem

"Create group" creates the group (`apiNewGroup`), opens it, loads its members and only then closes its page and opens Invite members (or the group link when incognito). The button stays enabled meanwhile and the core does not deduplicate, so a double click created two groups (the second named with a suffix, for example "G2_1"). Reproduced on the master prebuild at 40, 80 and 150 ms.

## Fix

The page remembers whether its create is in progress, as `CIGroupInvitationView` does for joining: the create handler checks and sets it at the start and clears it when its job ends, so a second click on that page while it is set is ignored.

## Not fixed

- The channel's "Continue" (desktop `onLinkReady`) was checked and not affected: a double click opened one link page in testing.
- A create that never returns keeps that page's "Create group" inert until it does; a page opened again meanwhile creates as on master.

## Effect

Desktop and Android: one group per double click. After a failed create the button works again. Single clicks are unchanged.
