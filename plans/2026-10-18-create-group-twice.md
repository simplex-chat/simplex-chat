# A double click on "Create group" creates two groups

Follow-up to #7725.

## Problem

"Create group" creates the group (`apiNewGroup`), opens it, loads its members and only then closes its page and opens Invite members (or the group link when incognito). The button stays enabled meanwhile and the core does not deduplicate, so a double click created two groups (the second named with a suffix, for example "G2_1"). Reproduced on the master prebuild at 40, 80 and 150 ms.

## Fix

The page remembers whether its create is in progress, as `CIGroupInvitationView` does for joining: the create handler checks and sets it at the start, so a second click on that page while it is set is ignored. It is cleared only when the create fails. A successful create closes the page, but the page leaves the screen only after its job has ended (in testing the job ended about 0.7 s after the click and the page was gone 0.8 to 1.1 s after it), so clearing it at the end of the job let a second click in that gap create a second group.

## Not fixed

- The channel's "Continue" (desktop `onLinkReady`) was checked and not affected: a double click opened one link page in testing.
- A create that never returns, or that throws instead of returning an error, keeps that page's "Create group" inert; a page opened again creates as on master.

## Effect

Desktop and Android: one group per double click. After a failed create the button works again. Single clicks are unchanged.
