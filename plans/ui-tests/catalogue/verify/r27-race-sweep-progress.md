# r27 race sweep: CLI command printed before its effect reaches alice

Scope: 01–04 rows where a CLI peer's command line prints when the effect is queued, and alice then acts/asserts at once (absence checks, branch-dependent steps).
Model wording: 02 `timed_menuHidden_whenFeatureOff`, 01 `contactInfoCallProhibitedAlert`, 03 `groupInvitation_tapAfterAcceptShowsJoiningAlert`.

## Changed
- 01:66 `unreadFilterToggle`/`unreadFilterEmptyState` row: muted-chat-hidden absence now checked once the row shows its unread count (CLI message echo precedes delivery; vacuous pass otherwise).
- 01:125 `profilesHiddenNotListedAfterHide`: blind `wait(5 s)` replaced by CLI `/_get chat … count=1` until `sndRcvd` (receipt proves alice's core received it; Store/Profiles.hs:143 receipts default on).
- 01:178 `planInvitationDeletedLinkError`: `/_delete :1` deletes the queue in a forked thread (Commands.hs:1381, simplexmq Agent.hs:1013-1015); CLI repeats `/_connect plan 1 <link>` until the AUTH error before alice pastes.
- 02:78 `voice_prohibitedByContact_showsAlert`: `/set voice @alice no` then click → sync on "Contact preferences" "Contact allows" "no" for "Voice messages" (model wording).
- 02:201 `report_notOffered_forModeratorOrOwn`, `report_groupPrefOff_notOffered`: role change → event "changed your role to moderator"; `/set reports #jokes off` → "Group preferences" "Member reports" "off".
- 02:248 `status_authError_clickShowsDeliveryError`: `/d alice notify=off` deletes the queue in a forked thread (Commands.hs:1378); bob polls `/get servers summary 1` until `"_connDeleted":1` (simplexmq Agent/Client.hs:2122-2124) before alice sends.
- 02:253 `meta_signature_missingAlert`: `/set signatures #team on` moved before adding alice and cath (no GUI-observable sync exists: no feature item in regular groups, no prefs row).
- 03:96 `groupPrefs_voiceForAdminsOnly`: `/set voice #team on admin` → sync on «Group preferences» «Enabled for» «admins» under «Voice messages».
- 03:151 `groupInvitation_joinAfterHostDeleted_showsExpiredAlert`: group/invitation deletion is async (Internal.hs:2173-2185) → bob polls `/get servers summary 1` for `"_connDeleted":1` before alice taps.
- 03:172 `memberInfo_roleReadOnlyForHigherRole`: alice's admin role now comes with the invitation (`/a team alice admin`); a later `/mr` would race and a plain member passes vacuously.
- 03:200 `supportChat_hiddenWhenDisabledWithoutChat`: `/set support #team off` → sync on «Group preferences» «Chat with admins» «off».
- 03:218 `moderate_notOfferedForHigherRole`: moderator role via the invitation (`/a team alice moderator`).
- 03:227 `mentions_blockedMemberMentionIgnored`: no-notification check waits until the item shows «blocked by admin».
- 03:278 `joinChannel_noActiveRelays`: owner's `/rm #team <relay>` updates link data async (Internal.hs:1618-1626) → alice re-pastes (Cancel on «Open channel», no side effect) until the NoRelays alert.
- 03:282 `subscriber_promotedToContributorCanPost`: alice posts once compose no longer reads «you are subscriber».
- 04:55 `privacy_autoAcceptImagesOff`: no-`/freceive` absence checked once the image item is in alice's chat.
- 04:277 `linkMobile_multicastDiscovery`: after CLI `/stop remote ctrl`, alice taps the stored host only once her GUI is back to local chats (Commands.hs:3657, Remote.hs:666-687).

## Safe (examined, no change)
- 01 all other rows: CLI-command rows end in positive polled assertions (connects, accepts, `/ad`+`/share address`, `/pa on` address section 257, `/set voice @alice no` Contact allows 269, `/d alice` row 280, 186 already synced); `/_profile` + `/auto_accept` (187, 203) update address link data synchronously (`setMyAddressData` → `setConnShortLink`, Commands.hs:4096-4110); remaining absence checks follow GUI actions (out of class).
- 02 all other rows: positive polled results (feature items/offers 260, received edits/deletes/reactions, `/d alice` draft drop 238, chip counts, call alerts — FIFO on one connection for `/_call end` then `/call`); 79 `/set voice @alice yes` equals bob's default so no XInfo is sent (Commands.hs:4122 guard); 172 already waits for the "Files and media: off" item, which stands alone after the join's member-connected event (Subscriber.hs:911-914) so it shows text; 246 `/receipts @alice off` is local to bob; 151 full delete is decided by alice's own pref.
- 03 all other rows: CLI group changes end in positive polled results (feature/event items 104, 233; «group is deleted»/«removed from group» 114, 241-242, 283; relay status 262; relay bar 284; «Chat with admins» row 283; «Add friends» 299; support disabled text 203; disappearing member chat 201); roles given at invitation or gated by a control that only appears with the role (46 invite sheet, 113 FIFO on bob→alice); 145 `/gp` sets link data synchronously (`setGroupLinkData'`, Commands.hs:4154).
- 04 all other rows: GUI-side settings with positive CLI/GUI results (auto-accept 57-58, receipts toggles, SOCKS, proxy icon, show-encryption, notifications); migration `/_upload` link printed after upload completes (214-218); `linkMobile_mobileStopsSession` asserts the positive result; import row 184 receives before the absence check.
- Out-of-class mirror (alice's GUI deletion → CLI `/c`/send expects AUTH; the core deletes the queue in a forked thread): 01:89 `pendingConnectionMenuDeleteInitiated` (Commands.hs:1381), 01:233 `addressDelete` new-`/c` failure (Commands.hs:2492; the CLI first waits for "alice removed contact address", which narrows it); 01:127 `profilesDeleteInactiveWithConnections` (agent `deleteUser`, not checked). 03:128 `groupLink_deletedLinkRejectsJoin` (GUI deletes link → CLI `/c` AUTH). Not changed; reported.

## Remaining
(none)

## Tool checks (after all edits)
- `python3 tools/catcheck.py | tail -2`: `duplicates: [] 0` / `TOTAL 1177 IGN 28 SECS 72`; badcells 0 in all four files.
- `python3 tools/classmap.py table`: identical to the pre-edit output (no test names or Needs changed).
