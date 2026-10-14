# r22c fixes progress (round 22 dimension C findings)

Findings' line numbers 01:283 and 02:63 were stale; the rows are 01:277 (deleteContactKeepConversation) and 02:58 (liveMessage_boltButton_emptyStart).

## Done
- F1 01:269 contactPrefsDisappearingMessagesAllow: starts on; off hides "Delete after" + "off …" line; on again 1 day + "enabled …" line
- F3/N1 01:251 call alerts: `/set calls @alice no` → "Calls prohibited!"; alice "You allow" no + CLI default yes → "Allow calls?" / "Allow" → CLI pref update line
- N2 01:229 addressAutoAcceptOff: turn on first (autoAccept null after create)
- N6 01:229 addressWelcomeMessage: Auto-accept on first (clears welcome), then welcome + Save
- N7 01:233 addressDelete: bob connected before creation, "Share" tapped
- N3 01:277 deleteContactKeepConversation: clears showDeleteContactNotice
- N4 02:58 live message: confirm "Live message!" / "Send"; note
- F2 02:265 memberContact_openButton: DM on precondition
- N8 03:91 groupPrefs_disappearingOff: turn on (3 s) + save first
- F2 03:92 DM: from default off — on→off; on + owners
- N5 03:140 retry >3 s note
- F2 03:176, 03:191, 03:192 DM on preconditions (03:192 also states bob not alice's contact)
- F2 04:58 DM on precondition
- catcheck: TOTAL 1177 IGN 28, badcells 0; needs.py unchanged

## Remaining
- none
