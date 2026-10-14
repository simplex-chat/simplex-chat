# r23c fixes progress (round 23 dimension C: timing and state carry-over)

Line numbers per the findings' current list (01:186, 01:231, 01:255, 02:117/120, 02:171, 02:210, 02:244, 02:290, 03:91, 03:180, 03:298, 04:184). Finding F3's `H/Profiles.hs:1950` is 1955 at d72f68e6d.

## Done
- F1 02 info_delivery_liveUpdatesOnReceipt: split from info_delivery_groupMemberStatuses (that one keeps P ×2); cath stopped before alice sends, Info > Delivery once delivered-partial, cath started, turns delivered without reopening; Needs P ×2, PEERCTL
- F2 02 status_direct_sentThenDelivered: split from status_receiptsOff_staysSent (keeps P, S); bob stopped before send, sent → started → delivered; Needs P, S, PEERCTL
- F3 03 connectPlan_businessPendingOpensKnownChat: bob stopped before alice sends the request (after she opens the prepared chat)
- N1 01 planAddressDeletedContactReconnect: paste after the row shows "deleted contact"; Known until processed (Commands.hs:4519,4688-4689)
- N2 01 addressAutoAcceptIncognito: Auto-accept on (saves) → Accept incognito on → Save → CLI /c
- N3 02 send_apiFailure_restoresCompose, forward_itemsDeletedAfterSelection_alert: wait for "marked deleted"
- N4 02 rcvFile_xftpAuthError / rcvFile_expired: reset after bob's "completed uploading file 1 (test.pdf) for alice"
- N5 03 memberInfo_connectionFailedSection: cath started before smp1 stop; explicit timeout ≈ outage/2 + 5 s
- N6 04 dbMgmt_importArchive_restoresData: chat switched back on after export before bob sends
- N7 01 contactInfoSendReceipts*: repeat /_get chat until sndRcvd; "no" checked after wait(5 s)
- N8 03 groupPrefs_disappearingMessageExpires: turn on (3 s) + save first
- tools: recount (352/281/312/232, unchanged), catcheck TOTAL 1177 IGN 28 badcells 0, needs PEERCTL 02: 2→4 (total 19→21), classmap table unchanged

## Remaining
- none
