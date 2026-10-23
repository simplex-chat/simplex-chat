# r49 fixes progress (code: ~/wt-uitest-ref d72f68e6d)

All 9 items verified against code before editing; edits pending.

## Done (all 11 edits applied; catcheck TOTAL 1177 IGN 28, 0 bad cells)
- 02:283 contextMenu_itemsMatrix_byItemType: done. "Expected item set…" → per-type labels: sent text Reply/Share/Copy/Edit/Forward/Info/Delete/Select; received text same minus Edit; file offer (test.pdf, not received) Reply/Download/Info/Delete/Select; marked deleted (`\ @alice hi`) Reveal/Info/Delete/Select (CIV:557-568,698-700); merged feature row Expand/Delete/Select; single profile-update event (`/p bob Bobby`) Delete/Select; call item Delete/Select (CIV:600-603,376-384); deleted content via #club full-delete moderation (`\\ #club @alice hi`) Info/Delete/Select. Note: no direct-chat path yields deleted content (full delete removes the row), so that case uses bob's #club.
- 03:157 memberList_rolesAndStatuses: done. "Role/status text, «blocked»" → setup (alice creates #team, bob admin, cath member) + «you: creator»/«owner», «connected»/«admin», cath no role text, «blocked» after block-for-me (GCIV:772,1102-1107,1151; ChatModel.kt:2857; /ms team H/Groups.hs:437-442).
- 03:259 channel_broadcastFile (NIT): done. → `#team> sends file <name> (<size> / <bytes> bytes) [>>]`, `use /fr <n> … to receive it [>>]` (elided `|` per 02:90 convention; H/Groups.hs:12215-12216; View.hs:2508,2513).
- 04:140 chatRelays_toggleUseAndDelete: done. → switch "Use for new channels" under "Use relay" header; row disabled after back (ChatRelayView.kt:331,337-341); Log `/_servers` enabled false after "Save servers"; "Delete relay" closes with no alert, row gone (OperatorView.kt:150-153; ProtocolServersView.kt:93).
- 04:193 dbEncrypt_footerStates: done. → four strings per state (random stored :69-72; own stored :73-74; not stored :79-81; toggle on not yet stored :76-77); unencrypted delegated to 04:194.
- 04:281 linkMobile_onboardingLinkAMobile (NIT): done. "Pairing works…" → passphrase step first (SimpleXInfo.desktop.kt:19) then onboarding closes, stage OnboardingComplete (LinkAMobileView.kt:77-83; ConnectMobileView.kt:335-339).
- 01:174 planOwnInvitationConnectToYourself: done. + "Use current profile" (ConnectPlan.kt:470-476) → dismiss "Connection request sent!" (:417-433).
- 01:180 planAddressFullLinkConnectAlert: done. inserted "Use current profile" between the two alerts.
- 01:90 / 01:175 / 01:182 (NIT): done. Trigger now names first paste → "Use current profile" → dismiss "Connection request sent!".
- Needs codes: unchanged everywhere. Skipped: none.
