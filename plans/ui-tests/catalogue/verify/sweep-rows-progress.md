# Sweep rows insertion (D1–D9 from uncited-files-sweep.md) — progress

Code: ~/wt-uitest-ref (master d72f68e6d), read-only.

## Done

- D1 → 02 §A after `toolbarCall_audio_opensCallLink`: `callStart_browserUnavailable_showsAlertAndEnds`. Re-verified (CallView.desktop.kt:153-171, CallManager.kt:75-97, strings 1291-1292). Fixed citation: `APIEndCall`/`withCurrentCall` → `Library/Commands.hs:1579-1583, 4232-4240` (throws `CENoCurrentCall`). Note says the throwing mode is a harness option of the recording `LocalUriHandler` (plan §7.6); plan §7.6 row got the clause.
- D2 → 02 §I after `edit_imageCaption_updates`: `edit_ownImage_previewWithoutCancel`, `edit_ownFile_previewWithoutCancel`. Re-verified (CMP:123-136, 264-278, 1207-1223; View.hs:2358-2362, 3066-3067). Made the new texts concrete ("hello" / "new doc") and both CLI lines explicit.
- D3 → 02 §E after `voice_playback_playPauseProgress`: `voice_preview_playPauseAndSeek`. Re-verified; fixed ComposeVoiceView citations (:106-134; `icon_descr_file` at :65).
- D4 → 02 §U after `featureItem_contactChangesPref_shown`: `featureItems_merged_iconRowExpandHide`. Re-verified (five items from `createFeatureEnabledItems_` on every connection path; texts via `featureText`/`timeText(null)` = "off"; ChatItemsMerger groups consecutive same category). Note updated to point at the fixed row.
- D5 → 01 §8 after `planViaChatItemLinkCard`: `linkCardInMessageShowsNameAndSignedInfoLine`. Re-verified; added `APIShareMyAddress` (Commands.hs:1247-1259: text = link) and the CLI line as `bob <# …`.
- D6 → 02 §U after `invalidJson_item_rendered`: `legacyDeletedItem_rendered`. Re-verified (CIV:588-598, 705-706; CIContent.hs:145-146, 591, 748; tag "sndDeleted" at :784).
- D7 → 02 §L after `forward_notOffered_forDeletedOrLive`: `forward_destinationProhibitsContent_alert`. Re-verified; added the wait for the image to load ("Forward" is offered only for a loaded file, CIV:471, ChatModel.kt:4610-4614).
- D8 → 04 §C after `lock_passcodeKeyboardKeys`: `lock_passcodeKeypad_digitsBackspaceClearReveal`. Re-verified; fixed "Save" disabled citation PasscodeView.kt:82 → :81.
- D9 → 01 §7 after `oneTimeLinkUsedNoAlert`: `oneTimeLinkQrClickSavesPng`. Re-verified (QRCode.kt:89-99, Utils.desktop.kt:239-252, Share.desktop.kt:25-43 / Files.desktop.kt:77-81, NewChatView.kt:76-96, 498, 510).
- Fixed 02 `featureItem_contactChangesPref_shown`: `/set voice @alice no` gives "Voice messages: off" (`prefEnabled`, Preferences.hs:1147-1153), which joins the connection's merged feature row; the test now right-clicks → "Expand" first; note states no message is sent in between.
- Recount: 01 350→352, 02 274→281, 04 232 (was 231); total 1,167→1,177 (28 ignored, runnable 1,149). Class map: 8 rows synced; max class 29, no class over 30.
- Prose tallies: 02 "Seam use" REC 8→9, AV 8→9 (added `voice_preview_playPauseAndSeek`). 03 "By Needs" unchanged (no 03 rows changed).
- Plan numbers: Summary 1,177; §7.7 1,149 runnable / 1,185 runs / 1,200–2,370 images; §10 table 352 / 281 / 232 / 1,177; §11.2 REC 9, AV 9; §12 P3 352, P4 281, P6 232; §14 1,177. Unchanged: 82 classes, 71 smoke, NAMES 13, RELAY 90, IMGCB 1, S 36.

## Remaining

(none)
