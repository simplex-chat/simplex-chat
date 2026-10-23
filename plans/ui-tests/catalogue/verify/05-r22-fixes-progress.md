# 05 round-22 fixes progress

Input: r22e findings (scratchpad r22e/findings.md). Files: 05-android-carryover.md, 05-android-only.md, research doc line ~158 only.

## Done
- Verified: no LocalUriHandler/LocalClipboardManager provider in android/ or androidMain; Compose ui-android 1.8.2 AndroidUriHandler.openUri = startActivity(Intent(ACTION_VIEW, Uri.parse)), ActivityNotFoundException rethrown as IllegalArgumentException; AndroidClipboardManager wraps the system ClipboardManager.
- Verified: Utils.kt:605-611, SettingsView.kt:267-271, strings 2898-2902 (Cellular), cleartext SDK refs, MAN:40-50, targetSdk 36, jsoup 1.17.2, LinkPreviews.kt:96-140.
- Header-aware scans: URI 16 tests (10 unlisted), HTTP 5 (4 unlisted), CB 90.
- Map: R13 added; 13 ADAPT rows (10 URI R13 + 3 link preview 02:44); Not-listed bullets for CB (90) and 04:53 vacuous pass; Summary recounted: 01 91/10/7/74/261, 02 102/12/11/79/179, 03 59/0/0/59/253, 04 102/11/32/59/130, total 354/33/50/271/823; names at cited lines verified, no dups, rows in line order.
- 05: codes para (URI/CB via R13, restart -> OQ14); setup item 4 clears clipboard; §12 step 2 + 05:322 note; 05:399 verification/Needs INTENTS/Notes fixed; 05:402 Cellular; Notes R1-R13 + bullet; open questions 14 (restart) and 15 (debug network-security-config). recount.py: 252/20/15 = header.
- Research doc :158 corrected to point at R13.

## Remaining
- none
