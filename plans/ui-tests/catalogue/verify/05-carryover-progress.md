# 05 carry-over map progress (round 20 + full map)

## Done
- Task 1 row fixes in 05: §6 share-links row (unlabelled share icon on the 1-time link / "Share address" / "Share 1-time link", refs NewChatView 715-757, UserAddressView 115,343-366, ContactConnectionInfoView 159-166); §13 back_onProtectedActionPrompt (entry Settings > "Migrate to another device", SettingsView 104); §14 interfaceSectionToolbars note (replaces 04 reachableChatToolbarVisibility); §14 Enter row pointer
- Map 05-android-carryover.md: rules R1-R10, "not listed" scope, summary; 01 (86), 02 (97), 03 (58), 04 (94) = 335 listed: 34 REPLACED, 50 DESKTOP-ONLY, 251 ADAPT; 842 unchanged
- All r20 GAPs/NITs applied as map rows (lock Keystore R8, 02 file rows, appearance zoom/persist/reachable, Esc rows, privacy/network/updates/migrateTo, serversSummaryFilesTab, noSelectedChatPlaceholder, channelRelays_removeLastRelayWarning, itemsMatrix, rcvFile_senderRevoked unchanged)
- 05 Notes carry-over lists replaced by pointer + general rules; §6 carry-over note, §10/§11 intros and header point to the map
- recount.py 05: 252 tests / 20 ignored / 15 sections (header unchanged)
- Checks: every map name exists in 01-04 at the cited line (scratchpad cmap/fill.py), REPLACED rows each name an existing 05 test, all 05 names cited exist; 01-04 concurrent edits (20:35/20:40) diffed vs r20a copies: only 02 itemsMatrix (now P, AV) affected -> R10 added

## Remaining
- none
