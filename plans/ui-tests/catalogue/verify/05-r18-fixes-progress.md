# 05 round-18 fixes progress

## Done
- WRONG-FACT sysNtf_previewHidden (no large icon; app-icon assertion moved to sysNtf_contactRequest_acceptAction)
- 6 GAP rows inserted (§6 attachSheet_keyboardOpen_fullyExpanded, copyAndroid_text_noToast, copyAndroid_fileItem_copiesCaptionOnly; §10 passcodeAndroid_landscapeShowsSubmit; §14 toolbar_notes_filterInMenuAndroid; §15 profilesAndroid_deleteLastVisibleReturnsToOnboarding)
- Notes bullets: copy, calls via CallActivity (replacement rows named), runtime permissions + "Paste link / Scan", notes filter, last-profile delete; setup item 2 pre-grants CAMERA/RECORD_AUDIO
- Open questions 10-12 (Open with viewer, ActivityNotFoundException stubs, offline vs Ethernet) + row pointers
- NITs: font slider; §14 XFTP/images dropped; clear-text row renamed compose_editText_clearButtonForWrappedParagraph (wrapped paragraph); MCAST (bridge) defined + §12 row
- Uniqueness across 01-05 checked (no duplicates); recount.py: 249 tests, 20 ignored; header totals updated

## Remaining
- none
