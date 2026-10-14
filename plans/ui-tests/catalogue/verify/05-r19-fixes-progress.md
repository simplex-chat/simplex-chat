# 05 round-19 fixes progress

## Done
- NITs: l.124/l.225 P ×2 (bob, cath); l.147 "Denied once"; l.148 canAsk pre-set false (+ no-dialog check, no SYSUI needed)
- Rows: §13 pendingReview_closeSupportChat_returnsToChatList; §14 compose_enterKeyInsertsNewline_sendButtonSends, connectProgress_inChatStripAndCancel (verified against d72f68e6d; strings "Chat with admins", "Loading profile…", "Send Message", "Cancel")
- Notes GAP 1: message-field bullet rewritten (no key handling on Android; send-button rows, replaced row, desktop-only rows, §6 "File" stub for draft_withAttachment_restored and typed-path rows 02 l.115/116/118)
- Notes GAP 2: §W desktop-only rows named; passphrase-step + Link a mobile 01 rows desktop-only; callStart_browserUnavailable desktop-only; text selection; pagination 75; dbMgmt_layout folder; right-click -> longClick with every affected 01-04 row named (all onRightClick sites have a combinedClickable onLongClick twin, checked)
- Notes NIT: QR tap = share chooser for 01 oneTimeLinkQrClickSavesPng
- Uniqueness 01-05 (1429 names, no dups); all Notes test refs exist; new rows 8 cells
- recount.py: 252 tests, 20 ignored, 15 sections; header totals updated

## Remaining
- none
