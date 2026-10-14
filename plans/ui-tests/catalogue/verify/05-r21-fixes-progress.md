# 05 round-21 fixes progress

## Done
- Verified: toast event class is `android.widget.Toast` (ToastPresenter.java:332, SDK 36), not `Toast$TN` (pre-R); app targetSdk 36; all Android toasts via showToast (UI.android.kt:23)
- Verified: setText -> cursor 0 (TextView.java:7504,7519; ArrowKeyMovementMethod.java:364); app forwards at PTF.android 123-128,160-167 (findings cited 126-131/156-160: off)
- Map: summary table + text (340 listed, 33/50/257, 837 unchanged); R11, R12; not-listed bullet (replaceText+setSelection, clear-text wrap note)
- Map rows: +01:120, +01:250, +03:57, +04:105, +04:187; 02:261 REPLACED->ADAPT; 02:262 Accept branch; 02:214 note; 04:183 R11
- 05: Needs-codes TOAST = R11 recorder; open question 13; 7 toast rows cite R11; setup item 5 cursor-at-end; 05:202 ref 715-763; §9 intro; Notes R1–R12 + bullet
- recount.py: 252 tests / 20 ignored / 15 sections (header unchanged); map recount 01 88(10/7/71), 02 97(12/11/74), 03 59(0/0/59), 04 96(11/32/53) = 340 (33/50/257) matches table; no dups; every row name at its cited line; all cited test names exist

## Remaining
- none
