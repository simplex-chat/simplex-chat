# Per-name expectations for multi-name rows (01–04)

Source list: r17c findings (area 3 NITs). Line numbers as of 2026-10-12 (edits keep line numbers). Only Verification cells changed (plus one Notes clause on 02:48). Code refs: master d72f68e6d at ~/wt-uitest-ref.

## Done
- 01 (16 rows, every name): 73, 84, 101, 110, 122, 160, 169, 199, 219, 227, 246, 248, 254, 266, 273, 274
- 02 (19 rows, every name): 38, 46, 48 (+Notes: the bot's command set), 129, 137, 147, 148, 155, 157, 162, 173, 195, 215, 218, 227, 232, 253, 260, 268
- 03 (26 rows, every name): 33, 58, 79, 91, 112, 127, 128, 139, 161, 171, 173, 174, 175, 211, 218, 240, 254, 258, 263, 264, 265, 268, 278, 282, 283, 299
- 04 (1 row): 154
- catcheck after edits: TOTAL 1167, IGN 28, badcells 0, duplicates 0

## Remaining
- none

## Unconfirmed
- 03:264 `channelWelcome_newSubscriberSeesWelcome`: kept as the bare test ref (…LinkAfterWelcomeUpdate :10049). The core puts the welcome in a prepared group as an item (`src/Simplex/Chat/Library/Commands.hs:2226-2233`, seen only via test-view `/_get chat`, H/Groups.hs:10082); no real-CLI line was found showing a `/c`-joined subscriber the welcome, so no oracle was written.
