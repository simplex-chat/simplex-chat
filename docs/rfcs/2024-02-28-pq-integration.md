# PQ integration in chat

## Problem

- Group size not known when joining
- Communicate intent and current state of each conversation

## Solution

### Group size not known when joining

- Add to XGrpInv GroupInvitation
  - pros: easy
  - cons: size can change before joining, but can ignore as it's still a good estimate

or

- Send before introductions
  - new protocol message
    - XGrpIntro :: GrpIntro -> ChatMsgEvent 'Json -- (GrpIntro is a box type with Int, for possible extension)
  - or put into XGrpInfo
    - XGrpInfo :: GroupProfile -> GroupStats -> ChatMsgEvent 'Json -- GroupData?
    - can update profile between invitation if it happened before joining
    - can later add logic to "verify" stats?
    - may be over-complicated until since there "supposed" use cases are out-of-scope / not planned / not known

- What should be default if it's not known? (e.g. admin has older version)
  - On -> then off when member count reaches 20?

### Communicate intent and current state of each conversation

- Current state items
  - RCEPQEnabled (see #3845) both for direct conversation and per member (regular event items, merged in UI)
  - created when PQ changes for contact/member (e.g. received from agent on MsgMeta / SENT)
  - experimental toggle is planned: it doesn't affect contacts/members with already enabled PQ
    - contact enabled PQ always overrides toggle (can't downgrade)
    - member enabled PQ also overrides, but can downgrade if group size increases past 20

- New items communicating state of e2e encryption in conversation
  - should be well pronounced in UI, not merged
  - should always say that conversation is e2e encrypted
  - in direct chats:
    - reflect actual state of PQ at the time of creation
    - created during connection handshake when receiving first info about PQ in MsgMeta / some other event (TBC agent api)
    - will not update if state changes (e.g. upgrades), as toggle is planned to be removed, PQ can't be downgraded, all will support soon
    - flag in contacts table "e2e_info_created" to only create it once?
    - should create for legacy contacts or not?
  - in groups:
    - reflect intent (should say "PQ will be used for members who support") based on number of members (see above) + toggle
    - created at the same time as feature items? race with history may be possible, but we don't observe it? need to double check or ignore
    - if based on XGrpInv GroupInvitation (first option above), can create item even before joining
    - also will not update (as conversation progresses and it will scroll far up anyway) even if group size changes and it's disabled
    - flag in groups table "e2e_info_created" to only create it once? and state is only reflected by RCEPQEnabled items?
    - or create new such item if group size increases and PQ is off / decreases and PQ is on?
    - "large group" thresholds have to different for group size increasing (e.g. 20) and decreases (e.g. 15), to avoid constant switching on the border.

- Example texts for "e2e encryption info" chat items:
  - for direct conversations:
    - with PQ (and also forward a couple releases when more clients have upgraded):
      ```
      Messages in this conversation are end-to-end encrypted.
      Post-quantum encryption is enabled.
      ```
    - no PQ (experimental toggle disabled):
      ```
      -//- (e2ee)
      Post-quantum encryption is not enabled. [Also possibly:] Enabling post-quantum encryption in experimental settings will enable it in this conversation if your contact supports it.
      ```
    - no PQ (experimental toggle enabled):
      ```
      -//-
      Post-quantum encryption will be enabled when your contact upgrades.
      ```
      "upgrades" / "supports it" / "starts to support it"
    - can be of different color, but seems unnecessary
    - created once at the start of conversation
    - created once for old contacts when PQ is enabled?
  - for groups:
    - with PQ (small group; toggle enabled or later, as above):
      ```
      -//-
      Post-quantum encryption will be enabled for members who support it.
      ```
      can remove qualification later when most clients have upgraded
    - no PQ (large group):
      ```
      -//-
      Post-quantum encryption is not enabled (group is too large).
      ```
    - created each time group changes between small/large, or once?
    - created for old groups when experimental toggle is first turned on, and first message is received?


- Save PQ encryption on chat items (messages)?
  - in meta for direct + group rcv
  - in group_snd_item_statuses for group snd?
  - display in chat item details (info)
  - may be overkill if aggressive upgrade strategy is planned
