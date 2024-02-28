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
