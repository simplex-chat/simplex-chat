# r45 fixes progress (reviewer C findings F1-F12, N1-N5; sweep of 03 multi-test rows)

Code: master d72f68e6d (~/wt-uitest-ref).

## Done
- (verification of all findings against code done; edits below as applied)

- 04:271 F1 done: + "and bob's row is gone" (UserPicker.kt:660-664)
- 01:46 N2 done: "New in %s" → "New in v7.0.1" (WhatsNewView.kt:946 last entry; v7.1 at :975 is commented out; isInUs :1083-1086)
- 02:157 N5 done: per-test outcomes, "Selected 1/2", "Nothing selected", (x) restores toolbar + compose (SEL:30-36, CV:298,1170-1176)
- 03:45 N4 done: "2 contact(s) selected" / Clear → "No contacts selected" (AddGroupMembersView.kt:272-292)
- 01:121 F3 done; 01:124 F4 done (hide must target the NON-active user: filteredUsers keeps the active user even when hidden, UserProfilesView.kt:305-315)
- 01:213 F11 done (added distinguishing text: info screen itself has "One-time invitation link" header at ContactConnectionInfoView.kt:134)
- 01:227 N1 done; 01:270 F5 done; 01:278 F2 done (archive via "Only delete conversation", reconnect for the second)
- 02:221 F6 done (CV:2594-2716, 3265-3298); 02:230 F7 done (group: "team" → "hey team", H/Groups.hs:499; goToResult closes search CV:1834-1838); 02:166 F8 done (H/Forward.hs:205-227/249-271/273-299); 02:171 F9 done (CIV:470-473)
- 03:48 F12 done: entry path picks «New member role» = «admin» (AddGroupMembersView.kt:39 default Member, 223-240); role label is lower-case "admin" (strings:1855)
- 04:132 F10 done. DISAGREE in detail: `json` is prettyPrint=true (SimpleXAPI.kt:6546-6551), so the Log has `"enabled": false` / `"deleted": true` (space after colon), not `"enabled":false`. Added note: both tests need a second saved SMP server (127.0.0.1) since disabling/deleting smp1 blocks save (Operators.hs:543-548); unsaved servers are removed without `deleted` (ProtocolServersView.kt:484-491)
- 04:63 N3 done (footer texts); 04:182 N3 done ("Database passphrase", DatabaseEncryptionView.kt:120)
- 02:171 F9: `live` is compose-state live message (CIV:124), so Forward is hidden on every item while a live message is in progress, not only "on a live item"

## Part 2: 03 multi-test rows (70 rows; 30 name every test with "`name`:" -> OK; 40 checked by hand)
Changed (outcome added, verified in code):
- 03:30 invalidNameAlertOffersFix (WelcomeView.kt:485-499; AddGroupView.kt:141-145,167)
- 03:43 searchFiltersContacts (getContactsToAdd AddGroupMembersView.kt:96-111)
- 03:47 <role> resolved per test (member default :39; observer picked)
- 03:104 readOnlyForMember (GroupPreferences.kt:241-251,282,314-327)
- 03:110 readOnlyForNonOwner (MemberAdmission.kt:91-98,117-131)
- 03:114 acceptedByAdmin + compose enabled (ChatModel.kt:1711-1713)
- 03:138 preparedShowsWelcomeBeforeJoin (H/Profiles.hs:4611-4628)
- 03:141 split OpensGroup / ShowsRole (ConnectPlan.kt:645-688)
- 03:148 joinFromChatListMenu (ChatListNavLinkView.kt:488-510)
- 03:178 markVerified / clearVerification (VerifyCodeView.kt:67-71,105-106,117; ChatInfoView.kt:1208-1215)
- 03:191 incognitoMemberSharesIncognitoProfile (H/Groups.hs:5079-5083)
- 03:192 rejectRequest + acceptIncognito (ComposeContextGroupDirectInvitationActionsView.kt:127-149)
- 03:200 supportChat_search (MemberSupportChatView.kt:106-108,119-121)
- 03:201 listSearchAndOpen / markRead / bannerInGroupChat / adminOpensFromMemberInfo (MemberSupportView.kt:99-103,134-145,182-212,277-282; ChatView.kt:1706-1731; GroupMemberInfoView.kt:408-424)
- 03:219 deleteOwnForEveryone (H/Groups.hs:1466-1469)
- 03:233 per-test mapping; memberLeftAndRemoved gains the «left» event (merge rule ChatModel.kt:3462-3470)
- 03:234 guiJoinerReceivesHistory (H/Groups.hs:5992-6028)
- 03:241 fromChatListMenu (ChatListNavLinkView.kt:475-484)
- 03:261 listShowsActive -> «active» (ChannelRelaysView.kt:178-193)
- 03:295 promoteCustomerToAdmin / removeMemberChatWording / deleteChat (GroupMemberInfoView.kt:305-311,968-985; GroupChatInfoView.kt:212-227,822-826)
OK as is (each test has a distinct outcome, though not prefixed by its name): 03:35, 74, 76, 78, 130, 142, 146, 176, 179, 212, 227, 242, 250, 255, 257, 262, 284, 286, 294, 296; 03:174 OK (parenthetical after the name)

## Checks
- catcheck: TOTAL 1177 IGN 28, badcells 0 in all four; no row with extra cells (unescaped-| count matches header in 01-04)

## Remaining
- none
