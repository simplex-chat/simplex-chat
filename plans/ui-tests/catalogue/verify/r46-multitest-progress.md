# r46 multi-test sweep progress (01, 02, 04)

Code: master d72f68e6d (~/wt-uitest-ref). List recomputed: 120 multi-test rows in 01/02/04 with a test name missing from Verification; 56 already reviewed in r46c (plus 01:121, 01:270 r46c OK; 04:301/302 IGNORE rows). 62 left to check below.

## Done
- 01:27 01:28 01:30 01:34 01:56 01:59 01:66 01:67 01:71 01:79 01:80 01:88 01:90 01:91 01:97 01:98 01:127 01:130 01:132 01:141 01:145 01:173 01:213 01:223 01:239 01:255 01:261: OK (each test has its own clause)
- 01:70 fixed: createListFromAddList chip with name; createListWithEmoji chip shows emoji in place of the label icon + Log `/_create tag` `"emoji": …` (ChatListView.kt:1336-1344; SimpleXAPI.kt:4169)
- 01:93 fixed: contactWithRequestComposeReject confirm "Reject" -> back to chat list, row gone, CLI silent (ComposeContextContactRequestActionsView.kt:122-131; ChatListNavLinkView.kt:759-771)
- 01:202 fixed: preparedContactSendRequestAlertAddMessage "Add message" sends nothing (ComposeView.kt:684-689 dismiss only)
- 01:204 fixed: ChangeProfile -> CLI "alice2: contact is connected"; Incognito -> "<rand>: contact is connected" (ComposeContextProfilePickerView.kt:80-94; H/Profiles.hs:4716-4721,4780-4797)
- 01:233 fixed: addressDeleteKeepsPendingRequests -> pre-deletion request row stays, Accept -> CLI "alice: contact is connected" (H/Profiles.hs:1853-1862)
- 01:240 fixed: simplexNameUnsavedChangesAlert "Exit without saving" -> address screen still "Get SimpleX name (BETA)" (SetSimplexNameView.kt:102-121)
- 01:254 fixed (also WRONG-FACT like r46 F1): Mark verified closes the code screen (VerifyCodeView.kt:40-45), info row "View security code"; ClearVerification in place -> "bob is not verified" (VerifyCodeView.kt:66-72,105-108)
- 01:129 fixed: editProfileUnsavedChangesAlertSave was "saves and notifies" -> name changed to alice2, "Save and notify contacts" -> CLI "contact alice changed to alice2" (UserProfileView.kt:104,288-293)
- 02:24 02:41 02:43 02:57 02:69 02:71 02:90 02:112 02:143 02:152 02:167 02:168 02:170 02:222 02:225 02:232: OK
- 02:44 fixed: linkPreview_firstTimeAlert_enable_fetchesPreview -> "Enable" fetches; compose shows og:title + URL (CMP:479-483; LinkPreviews.kt:168-180)
- 02:45 fixed: linkPreview_received_rendersCard "renders the card" -> "link preview image" + title/description + URL (LinkPreviews.kt:194-218)
- 04:63 04:84 04:88 04:99 04:108 04:135 04:141: OK
- 04:82 fixed: lock_screenOnStartup_correct had only labels -> right passcode + Submit shows "Chats" (LocalAuthView.kt:35-39; AppLock.kt:162-163; App.kt:210-224)
- 04:149 fixed: "Log /_network with smpProxyMode" could not fail (encodeDefaults=true, SimpleXAPI.kt:6546-6551) -> "Never" => `"smpProxyMode": "never"`, Pref NetworkSMPProxyMode=Never, "Allow downgrade" disabled (AdvancedNetworkSettings.kt:235); "No" => `"smpProxyFallback": "prohibit"`, Pref NetworkSMPProxyFallback=Prohibit (SimpleXAPI.kt:364-366,3881-3882,5356-5375)

## Checks
- catcheck: TOTAL 1177 IGN 28, badcells 0 in all four; cell counts match headers in 01, 02, 04

## Remaining
- none
