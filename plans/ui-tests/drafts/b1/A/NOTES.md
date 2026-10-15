# b1/A: catalogue 01 §1 Onboarding (desktop)

Files:
- `desktop/OnboardingTest.kt`: package `chat.simplex.app.ui.desktop`, class `OnboardingTest` (class map: 01 §1 → `OnboardingTest`). It has 22 tests, all desktop-only because they boot `app(null)` into the desktop onboarding (`App.kt:177` → `DesktopOnboarding`, `App.kt:275`). Android's onboarding is a different flow ("Get started", no passphrase step, notifications step).
- `screens.kt`: `OnboardingScreen` plus `UiScope.onboarding()`. It uses only shared scope methods (click, typeInto, poll, exists, waitFor), so it can go into the shared `harness/Screens.kt`.
- `keys.diff`: applies cleanly to `/home/user/wt-uitest` (checked with `git apply --check`).

Line numbers below are for the worktree (`/home/user/wt-uitest`, HEAD eda94a505; the onboarding files have no local changes). Where the catalogue's ref (`d72f68e6d`) differs, the ref line is given as "ref :N". C/ = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

## Keys added (keys.diff)

`Ui.Onboarding`:
- `LinkAMobile`
- `WhyBuilt`
- `Migrate`
- `NewPassphrase`
- `ConfirmPassphrase`
- `SetPassphrase`
- `RandomPassphrase`
- `SetupRouters`
- `SetupNotifications`
- `Continue`
- `Operator(operatorId)`
- `OperatorsOk`
- `HowItHelps`
- `ConditionsLink`
- `Accept`
- `ConditionsAccept`

New `Ui.Remote`:
- `DeviceName`
- `Qr`
- `ShareLink`

Derived key, no new declaration: `Ui.Onboarding.ProfileName.part("invalid")`, attached as `uiKey?.part("invalid")` on the invalid-name info icon inside `ProfileNameField` (`C/views/WelcomeView.kt:461`).

Existing keys extended to the desktop composables:
- `Ui.Onboarding.GetStarted` is now on the desktop "Create your profile" button (`D/views/onboarding/SimpleXInfo.desktop.kt:20`). Before this, only the Android "Get started" button had it. Both buttons lead to `Step2_CreateProfile`, so the key means the same thing on both platforms.
- `ProfileName` and `CreateProfile` were already on both the desktop and the mobile profile screens (`WelcomeView.kt:290,296,335,340`).

Where each key is attached:

| Key | Composable |
|---|---|
| `LinkAMobile`, `GetStarted` | `D/views/onboarding/SimpleXInfo.desktop.kt:19,20` |
| `WhyBuilt` | `C/views/onboarding/SimpleXInfo.kt:89` (desktop) and `:157` (mobile); `TextButtonBelowOnboardingButton` takes a new optional `uiKey` |
| `HowItHelps` | `OnboardingInformationButton` takes a new optional `uiKey` (`SimpleXInfo.kt:249`); passed at `ChooseServerOperators.kt:198` |
| `Migrate` | `MigrateButton` (`WelcomeView.kt:220`), desktop and mobile |
| `NewPassphrase`, `ConfirmPassphrase`, `SetPassphrase`, `RandomPassphrase` | `SetupDatabasePassphrase.kt:139-206` |
| `Continue` | `YourNetwork.kt:119` (mobile) and `:158` (desktop) |
| `SetupRouters`, `SetupNotifications` | the `Box`es at `YourNetwork.kt:174,207` |
| `ConditionsLink` | `ChooseServerOperators.kt:106` (mobile) and `:152` (desktop) |
| `Operator(id)` | the `TextButton` at `ChooseServerOperators.kt:236` |
| `OperatorsOk` | `ChooseServerOperators.kt:283` |
| `Accept` | `ChooseServerOperators.kt:319` |
| `ConditionsAccept` | `SimpleConditionsView`'s button (`C/views/usersettings/networkAndServers/NetworkAndServers.kt:842`; used only from onboarding) |
| `Remote.DeviceName` | `DeviceNameField` (`C/views/remote/ConnectMobileView.kt:161`) |
| `Remote.Qr` | `QRCode(invitation, modifier = …)` (`:195`) |
| `Remote.ShareLink` | the developer-tools "Share link" `SectionItemView` (`:210`) |

`ConditionsAccept` is a separate key from `Accept` because, while the conditions modal is open, the "Network commitments" Accept button underneath stays composed.

## Harness notes (apply to several tests)
- **Password fields.** `PassphraseField` uses `PasswordVisualTransformation('*')`, and the semantics `EditableText` is the transformed text (Compose 1.8 also has a separate `InputText` property). So the harness `typeInto`/`replaceText`, which poll `editableText == text`, cannot confirm input in these fields. The test file has a private desktop-only `typeSecret`, which uses `ui.onNode(...).performTextInput` and then polls the `EditableText` length. Moving it into the harness, or making `typeInto` read `InputText`, is the caller's call.
- **`OnboardingSharedState`** (`C/views/onboarding/YourNetwork.kt:37`, `internal object`) is read as the model for the operator selection. It holds the same `MutableState` that draws the check circles, copied through a `LaunchedEffect` (`YourNetwork.kt:52-54`). The operator cards have no other observable checked state, because the check icons have no description.
  - `internal` is visible from `desktopTest`, which is compiled with the module.
  - The object is not covered by `ModelSnapshot`, so it is not reset between tests. Every test re-enters "Your network", which overwrites it on composition.
- **Duplicate keys while HowItWorks is open.** On desktop with no user, `HowItWorks` repeats `OnboardingActionButton(user, …)` (`C/views/onboarding/HowItWorks.kt:46`). The keyed "Link a mobile"/"Create your profile" buttons then exist twice, once on the welcome screen underneath and once in the modal. `click` on them would fail while HowItWorks is open, so the test only asserts the heading.
- **Local servers.** The harness has no `useLocalServers()`. After onboarding, the connect test calls the desktop `TestServers.configure()` (`= configureServers(smp1, xftp1)`).
- **Remote host.** The "Link a mobile" tests set `appPrefs.offerRemoteMulticast` to false before opening the screen. The QR then comes from a non-multicast `startRemoteHost` (`ConnectMobileView.kt:282-287`), which should not need the MCAST category. Uncertain: whether `startRemoteHost` with multicast on fails in the loopback namespace was not checked.

## Per test

All rows are in `plans/ui-tests/catalogue/01-onboarding-chatlist-connect-profiles.md`. Needs: ONBOARD unless stated.

1. **onboardingWelcomeShowsDesktopActions** (catalogue :27)
   - Asserts: stage `Step1_SimpleXInfo` and no user. The keyed buttons read "Link a mobile" and "Create your profile", and the text button reads "Why SimpleX is built.".
   - Code: `SimpleXInfo.desktop.kt:16-21`, `SimpleXInfo.kt:66-92`.
   - Strings: `link_a_mobile`, `create_your_profile`, `why_simplex_is_built`.
2. **onboardingWhySimplexIsBuiltOpensHowItWorks** (:27)
   - Asserts: "You were born without an account." is absent; after clicking `WhyBuilt`, a fullscreen modal is open and the heading is shown.
   - Code: `SimpleXInfo.kt:89-91`, `HowItWorks.kt:31` (`why_built_heading`).
3. **onboardingCreateProfileValidName** (:28)
   - Asserts: `OnboardingScreen.createProfile("alice")` (type, button enabled, click) creates user alice. The stage becomes `Step2_5_SetupDatabasePassphrase`, and "Setup database passphrase" plus the `SetPassphrase` button are shown.
   - Code: `WelcomeView.kt:393-416`; `:404` desktop with `initialRandomDBPassphrase` (set by `DbTemplate.installIfPresent`).
4. **onboardingCreateProfileButtonDisabledForEmptyName** (:28)
   - Asserts: `CreateProfile` is disabled for "" and for "   ", and no user is created.
   - Code: `canCreateProfile`, `WelcomeView.kt:485-488` (it trims).
5. **onboardingCreateProfileInvalidNameShowsCorrectionAlert** (:28)
   - Asserts: for "alice_1", the info icon appears and Create profile is disabled. The icon opens "Invalid name!" / "Correct name to alice-1?", and confirming puts "alice-1" in the field, enables the button and removes the icon.
   - Code: `WelcomeView.kt:459-465,490-504`.
   - Haskell: `tests/ValidNames.hs:60` (`mkValidName "alice_1"` = `"alice-1"`); `mkValidName` is at `src/Simplex/Chat/Library/Commands.hs:6767` (ref :6703).
6. **onboardingCreateProfileBackReturnsToWelcome** (:28)
   - Asserts: `escape()` on the profile screen goes back to `Step1_SimpleXInfo`: the welcome buttons show, the name field is gone, and no user exists.
   - Code: `ModalView` `BackHandler` (`C/views/helpers/ModalView.kt:41`) → `onboardingBackAction` (`WelcomeView.kt:231-237`).
7. **onboardingMigrateFromAnotherDeviceOpensMigrationScreen** (:29)
   - Asserts: the button reads "Migrate from another device" (desktop string). After the click, `chatModel.migrationState` is `PasteOrScanLink`, with "Migrate here" and "Paste archive link".
   - Code: `WelcomeView.kt:205-229`; `C/views/migration/MigrateToDevice.kt:171,202-216`.
8. **onboardingSetDatabasePassphrase** (:30)
   - Asserts: the passphrase is typed twice and "Set database passphrase" is clicked. The "Encrypt database?" alert contains "Database will be encrypted.", and its confirm button reads "Encrypt". After confirming, the stage is `Step3_ChooseServerOperators` and the "Database encrypted!" alert shows. Once dismissed, "Your network" shows and `StoreDBPassphrase` in settings is "false".
   - Code: `SetupDatabasePassphrase.kt:47-74`; `DatabaseEncryptionView.kt:228-236,483-486`.
   - Haskell: `testDatabaseEncryption` (`H/Direct.hs:1589`) covers the core side.
9. **onboardingSetPassphraseMismatchDisablesButton** (:30)
   - Asserts: the confirm field is one character short, so the button is disabled. Typing the last character enables it.
   - Code: `SetupDatabasePassphrase.kt:119-124`.
10. **onboardingUseRandomPassphraseAlert** (:30)
    - Asserts: the alert text contains "Random passphrase is stored in settings as plaintext."; the alert's title and the button share the label "Use random passphrase", so the alert is identified by `AlertManager.shared.hasAlertsShown()` plus that text. The confirm button reads "OK". After it, "Your network" shows, `storeDBPassphrase` is still true and `EncryptedDBPassphrase` is still in settings.
    - Code: `SetupDatabasePassphrase.kt:181-186,232-239`.
11. **onboardingPassphrasePersistsAcrossRestart** (:31, ONBOARD + RESTART)
    - Asserts: after "Encrypt" and the "Database encrypted!" alert, `StoreDBPassphrase` = "false" and `EncryptedDBPassphrase` is absent. After `restart()`, `chatDbStatus` is `ErrorNotADatabase` and the screen shows "Encrypted database" and "Database passphrase is required to open chat.".
    - Code: `C/platform/Core.kt:75-78`; `C/views/database/DatabaseErrorView.kt:96-97`.
12. **onboardingYourNetworkShowsSetupRouters** (:32)
    - Asserts: "Your network" and "Network routers cannot know…" (substring, because the string contains `\n`). The routers button reads "Setup routers" and opens "Server operators", with a card per `chatModel.conditions` operator.
    - Code: `YourNetwork.kt:136-198`.
13. **onboardingContinueGoesToConditions** (:32)
    - Asserts: Continue → `Step4_NetworkCommitments`, "Network commitments", the conditions link exists and Accept is enabled.
    - Code: `YourNetwork.kt:157-164`; `ChooseServerOperators.kt:134-175`.
14. **onboardingChooseOperatorsToggle** (:33)
    - Asserts: clicking the first operator card flips its membership in the selection, and a second click flips it back.
    - Code: `ChooseServerOperators.kt:234-252`. See the `OnboardingSharedState` note above.
15. **onboardingChooseOperatorsCannotDeselectAll** (:33)
    - Asserts: deselecting every enabled operator empties the selection and disables OK. `escape()` does not close the modal. Reselecting one enables OK, which closes the modal and keeps that one selected.
    - Code: `ChooseServerOperators.kt:187` (`enableClose`), `:224-227`.
    - Close disabled: `BackHandler(enabled = false)` never reaches the desktop back stack (`D/platform/Back.desktop.kt:9`). The disabled back arrow has no key; it uses the same `enableClose`.
16. **onboardingHowItHelpsPrivacyInfo** (:33)
    - Asserts: "Network operators" is absent; after `HowItHelps` the fullscreen modal count is one higher and "Network operators" shows.
    - Code: `ChooseServerOperators.kt:198-201,362-364`.
17. **onboardingAcceptConditionsCompletesOnboarding** (:34)
    - Asserts: Accept → `OnboardingComplete`, the chat-list `NewChat` key and "Chats". The operators enabled in `chatModel.conditions` equal the onboarding selection.
    - Code: `ChooseServerOperators.kt:293-311,331-358`.
    - Haskell: `testOperators` (`H/Direct.hs:1226`).
18. **onboardingConditionsLinkOpensConditionsText** (:34)
    - Asserts: the link opens "Conditions of use" with its own Accept (`ConditionsAccept`), which completes onboarding and shows "Chats".
    - Code: `ChooseServerOperators.kt:147-164`; `NetworkAndServers.kt:831-849`.
19. **onboardingNoNotificationsStepOnDesktop** (:36)
    - Asserts: on "Your network", `SetupRouters` exists, while `SetupNotifications` and the text "Setup notifications" do not.
    - Code: `YourNetwork.kt:58-59` (the desktop branch has no notifications button) vs `:105-109`.
20. **onboardingThenConnectToCliPeerViaLocalServers** (:37, ONBOARD + P)
    - Asserts: full onboarding (random passphrase), then `quiesce()` and `TestServers.configure()`, then New chat → one-time link. bob runs `/c <link>`, and the test expects "confirmation sent!" then "alice: contact is connected", and contact bob is `ready` in the model.
    - CLI: as in `Helpers.connect`; Haskell `testAddContact` (`H/Direct.hs:191`).
21. **onboardingLinkAMobileShowsQrAndDeviceName** (:38, ONBOARD + CB + DT)
    - Setup: developer tools on and multicast off, both set after `app(null)` because `app()` restores the prefs.
    - Asserts: "Link a mobile" → passphrase step → random → `LinkAMobile` stage with the QR (`QR Code` description). "This device name" and "Discoverable via local network" show, and the name field equals `deviceNameForRemoteAccess`. "Share link" puts an `xrcp:/…` invitation on the clipboard stub.
    - Code: `LinkAMobileView.kt:24-89`; `ConnectMobileView.kt:193-213`; `D/platform/Share.desktop.kt:20-23`.
    - Invitation scheme: simplexmq `src/Simplex/RemoteControl/Invitation.hs:61`.
22. **onboardingLinkAMobileBackReturnsToWelcome** (:39)
    - Asserts: same entry as 21 (multicast off); `escape()` → `Step1_SimpleXInfo`, the welcome buttons show, the device-name field is gone and no user exists.
    - Code: `LinkAMobileView.kt:58`.

## Catalogue corrections

- **01:8 (setup item 3)**
  - Problem: "onboarding tests call `useLocalServers()`": no such harness function exists. Also, `acceptConditions` turns on the selected operators, not all preset ones.
  - Replacement: "**Local servers only.** `acceptConditions` → `setServerOperators` (`C/views/onboarding/ChooseServerOperators.kt:293-311`) enables the operators selected during onboarding (`enabledOperators`, :331-358), whose preset servers are unreachable in the loopback namespace; onboarded tests get the local servers in `resetForTest`; onboarding tests call `TestServers.configure()` (`configureServers(smp1, xftp1)`) after the chat list appears and before anything connects (plan §8.3). No core change is needed."
- **01:27**
  - Problem: the testability note does not say that HowItWorks on desktop repeats the welcome actions.
  - Append to Testability notes: "; on desktop with no user HowItWorks repeats the "Link a mobile"/"Create your profile" buttons (`HowItWorks.kt:46` → `SimpleXInfo.desktop.kt:16-21`), so their keys match two nodes while it is open"
- **01:28**
  - Problem: no Haskell test is cited for the name rule.
  - Change the end of Verification to: "Name rule: core `mkValidName` (`src/Simplex/Chat/Library/Commands.hs:6703`; Haskell `tests/ValidNames.hs:60`: `alice_1` → `alice-1`)"
- **01:29**
  - Problem: the Verification cell has no visible labels.
  - Replacement Verification: "Opens `MigrateToDeviceView` ("Migrate here", `MigrateToDevice.kt:171`) in the `PasteOrScanLink` state, showing "Paste archive link" and "Or import archive file" (`:215-221`)"
- **01:30**
  - Problem: the "Database encrypted!" alert after "Encrypt" is missing.
  - Replacement for the sentence about "Encrypt": ""Encrypt" sets `storeDBPassphrase=false`, encrypts, shows the "Database encrypted!" alert (`DatabaseEncryptionView.kt:475`, over the next screen) and moves to "Your network" (or "Link a mobile" if no user)."
  - Append to the mismatch sentence: "the button is also disabled when the new passphrase equals the current one or contains whitespace or non-ASCII characters (`SetupDatabasePassphrase.kt:118-123`, `validKey`)."
- **01:31**
  - Problem: the `storeDBPassphrase=false` line is cited as `SetupDatabasePassphrase.kt:50`; at ref it is `:52`. The prompt text is unnamed.
  - Fix the citation in both cells to `SetupDatabasePassphrase.kt:52`.
  - Replacement Verification: "After "Encrypt" (and its "Database encrypted!" alert), `storeDBPassphrase` is false and no passphrase is stored in settings (`EncryptedDBPassphrase` absent); after the restart the passphrase prompt (`DatabaseErrorView`, "Encrypted database" / "Database passphrase is required to open chat.", `DatabaseErrorView.kt:96-97` at worktree) is shown. Haskell: `testDatabaseEncryption`"
- **01:34**
  - Problem: "Accepting enables the preset operators (`ChooseServerOperators.kt:301-305`)" is wrong; it enables the selected ones.
  - Replacement: "Accepting enables the operators selected in onboarding (`ChooseServerOperators.kt:299-305`, `enabledOperators` :331-358), whose servers are unreachable in the namespace; tests that connect afterwards call `TestServers.configure()` (setup item 3)"
- **01:37**
  - Problem: Verification names `useLocalServers()`.
  - Replacement: "After onboarding (passphrase step included) the test calls `TestServers.configure()`, creates a 1-time link; CLI `/c <link>` gives "confirmation sent!" then "alice: contact is connected". Haskell: `testAddContact` (H/Direct.hs:191)"
- **01:38**
  - Problem: the QR depends on `startRemoteHost`, which announces over multicast while "Discoverable via local network" is on (the default).
  - Append to Testability notes: "; the test turns `offerRemoteMulticast` off before opening the screen so that `startRemoteHost` (`ConnectMobileView.kt:282-287`) does not announce over multicast (otherwise Needs would include `MCAST`)"
- **01:39**
  - Problem: the entry path omits the passphrase step that comes before the screen.
  - Replacement Entry path: "Welcome → "Link a mobile" → "Setup database passphrase" ("Use random passphrase" → "OK") → "Link a mobile" screen → Back"

## Rows skipped

None of the 22 assigned names. The other two §1 rows, `onboardingAcceptButtonDisabledWithNoOperators` and `onboardingCreateProfileWhenOnlyHiddenUsersExist` (both FIXTURE), were not assigned to this folder.

## Uncertainties
- Compose 1.8 `CoreTextField`: `EditableText` is assumed to be the transformed text, so `typeSecret` compares lengths. This works either way.
- `ProfileName.part("invalid")` sits on an `IconButton` inside the `BasicTextField` decoration box. It relies on the clickable `IconButton` staying its own node in the merged tree, which stops merging at nodes that merge their own descendants.
- `restart()` → `DatabaseErrorView` needs `AppLock.userAuthorized` to be true after the restart (`App.kt:150-153`). Not checked in the harness.
- Fresh-database operators: tests read `chatModel.conditions` (loaded at `Core.kt:143`) and never hard-code ids. `onboardingChooseOperatorsCannotDeselectAll` fails fast if no operator is enabled.
- `Ui.Remote.*` may overlap with keys drafted for catalogue 04 §K (remote desktop). Merge them if names differ.
