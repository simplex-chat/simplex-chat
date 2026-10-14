# Android harness research (2026-10-13), notes

Worktree: /home/user/wt-uitest (branch nd/desktop-ui-tests). Read-only research.

## Q1 build
- android/build.gradle.kts:24 stale runner android.support.test.runner.AndroidJUnitRunner
- android/build.gradle.kts:189-192 androidTest deps: androidx.test.ext:junit 1.2.1, espresso-core 3.6.1, ui-test-junit4 commented out
- gradle.properties:33-35 kotlin 2.1.20, AGP 8.10.1, compose 1.8.2
- existing android/src/androidTest/java/chat/simplex/app/ExampleInstrumentedTest.kt (stub)
- abi splits arm64-v8a+armeabi-v7a (build.gradle.kts:131-143)
- ui-test-junit4-android NOT in ~/.gradle cache (only ui-android 1.8.2, androidx.test runner 1.6.1/core 1.6.1/monitor 1.7.1, espresso-core 3.6.1, ext junit); first build needs network. uiautomator, espresso-intents not cached.
- common/build.gradle.kts: androidTarget()+jvm("desktop") library; desktopTest has uiTestJUnit4+pty4j (wt diff). Ui.kt:15 `internal var uiTagsEnabled` -> invisible from :android androidTest. Other harness symbols public (deleteChatDatabaseFilesAndState DatabaseView.kt:556, stopChatAsync :509, initChatController Core.kt:70, chatCloseStore Core.kt:26, runMigrations AppCommon.kt:46). Private statics via reflection (works on ART).
- android.sh:42-46 task hardcoded :android:assembleFossDebug; publishes only arm64 app APK via output-metadata (:157-168). patch_slot_common (common.sh:931-943) seds splits to arm64-v8a in slot copy only. Worktree has NO local.properties -> android.sh uses local.properties.example (.debug suffix).
- ExampleInstrumentedTest asserts packageName "chat.simplex.app" -> fails with .debug suffix; delete.

## Q2 boot
- SimplexApp.kt:73-87 onCreate: initHaskell, initMultiplatform, runMigrations, initChatControllerOnStart() (if no app passcode)
- Core.kt:59-67 initChatControllerOnStart launches coroutine; Core.kt:70-191 initChatController; with no user on Android sets onboardingStage Step1 (:168), no startChat
- DatabaseUtils.kt useDatabaseKey: no DB -> random key into keystore (ksDatabasePassword) + initialRandomDBPassphrase=true
- prebuilt core ~/libsimplex.so = simplex-chat 7.1.0.6 / simplexmq 7.1.0.6 (strings); worktree simplex-chat.cabal:8 7.1.0.11; app 7.1-beta.6 (gradle.properties:27)

## Q3 network
- UITestServers.hs:41 prints smp://<hash>@localhost:<base+1>; desktop runner (uitest.sh:64-66) runs everything in unshare -rn netns (loopback only)
- wd.sh:361-366 net isolate = nft forward drop on bridge (per instance BR); input to host still accepted (net_allow wd.sh:115-119)
- instance 1: waydroid0 192.168.240.1 / dev .112; instance 2: waydroid1 192.168.241.0/24, dev 192.168.241.113 (README:82); android.md:111 local SMP at 192.168.240.1 reachable from instance 2
- SMP server binds all interfaces (simplexmq Transport/Server.hs:233-238 startTCPServer Nothing, AI_PASSIVE, prefers AF_INET6 ::) -> reachable on 127.0.0.1 (adb reverse) and bridge IPs when run in host netns.
- NetworkObserver.kt:56-92: online = INTERNET && VALIDATED; offline -> apiSetNetworkInfo(offline) after 3 s. Under `net isolate` validation unverified (captive_portal_mode currently null on instance 1; VALIDATED with net open).
- evidence/net/net-probe-isolated.txt: bridge + adb reverse work while isolated, internet blocked.
- instance 2: waydroid1 192.168.241.1 (waydroid2/lib/data/scripts/waydroid-net.sh:20), container mem cap 2500M (lxc config:38); currently STOPPED. Instance 1 running, free, no instrumentation installed, no reverses.
- wd.sh:362 isolate note text hardcodes 192.168.240.1 (cosmetic for instance 2).

## Q4 runner
- wd.sh shell (wd.sh:388) holds the instance flock for the whole command (with_lock wd.sh:53, dispatch :376) -> do NOT run long am instrument through wd.sh shell; use adb -s $(wd.sh serial) directly while holding the claim; refresh claim with `wd.sh claim <same name>` (no lock, :375).
- wd.sh install = adb install -r -g (wd.sh:253) grants runtime perms; pm clear revokes them -> regrant.
- code/testing/android.md:67 pm clear/reset-app/uninstall kills every wd-app snapshot on that instance (keystore key).

## Q5 multi
- README:74-102, android.md:47-53,189: per-instance claim; WD_INSTANCE=2 up/down; ~1.5-2.5 GB per instance; host 12.7 GB, ~7 GB available at time of check.

## Misc
- SimplexApp.kt:50-70 wraps main Looper.loop() in a catch loop + registerGlobalErrorHandler: main-thread exceptions become in-app alerts, not instrumentation crashes.
- Address Share on Android = Intent.createChooser via androidAppContext.startActivity (Share.android.kt:30-45) -> Espresso-Intents stub; no TestRoot clipboard on Android.
- Ui.Appearance.Zoom only in desktopMain (Appearance.desktop.kt:139) -> Android appearance test drops zoom.
- Ui.Chat.Send declared but not applied anywhere; Android send needs it (or content-desc).
- desktop harness is being edited live (P1b: rightClick/textsOf/replaceText, Ui.Chat.Item) -> coordinate any shared-source refactor.
- prebuilt core exports cover all JNI glue calls of wt-uitest simplex-api.c (nm check: none missing).
