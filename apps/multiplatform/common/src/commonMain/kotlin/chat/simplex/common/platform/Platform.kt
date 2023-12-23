package chat.simplex.common.platform

import chat.simplex.common.model.NotificationsMode

interface PlatformInterface {
  suspend fun androidServiceStart() {}
  fun androidServiceSafeStop() {}
  fun androidNotificationsModeChanged(mode: NotificationsMode) {}
  fun androidChatStartedAfterBeingOff() {}
  fun androidChatStopped() {}
  fun androidChatInitializedAndStarted() {}
  fun androidIsBackgroundCallAllowed(): Boolean = true
  fun androidSetNightModeIfSupported() {}
  suspend fun androidAskToAllowBackgroundCalls(): Boolean = true
}
/**
 * Multiplatform project has separate directories per platform + common directory that contains directories per platform + common for all of them.
 * This means that we can not call code from `android` directory via code from `common/androidMain` directory. So this is a way to do it:
 * - we specify interface that should be implemented by platforms
 * - platforms made its implementation by assigning it to this variable at runtime
 * - common code calls this variable and everything works as expected.
 *
 * Functions that expected to be used on only one platform, should be prefixed with platform name, like androidSomething. It helps
 * to identify it's use-case. Easy to understand that it is only needed on one specific platform. Functions without prefixes are used on
 * more than one platform.
 *
 * See [SimplexApp] and [AppCommon.desktop] for re-assigning of this var
 * */
var platform: PlatformInterface = object : PlatformInterface {}
