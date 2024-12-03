package chat.simplex.common.platform

import androidx.compose.animation.core.*
import androidx.compose.foundation.ScrollState
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.common.model.ChatId
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.ui.theme.CurrentColors
import kotlinx.coroutines.Job
import java.io.Closeable

interface PlatformInterface {
  suspend fun androidServiceStart() {}
  fun androidServiceSafeStop() {}
  fun androidCallServiceSafeStop() {}
  fun androidNotificationsModeChanged(mode: NotificationsMode) {}
  fun androidChatStartedAfterBeingOff() {}
  fun androidChatStopped() {}
  fun androidChatInitializedAndStarted() {}
  fun androidIsBackgroundCallAllowed(): Boolean = true
  fun androidSetNightModeIfSupported() {}
  fun androidSetStatusAndNavigationBarAppearance(isLightStatusBar: Boolean, isLightNavBar: Boolean, blackNavBar: Boolean = false, themeBackgroundColor: Color = CurrentColors.value.colors.background) {}
  fun androidStartCallActivity(acceptCall: Boolean, remoteHostId: Long? = null, chatId: ChatId? = null) {}
  fun androidPictureInPictureAllowed(): Boolean = true
  fun androidCallEnded() {}
  fun androidRestartNetworkObserver() {}
  fun androidCreateActiveCallState(): Closeable = Closeable { }
  fun androidIsXiaomiDevice(): Boolean = false
  val androidApiLevel: Int? get() = null
  @Composable fun androidLockPortraitOrientation() {}
  suspend fun androidAskToAllowBackgroundCalls(): Boolean = true
  @Composable fun desktopShowAppUpdateNotice() {}
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
