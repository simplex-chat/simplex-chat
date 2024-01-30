package chat.simplex.app.views.call

import android.app.*
import android.content.Intent
import android.content.res.Configuration
import android.graphics.Rect
import android.os.Build
import android.os.Bundle
import android.util.Rational
import android.view.View
import android.view.WindowManager
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.trackPipAnimationHintView
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalView
import chat.simplex.app.MainActivity
import chat.simplex.app.TAG
import chat.simplex.app.model.NtfManager.AcceptCallAction
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.launch
import java.lang.ref.WeakReference

class CallActivity: ComponentActivity() {
  var relaunchingActivity = false

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    callActivity = WeakReference(this)
    when (intent?.action) {
      AcceptCallAction -> {
        val remoteHostId = intent.getLongExtra("remoteHostId", -1).takeIf { it != -1L }
        val chatId = intent.getStringExtra("chatId")
        val invitation = (chatModel.callInvitations.values + chatModel.activeCallInvitation.value).lastOrNull {
          it?.remoteHostId == remoteHostId && it?.contact?.id == chatId
        }
        if (invitation != null) {
          chatModel.callManager.acceptIncomingCall(invitation = invitation)
        }
      }
    }

    setContent { CallActivityView() }
  }

  fun setPipParams(video: Boolean, sourceRectHint: Rect? = null) {
    // By manually specifying source rect we exclude empty background while toggling PiP
    val builder = PictureInPictureParams.Builder()
        .setAspectRatio(Rational(100, 133))
        .setSourceRectHint(sourceRectHint)
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      builder.setAutoEnterEnabled(video)
    }
    setPictureInPictureParams(builder.build())
  }

  override fun onPictureInPictureModeChanged(isInPictureInPictureMode: Boolean, newConfig: Configuration) {
    super.onPictureInPictureModeChanged(isInPictureInPictureMode, newConfig)
    chatModel.activeCallViewIsCollapsed.value = isInPictureInPictureMode
    val layoutType = if (!isInPictureInPictureMode) {
      LayoutType.Default
    } else {
      LayoutType.RemoteVideo
    }
    chatModel.callCommand.add(WCallCommand.Layout(layoutType))
  }

  override fun onUserLeaveHint() {
    // On Android 12+ PiP is enabled automatically when a user hides the app
    if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.R && callSupportsVideo()) {
      enterPictureInPictureMode()
    }
  }

  override fun onStop() {
    super.onStop()
    val call = chatModel.activeCall.value
    if (call != null && chatModel.activeCallViewIsCollapsed.value && !relaunchingActivity) {
      withBGApi {
        chatModel.callManager.endCall(call)
      }
    }
    relaunchingActivity = false
  }

  companion object {
    const val activityFlags = WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON or WindowManager.LayoutParams.FLAG_ALLOW_LOCK_WHILE_SCREEN_ON
  }
}

private fun callSupportsVideo() = chatModel.activeCall.value?.supportsVideo() == true || chatModel.activeCallInvitation.value?.callType?.media == CallMediaType.Video

@Composable
fun CallActivityView() {
  val m = chatModel
  val switchingCall = m.switchingCall.value
  val invitation = m.activeCallInvitation.value
  val call = m.activeCall.value
  val showCallView = m.showCallView.value
  val activity = LocalContext.current as CallActivity
  LaunchedEffect(invitation, call, switchingCall, showCallView) {
    if (!switchingCall && invitation == null && (!showCallView || call == null)) {
      Log.d(TAG, "CallActivityView: finishing activity")
      activity.finish()
    }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { chatModel.activeCallViewIsCollapsed.value }
      .collect { collapsed ->
        when {
          collapsed -> {
            if (chatModel.activeCall.value?.supportsVideo() == true) {
              activity.enterPictureInPictureMode()
            } else {
              activity.relaunchingActivity = true
              activity.moveTaskToBack(true)
              //activity.startActivity(Intent(activity, MainActivity::class.java))
            }
          }
          activity.isInPictureInPictureMode -> {
            activity.moveTaskToBack(false)
            platform.androidStartCallActivity(false)
          }
        }
      }
  }
  SimpleXTheme {
    val view = LocalView.current
    Box(
      Modifier
        .fillMaxSize()
    ) {
      Box {
        ActiveCallView()
        if (invitation != null) IncomingCallAlertView(invitation, m)
      }
    }
    if (callSupportsVideo()) {
      val scope = rememberCoroutineScope()
      LaunchedEffect(Unit) {
        scope.launch {
          activity.setPipParams(callSupportsVideo())
          activity.trackPipAnimationHintView(view)
        }
      }
      //TrackViewSizeManually()
    }
  }
}

@Composable
private fun TrackViewSizeManually() {
  val activity = LocalContext.current as CallActivity
  val view = LocalView.current
  DisposableEffect(Unit) {
    val listener = View.OnLayoutChangeListener { _: View?, _: Int, _: Int, _: Int, _: Int, _: Int, _: Int, _: Int, _: Int ->
      val sourceRectHint = Rect()
      view.getGlobalVisibleRect(sourceRectHint)
      activity.setPipParams(callSupportsVideo(), sourceRectHint)
    }
    view.addOnLayoutChangeListener(listener)
    onDispose {
      view.removeOnLayoutChangeListener(listener)
    }
  }
}
