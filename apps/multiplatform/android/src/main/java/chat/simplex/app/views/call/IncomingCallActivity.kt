package chat.simplex.app.views.call

import android.app.Activity
import android.app.KeyguardManager
import android.content.Context
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.view.WindowManager
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.common.model.*
import chat.simplex.app.model.NtfManager.OpenChatAction
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.datetime.Clock

class IncomingCallActivity: ComponentActivity() {

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    setContent { IncomingCallActivityView(ChatModel) }
    unlockForIncomingCall()
  }

  override fun onDestroy() {
    super.onDestroy()
    lockAfterIncomingCall()
  }

  private fun unlockForIncomingCall() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O_MR1) {
      setShowWhenLocked(true)
      setTurnScreenOn(true)
    } else {
      window.addFlags(activityFlags)
    }
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      getKeyguardManager(this).requestDismissKeyguard(this, null)
    }
  }

  private fun lockAfterIncomingCall() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O_MR1) {
      setShowWhenLocked(false)
      setTurnScreenOn(false)
    } else {
      window.clearFlags(activityFlags)
    }
  }

  companion object {
    const val activityFlags = WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON or WindowManager.LayoutParams.FLAG_ALLOW_LOCK_WHILE_SCREEN_ON
  }
}

@Composable
fun IncomingCallActivityView(m: ChatModel) {
  val switchingCall = m.switchingCall.value
  val invitation = m.activeCallInvitation.value
  val call = m.activeCall.value
  val showCallView = m.showCallView.value
  val activity = LocalContext.current as Activity
  LaunchedEffect(invitation, call, switchingCall, showCallView) {
    if (!switchingCall && invitation == null && (!showCallView || call == null)) {
      Log.d(TAG, "IncomingCallActivityView: finishing activity")
      activity.finish()
    }
  }
  SimpleXTheme {
    Surface(
      Modifier
        .fillMaxSize(),
      color = MaterialTheme.colors.background,
      contentColor = LocalContentColor.current
    ) {
      if (!showCallView && invitation != null) {
        IncomingCallLockScreenAlert(invitation, m)
      }
    }
  }
}



@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true
)*/
@Composable
fun PreviewIncomingCallLockScreenAlert() {
  SimpleXTheme(true) {
    Surface(
      Modifier
        .fillMaxSize(),
      color = MaterialTheme.colors.background,
      contentColor = LocalContentColor.current
    ) {
      IncomingCallLockScreenAlertLayout(
        invitation = RcvCallInvitation(
          remoteHostId = null,
          user = User.sampleData,
          contact = Contact.sampleData,
          callType = CallType(media = CallMediaType.Audio, capabilities = CallCapabilities(encryption = false)),
          sharedKey = null,
          callTs = Clock.System.now()
        ),
        callOnLockScreen = null,
        chatModel = SimplexApp.context.chatModel,
        rejectCall = {},
        ignoreCall = {},
        acceptCall = {},
        openApp = {},
      )
    }
  }
}
