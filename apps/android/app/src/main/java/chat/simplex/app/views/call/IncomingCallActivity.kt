package chat.simplex.app.views.call

import android.app.KeyguardManager
import android.app.KeyguardManager.KeyguardLock
import android.content.*
import android.content.res.Configuration
import android.os.Build
import android.os.Bundle
import android.util.Log
import android.view.WindowManager
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Contact
import chat.simplex.app.model.NtfManager.Companion.OpenChatAction
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ProfileImage
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.onboarding.SimpleXLogo

class IncomingCallActivity: ComponentActivity() {
  private val vm by viewModels<SimplexViewModel>()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    val activity = this
    setContent { IncomingCallActivityView(vm.chatModel, activity) }
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

fun getKeyguardManager(context: Context): KeyguardManager =
  context.getSystemService(Context.KEYGUARD_SERVICE) as KeyguardManager

@Composable
fun IncomingCallActivityView(m: ChatModel, activity: IncomingCallActivity) {
  val switchingCall = m.switchingCall.value
  val invitation = m.activeCallInvitation.value
  val call = m.activeCall.value
  val showCallView = m.showCallView.value
  LaunchedEffect(invitation, call, switchingCall, showCallView) {
    if (!switchingCall && invitation == null && (!showCallView || call == null)) {
      Log.d(TAG, "IncomingCallActivityView: finishing activity")
      activity.finish()
    }
  }
  SimpleXTheme {
    Surface(
      Modifier
        .background(MaterialTheme.colors.background)
        .fillMaxSize()) {
      if (showCallView) {
        Box {
          ActiveCallView(m)
          if (invitation != null) IncomingCallAlertView(invitation, m)
        }
      } else if (invitation != null) {
        IncomingCallLockScreenAlert(invitation, m, activity)
      }
    }
  }
}

@Composable
fun IncomingCallLockScreenAlert(invitation: CallInvitation, chatModel: ChatModel, activity: IncomingCallActivity) {
  val cm = chatModel.callManager
  val cxt = LocalContext.current
  val scope = rememberCoroutineScope()
  var acceptCallsFromLockScreen by remember { mutableStateOf(chatModel.controller.acceptCallsFromLockScreen.get()) }
  LaunchedEffect(true) { SoundPlayer.shared.start(cxt, scope, sound = true) }
  DisposableEffect(true) { onDispose { SoundPlayer.shared.stop() } }
  IncomingCallLockScreenAlertLayout(
    invitation,
    acceptCallsFromLockScreen,
    rejectCall = { cm.endCall(invitation = invitation) },
    ignoreCall = { chatModel.activeCallInvitation.value = null },
    acceptCall = { cm.acceptIncomingCall(invitation = invitation) },
    openApp = {
      SoundPlayer.shared.stop()
      var intent = Intent(activity, MainActivity::class.java)
        .setAction(OpenChatAction)
        .putExtra("chatId", invitation.contact.id)
      activity.startActivity(intent)
      activity.finish()
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        getKeyguardManager(activity).requestDismissKeyguard(activity, null)
      }
    }
  )
}

@Composable
fun IncomingCallLockScreenAlertLayout(
  invitation: CallInvitation,
  acceptCallsFromLockScreen: Boolean,
  rejectCall: () -> Unit,
  ignoreCall: () -> Unit,
  acceptCall: () -> Unit,
  openApp: () -> Unit
) {
  Column(
    Modifier
      .padding(30.dp)
      .fillMaxSize(),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    IncomingCallInfo(invitation)
    Spacer(Modifier.fillMaxHeight().weight(1f))
    if (acceptCallsFromLockScreen) {
      ProfileImage(size = 192.dp, image = invitation.contact.profile.image)
      Text(invitation.contact.chatViewName, style = MaterialTheme.typography.h2)
      Spacer(Modifier.fillMaxHeight().weight(1f))
      Row {
        LockScreenCallButton(stringResource(R.string.reject), Icons.Filled.CallEnd, Color.Red, rejectCall)
        Spacer(Modifier.size(48.dp))
        LockScreenCallButton(stringResource(R.string.ignore), Icons.Filled.Close, MaterialTheme.colors.primary, ignoreCall)
        Spacer(Modifier.size(48.dp))
        LockScreenCallButton(stringResource(R.string.accept), Icons.Filled.Check, SimplexGreen, acceptCall)
      }
    } else {
      SimpleXLogo()
      Text(stringResource(R.string.open_simplex_chat_to_accept_call), textAlign = TextAlign.Center, lineHeight = 22.sp)
      Text(stringResource(R.string.allow_accepting_calls_from_lock_screen), textAlign = TextAlign.Center, style = MaterialTheme.typography.body2, lineHeight = 22.sp)
      Spacer(Modifier.fillMaxHeight().weight(1f))
      SimpleButton(text = stringResource(R.string.open_verb), icon = Icons.Filled.Check, click = openApp)
    }
  }
}

@Composable
private fun LockScreenCallButton(text: String, icon: ImageVector, color: Color, action: () -> Unit) {
  Surface(
    shape = RoundedCornerShape(10.dp),
    color = Color.Transparent
  ) {
    Column(
      Modifier
        .defaultMinSize(minWidth = 50.dp)
        .padding(4.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      IconButton(action) {
        Icon(icon, text, tint = color, modifier = Modifier.scale(1.75f))
      }
      Spacer(Modifier.height(16.dp))
      Text(text, style = MaterialTheme.typography.body2, color = HighOrLowlight)
    }
  }
}

@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true
)
@Composable
fun PreviewIncomingCallLockScreenAlert() {
  SimpleXTheme(true) {
    Surface(
      Modifier
        .background(MaterialTheme.colors.background)
        .fillMaxSize()) {
      IncomingCallLockScreenAlertLayout(
        invitation = CallInvitation(
          contact = Contact.sampleData,
          peerMedia = CallMediaType.Audio,
          sharedKey = null
        ),
        acceptCallsFromLockScreen = false,
        rejectCall = {},
        ignoreCall = {},
        acceptCall = {},
        openApp = {},
      )
    }
  }
}
