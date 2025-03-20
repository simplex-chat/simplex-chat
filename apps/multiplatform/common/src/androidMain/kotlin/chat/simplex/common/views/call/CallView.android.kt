package chat.simplex.common.views.call

import SectionSpacer
import SectionView
import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.*
import android.content.pm.PackageManager
import android.media.*
import android.os.Build
import android.os.PowerManager
import android.os.PowerManager.PROXIMITY_SCREEN_OFF_WAKE_LOCK
import android.os.PowerManager.WakeLock
import android.view.View
import android.view.ViewGroup
import android.webkit.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import androidx.lifecycle.*
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat
import chat.simplex.common.helpers.applyAppLocale
import chat.simplex.common.helpers.showAllowPermissionInSettingsAlert
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import com.google.accompanist.permissions.*
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filterNotNull
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.io.Closeable

// Should be destroy()'ed and set as null when call is ended. Otherwise, it will be a leak
@SuppressLint("StaticFieldLeak")
private var staticWebView: WebView? = null

// WebView methods must be called on Main thread
fun activeCallDestroyWebView() = withApi {
  // Stop it when call ended
  platform.androidCallServiceSafeStop()
  staticWebView?.destroy()
  staticWebView = null
  Log.d(TAG, "CallView: webview was destroyed")
}

class ActiveCallState: Closeable {
  val proximityLock: WakeLock? = screenOffWakeLock()
  var wasConnected = false
  val callAudioDeviceManager = CallAudioDeviceManagerInterface.new()
  private var closed = false

  init {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      callAudioDeviceManager.start()
    }
  }

  override fun close() {
    if (closed) return
    closed = true
    CallSoundsPlayer.stop()
    if (wasConnected) {
      CallSoundsPlayer.vibrate()
    }
    callAudioDeviceManager.stop()
    dropAudioManagerOverrides()
    if (proximityLock?.isHeld == true) {
      proximityLock.release()
    }
  }

  private fun screenOffWakeLock(): WakeLock? {
    val pm = (androidAppContext.getSystemService(Context.POWER_SERVICE) as PowerManager)
    return if (pm.isWakeLockLevelSupported(PROXIMITY_SCREEN_OFF_WAKE_LOCK)) {
      pm.newWakeLock(PROXIMITY_SCREEN_OFF_WAKE_LOCK, androidAppContext.packageName + ":proximityLock")
    } else {
      null
    }
  }
}


@SuppressLint("SourceLockedOrientationActivity")
@Composable
actual fun ActiveCallView() {
  val call = remember { chatModel.activeCall }.value
  val callState = call?.androidCallState as ActiveCallState?
  val scope = rememberCoroutineScope()
  LaunchedEffect(call) {
    if (call?.callState == CallState.Connected && callState != null && !callState.wasConnected) {
      CallSoundsPlayer.vibrate(2)
      callState.wasConnected = true
    }
  }
  LaunchedEffect(callState, chatModel.activeCallViewIsCollapsed.value) {
    callState ?: return@LaunchedEffect
    if (chatModel.activeCallViewIsCollapsed.value) {
      if (callState.proximityLock?.isHeld == true) callState.proximityLock.release()
    } else {
      delay(1000)
      if (callState.proximityLock?.isHeld == false) callState.proximityLock.acquire()
    }
  }
  Box(Modifier.fillMaxSize()) {
    WebRTCView(chatModel.callCommand) { apiMsg ->
      Log.d(TAG, "received from WebRTCView: $apiMsg")
      val call = chatModel.activeCall.value
      if (call != null) {
        val callState = call.androidCallState as ActiveCallState
        Log.d(TAG, "has active call $call")
        val callRh = call.remoteHostId
        when (val r = apiMsg.resp) {
          is WCallResponse.Capabilities -> withBGApi {
            val callType = CallType(call.initialCallType, r.capabilities)
            chatModel.controller.apiSendCallInvitation(callRh, call.contact, callType)
            updateActiveCall(call) { it.copy(callState = CallState.InvitationSent, localCapabilities = r.capabilities) }
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.S) {
              // Starting is delayed to make Android <= 11 working good with Bluetooth
              callState.callAudioDeviceManager.start()
            } else {
              callState.callAudioDeviceManager.selectLastExternalDeviceOrDefault(call.hasVideo, true)
            }
            CallSoundsPlayer.startConnectingCallSound(scope)
            activeCallWaitDeliveryReceipt(scope)
          }
          is WCallResponse.Offer -> withBGApi {
            chatModel.controller.apiSendCallOffer(callRh, call.contact, r.offer, r.iceCandidates, call.initialCallType, r.capabilities)
            updateActiveCall(call) { it.copy(callState = CallState.OfferSent, localCapabilities = r.capabilities) }
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.S) {
              // Starting is delayed to make Android <= 11 working good with Bluetooth
              callState.callAudioDeviceManager.start()
            } else {
              callState.callAudioDeviceManager.selectLastExternalDeviceOrDefault(call.hasVideo, true)
            }
          }
          is WCallResponse.Answer -> withBGApi {
            chatModel.controller.apiSendCallAnswer(callRh, call.contact, r.answer, r.iceCandidates)
            updateActiveCall(call) { it.copy(callState = CallState.Negotiated) }
            CallSoundsPlayer.stop()
          }
          is WCallResponse.Ice -> withBGApi {
            chatModel.controller.apiSendCallExtraInfo(callRh, call.contact, r.iceCandidates)
          }
          is WCallResponse.Connection ->
            try {
              val callStatus = json.decodeFromString<WebRTCCallStatus>("\"${r.state.connectionState}\"")
              if (callStatus == WebRTCCallStatus.Connected) {
                updateActiveCall(call) { it.copy(callState = CallState.Connected, connectedAt = Clock.System.now()) }
              }
              withBGApi { chatModel.controller.apiCallStatus(callRh, call.contact, callStatus) }
            } catch (e: Throwable) {
              Log.d(TAG,"call status ${r.state.connectionState} not used")
            }
          is WCallResponse.Connected -> {
            updateActiveCall(call) { it.copy(callState = CallState.Connected, connectionInfo = r.connectionInfo) }
          }
          is WCallResponse.PeerMedia -> {
            updateActiveCall(call) {
              val sources = it.peerMediaSources
              when (r.source) {
                CallMediaSource.Mic -> it.copy(peerMediaSources = sources.copy(mic = r.enabled))
                CallMediaSource.Camera -> it.copy(peerMediaSources = sources.copy(camera = r.enabled))
                CallMediaSource.ScreenAudio -> it.copy(peerMediaSources = sources.copy(screenAudio = r.enabled))
                CallMediaSource.ScreenVideo -> it.copy(peerMediaSources = sources.copy(screenVideo = r.enabled))
              }
            }
          }
          is WCallResponse.End -> {
            withBGApi { chatModel.callManager.endCall(call) }
          }
          is WCallResponse.Ended -> {
            updateActiveCall(call) { it.copy(callState = CallState.Ended) }
            withBGApi { chatModel.callManager.endCall(call) }
          }
          is WCallResponse.Ok -> when (val cmd = apiMsg.command) {
            is WCallCommand.Answer ->
              updateActiveCall(call) { it.copy(callState = CallState.Negotiated) }
            is WCallCommand.Media -> {
              updateActiveCall(call) {
                val sources = it.localMediaSources
                when (cmd.source) {
                  CallMediaSource.Mic -> {
                    val am = androidAppContext.getSystemService(Context.AUDIO_SERVICE) as AudioManager
                    am.isMicrophoneMute = !cmd.enable
                    it.copy(localMediaSources = sources.copy(mic = cmd.enable))
                  }
                  CallMediaSource.Camera -> it.copy(localMediaSources = sources.copy(camera = cmd.enable))
                  CallMediaSource.ScreenAudio -> it.copy(localMediaSources = sources.copy(screenAudio = cmd.enable))
                  CallMediaSource.ScreenVideo -> it.copy(localMediaSources = sources.copy(screenVideo = cmd.enable))
                }
              }
            }
            is WCallCommand.Camera -> {
              updateActiveCall(call) { it.copy(localCamera = cmd.camera) }
              if (!call.localMediaSources.mic) {
                chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.Mic, enable = false))
              }
            }
            is WCallCommand.End -> {
              withBGApi { chatModel.callManager.endCall(call) }
            }
            else -> {}
          }
          is WCallResponse.Error -> {
            Log.e(TAG, "ActiveCallView: command error ${r.message}")
          }
        }
      }
    }
    val showOverlay = when {
      call == null -> false
      !platform.androidPictureInPictureAllowed() -> true
      !chatModel.activeCallViewIsCollapsed.value -> true
      else -> false
    }
    if (call != null && showOverlay && callState != null) {
      ActiveCallOverlay(call, chatModel, callState.callAudioDeviceManager)
    }
  }
  KeyChangeEffect(callState, call?.localMediaSources?.hasVideo) {
    if (call != null && call.hasVideo && callState != null && callState.callAudioDeviceManager.currentDevice.value?.type == AudioDeviceInfo.TYPE_BUILTIN_EARPIECE) {
      // enabling speaker on user action (peer action ignored) and not disabling it again
      callState.callAudioDeviceManager.selectLastExternalDeviceOrDefault(call.hasVideo, true)
    }
  }
  val context = LocalContext.current
  DisposableEffect(Unit) {
    val activity = context as? Activity ?: return@DisposableEffect onDispose {}
    val prevVolumeControlStream = activity.volumeControlStream
    activity.volumeControlStream = AudioManager.STREAM_VOICE_CALL
    chatModel.activeCallViewIsVisible.value = true
    // After the first call, End command gets added to the list which prevents making another calls
    chatModel.callCommand.removeAll { it is WCallCommand.End }
    keepScreenOn(true)
    onDispose {
      activity.volumeControlStream = prevVolumeControlStream
      chatModel.activeCallViewIsVisible.value = false
      chatModel.callCommand.clear()
      keepScreenOn(false)
    }
  }
}

@Composable
private fun ActiveCallOverlay(call: Call, chatModel: ChatModel, callAudioDeviceManager: CallAudioDeviceManagerInterface) {
  ActiveCallOverlayLayout(
    call = call,
    devices = remember(callAudioDeviceManager) { callAudioDeviceManager.devices }.value,
    currentDevice = remember(callAudioDeviceManager) { callAudioDeviceManager.currentDevice },
    dismiss = { withBGApi { chatModel.callManager.endCall(call) } },
    toggleAudio = { chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.Mic, enable = !call.localMediaSources.mic)) },
    selectDevice = { callAudioDeviceManager.selectDevice(it.id) },
    toggleVideo = {
      if (ContextCompat.checkSelfPermission(androidAppContext, Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED) {
        chatModel.callCommand.add(WCallCommand.Media(CallMediaSource.Camera, enable = !call.localMediaSources.camera))
      } else {
        updateActiveCall(call) { it.copy(wantsToEnableCamera = true) }
      }
    },
    toggleSound = {
      val enableSpeaker = callAudioDeviceManager.currentDevice.value?.type == AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
      val preferredInternalDevice = callAudioDeviceManager.devices.value.firstOrNull { it.type == if (enableSpeaker) AudioDeviceInfo.TYPE_BUILTIN_SPEAKER else AudioDeviceInfo.TYPE_BUILTIN_EARPIECE }
      if (preferredInternalDevice != null) {
        callAudioDeviceManager.selectDevice(preferredInternalDevice.id)
      }
    },
    flipCamera = { chatModel.callCommand.add(WCallCommand.Camera(call.localCamera.flipped)) }
  )
}

@Composable
fun ActiveCallOverlayDisabled(call: Call) {
  ActiveCallOverlayLayout(
    call = call,
    devices = emptyList(),
    currentDevice = remember { mutableStateOf(null) },
    enabled = false,
    dismiss = {},
    toggleAudio = {},
    selectDevice = {},
    toggleVideo = {},
    toggleSound = {},
    flipCamera = {}
  )
}

private fun dropAudioManagerOverrides() {
  val am = androidAppContext.getSystemService(Context.AUDIO_SERVICE) as AudioManager
  am.mode = AudioManager.MODE_NORMAL
  // Clear selected communication device to default value after we changed it in call
  if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
    am.clearCommunicationDevice()
  } else {
    am.isSpeakerphoneOn = false
    am.stopBluetoothSco()
  }
}

@Composable
private fun ActiveCallOverlayLayout(
  call: Call,
  devices: List<AudioDeviceInfo>,
  currentDevice: State<AudioDeviceInfo?>,
  enabled: Boolean = true,
  dismiss: () -> Unit,
  toggleAudio: () -> Unit,
  selectDevice: (AudioDeviceInfo) -> Unit,
  toggleVideo: () -> Unit,
  toggleSound: () -> Unit,
  flipCamera: () -> Unit
) {
  Column {
    CallAppBar(
      title = {
        if (call.hasVideo) {
          Text(call.contact.chatViewName, Modifier.offset(x = (-4).dp).padding(end = DEFAULT_PADDING), color = Color(0xFFFFFFD8), style = MaterialTheme.typography.h2, overflow = TextOverflow.Ellipsis, maxLines = 1)
        }
      },
      onBack = { chatModel.activeCallViewIsCollapsed.value = true }
    )
    Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
      @Composable
      fun SelectSoundDevice(size: Dp) {
        if (devices.size == 2 &&
          devices.all { it.type == AudioDeviceInfo.TYPE_BUILTIN_EARPIECE || it.type == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER } ||
          currentDevice.value == null ||
          devices.none { it.id == currentDevice.value?.id }
        ) {
          val isSpeaker = currentDevice.value?.type == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER
          ToggleSoundButton(enabled, isSpeaker, !call.peerMediaSources.mic, toggleSound, size = size)
        } else {
          ExposedDropDownSettingWithIcon(
            devices.map { Triple(it, if (call.peerMediaSources.mic) it.icon else MR.images.ic_volume_off, if (it.name != null) generalGetString(it.name!!) else it.productName.toString()) },
            currentDevice,
            fontSize = 18.sp,
            boxSize = size,
            listIconSize = 30.dp,
            iconColor = Color(0xFFFFFFD8),
            background = controlButtonsBackground(),
            minWidth = 300.dp,
            onSelected = {
              if (it != null) {
                selectDevice(it)
              }
            }
          )
        }
      }

      when (call.hasVideo) {
        true -> VideoCallInfoView(call)
        false -> {
          Spacer(Modifier.fillMaxHeight().weight(1f))
          Column(
            Modifier.fillMaxWidth(),
            horizontalAlignment = Alignment.CenterHorizontally,
            verticalArrangement = Arrangement.Center
          ) {
            ProfileImage(size = 192.dp, image = call.contact.profile.image)
            AudioCallInfoView(call)
          }
        }
      }
      Box(Modifier.fillMaxWidth().fillMaxHeight().weight(1f), contentAlignment = Alignment.BottomCenter) {
        DisabledBackgroundCallsButton()
      }

      BoxWithConstraints(Modifier.padding(start = 6.dp, end = 6.dp, bottom = DEFAULT_PADDING).align(Alignment.CenterHorizontally)) {
        val size = ((maxWidth - DEFAULT_PADDING_HALF * 4) / 5).coerceIn(0.dp, 60.dp)
        // limiting max width for tablets/wide screens, will be displayed in the center
        val padding = ((min(420.dp, maxWidth) - size * 5) / 4).coerceAtLeast(0.dp)
        Row(horizontalArrangement = Arrangement.spacedBy(padding), verticalAlignment = Alignment.CenterVertically) {
          ToggleMicButton(call, enabled, toggleAudio, size = size)
          SelectSoundDevice(size = size)
          ControlButton(painterResource(MR.images.ic_call_end_filled), MR.strings.icon_descr_hang_up, enabled = enabled, dismiss, background = Color.Red, size = size, iconPaddingPercent = 0.166f)
          if (call.localMediaSources.camera) {
            ControlButton(painterResource(MR.images.ic_flip_camera_android_filled), MR.strings.icon_descr_flip_camera, enabled, flipCamera, size = size)
            ControlButton(painterResource(MR.images.ic_videocam_filled), MR.strings.icon_descr_video_off, enabled, toggleVideo, size = size)
          } else {
            Spacer(Modifier.size(size))
            ControlButton(painterResource(MR.images.ic_videocam_off), MR.strings.icon_descr_video_on, enabled, toggleVideo, size = size)
          }
        }
      }
    }
  }
}

@Composable
private fun ControlButton(icon: Painter, iconText: StringResource, enabled: Boolean = true, action: () -> Unit, background: Color = controlButtonsBackground(), size: Dp, iconPaddingPercent: Float = 0.2f) {
  ControlButtonWrap(enabled, action, background, size) {
    Icon(icon, stringResource(iconText), tint = if (enabled) Color(0xFFFFFFD8) else MaterialTheme.colors.secondary, modifier = Modifier.padding(size * iconPaddingPercent).fillMaxSize())
  }
}

@Composable
private fun ControlButtonWrap(enabled: Boolean = true, action: () -> Unit, background: Color = controlButtonsBackground(), size: Dp, content: @Composable () -> Unit) {
  val ripple = remember { ripple(bounded = false, radius = size / 2, color = background.lighter(0.1f)) }
  Box(
    Modifier
      .background(background, CircleShape)
      .size(size)
      .clickable(
        onClick = action,
        role = Role.Button,
        interactionSource = remember { MutableInteractionSource() },
        indication = ripple,
        enabled = enabled
      ),
    contentAlignment = Alignment.Center
  ) {
    content()
  }
}

@Composable
private fun ToggleMicButton(call: Call, enabled: Boolean = true, toggleAudio: () -> Unit, size: Dp) {
  if (call.localMediaSources.mic) {
    ControlButton(painterResource(MR.images.ic_mic), MR.strings.icon_descr_audio_off, enabled, toggleAudio, size = size)
  } else {
    ControlButton(painterResource(MR.images.ic_mic_off), MR.strings.icon_descr_audio_on, enabled, toggleAudio, size = size)
  }
}

@Composable
private fun ToggleSoundButton(enabled: Boolean, speaker: Boolean, muted: Boolean, toggleSound: () -> Unit, size: Dp) {
  when {
    muted -> ControlButton(painterResource(MR.images.ic_volume_off), MR.strings.icon_descr_sound_muted, enabled, toggleSound, size = size)
    speaker -> ControlButton(painterResource(MR.images.ic_volume_up), MR.strings.icon_descr_speaker_off, enabled, toggleSound, size = size)
    else -> ControlButton(painterResource(MR.images.ic_volume_down), MR.strings.icon_descr_speaker_on, enabled, toggleSound, size = size)
  }
}

@Composable
fun controlButtonsBackground(): Color = if (chatModel.activeCall.value?.peerMediaSources?.hasVideo == true) Color.Black.copy(0.2f) else Color.White.copy(0.2f)

@Composable
fun AudioCallInfoView(call: Call) {
  Column(horizontalAlignment = Alignment.CenterHorizontally) {
    InfoText(call.contact.chatViewName, style = MaterialTheme.typography.h2)
    InfoText(call.callState.text)

    val connInfo = call.connectionInfo
    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text})"
    InfoText(call.encryptionStatus + connInfoText)
  }
}

@Composable
fun VideoCallInfoView(call: Call) {
  Column(horizontalAlignment = Alignment.Start) {
    InfoText(call.callState.text)

    val connInfo = call.connectionInfo
    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text})"
    InfoText(call.encryptionStatus + connInfoText)
  }
}

@Composable
fun InfoText(text: String, modifier: Modifier = Modifier, style: TextStyle = MaterialTheme.typography.body2) =
  Text(text, modifier, color = Color(0xFFFFFFD8), style = style)

@Composable
private fun DisabledBackgroundCallsButton() {
  var show by remember { mutableStateOf(!platform.androidIsBackgroundCallAllowed()) }
  if (show) {
    Row(
      Modifier
        .padding(bottom = 24.dp)
        .clickable {
          withLongRunningApi {
            show = !platform.androidAskToAllowBackgroundCalls()
          }
        }
        .background(WarningOrange.copy(0.3f), RoundedCornerShape(50))
        .padding(start = 14.dp, top = 4.dp, end = 8.dp, bottom = 4.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(stringResource(MR.strings.system_restricted_background_in_call_title), color = WarningOrange)
      Spacer(Modifier.width(8.dp))
      IconButton(onClick = { show = false }, Modifier.size(22.dp)) {
        Icon(painterResource(MR.images.ic_close), null, tint = WarningOrange)
      }
    }
  }
}

//@Composable
//fun CallViewDebug(close: () -> Unit) {
//  val callCommand = remember { mutableStateOf<WCallCommand?>(null)}
//  val commandText = remember { mutableStateOf("{\"command\": {\"type\": \"start\", \"media\": \"video\", \"aesKey\": \"FwW+t6UbnwHoapYOfN4mUBUuqR7UtvYWxW16iBqM29U=\"}}") }
//  val clipboard = ContextCompat.getSystemService(LocalContext.current, ClipboardManager::class.java)
//
//  BackHandler(onBack = close)
//  Column(
//    horizontalAlignment = Alignment.CenterHorizontally,
//    verticalArrangement = Arrangement.spacedBy(12.dp),
//    modifier = Modifier
//      .themedBackground()
//      .fillMaxSize()
//  ) {
//    WebRTCView(callCommand) { apiMsg ->
//      // for debugging
//      // commandText.value = apiMsg
//      commandText.value = json.encodeToString(apiMsg)
//    }
//
//    TextEditor(Modifier.height(180.dp), text = commandText)
//
//    Row(
//      Modifier
//        .fillMaxWidth()
//        .padding(bottom = 6.dp),
//      horizontalArrangement = Arrangement.SpaceBetween
//    ) {
//      Button(onClick = {
//        val clip: ClipData = ClipData.newPlainText("js command", commandText.value)
//        clipboard?.setPrimaryClip(clip)
//      }) { Text("Copy") }
//      Button(onClick = {
//        try {
//          val apiCall: WVAPICall = json.decodeFromString(commandText.value)
//          commandText.value = ""
//          println("sending: ${commandText.value}")
//          callCommand.value = apiCall.command
//        } catch(e: Error) {
//          println("error parsing command: ${commandText.value}")
//          println(e)
//        }
//      }) { Text("Send") }
//      Button(onClick = {
//        commandText.value = ""
//      }) { Text("Clear") }
//    }
//  }
//}

@Composable
fun CallPermissionsView(pipActive: Boolean, hasVideo: Boolean, cancel: () -> Unit) {
  val audioPermission = rememberPermissionState(Manifest.permission.RECORD_AUDIO)
  val cameraPermission = rememberPermissionState(Manifest.permission.CAMERA)
  val permissionsState = rememberMultiplePermissionsState(
    permissions = if (hasVideo) {
      listOf(Manifest.permission.CAMERA, Manifest.permission.RECORD_AUDIO)
    } else {
      listOf(Manifest.permission.RECORD_AUDIO)
    }
  )
  val context = LocalContext.current
  val buttonEnabled = remember { mutableStateOf(true) }
  LaunchedEffect(Unit) {
    if (!pipActive) {
      permissionsState.launchMultiplePermissionRequestWithFallback(buttonEnabled, context::showAllowPermissionInSettingsAlert)
    }
  }

  if (pipActive) {
    Column(Modifier.fillMaxSize(), horizontalAlignment = Alignment.CenterHorizontally, verticalArrangement = Arrangement.SpaceEvenly) {
      if (audioPermission.status is PermissionStatus.Denied) {
        Icon(
          painterResource(MR.images.ic_call_500),
          stringResource(MR.strings.permissions_record_audio),
          Modifier.size(22.dp),
          tint = Color(0xFFFFFFD8)
        )
      }
      if (hasVideo && cameraPermission.status is PermissionStatus.Denied) {
        Icon(
          painterResource(MR.images.ic_videocam),
          stringResource(MR.strings.permissions_camera),
          Modifier.size(22.dp),
          tint = Color(0xFFFFFFD8)
        )
      }
    }
  } else {
    ModalView(background = Color.Black, showAppBar = false, close = {}) {
      Column {
        Spacer(Modifier.height(AppBarHeight * fontSizeSqrtMultiplier))
        AppBarTitle(stringResource(MR.strings.permissions_required))
        Spacer(Modifier.weight(1f))
        val onClick = {
          if (permissionsState.shouldShowRationale) {
            context.showAllowPermissionInSettingsAlert()
          } else {
            permissionsState.launchMultiplePermissionRequestWithFallback(buttonEnabled, context::showAllowPermissionInSettingsAlert)
          }
        }
        Text(stringResource(MR.strings.permissions_grant), Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING), textAlign = TextAlign.Center, color = Color(0xFFFFFFD8))
        SectionSpacer()
        SectionView {
          Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
            val text = if (hasVideo && audioPermission.status is PermissionStatus.Denied && cameraPermission.status is PermissionStatus.Denied) {
              stringResource(MR.strings.permissions_camera_and_record_audio)
            } else if (audioPermission.status is PermissionStatus.Denied) {
              stringResource(MR.strings.permissions_record_audio)
            } else if (hasVideo && cameraPermission.status is PermissionStatus.Denied) {
              stringResource(MR.strings.permissions_camera)
            } else null
            if (text != null) {
              GrantPermissionButton(text, buttonEnabled.value, onClick)
            }
          }
        }

        Spacer(Modifier.weight(1f))
        Box(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
          SimpleButtonFrame(cancel, Modifier.height(60.dp)) {
            Text(stringResource(MR.strings.call_service_notification_end_call), fontSize = 20.sp, color = Color(0xFFFFFFD8))
          }
        }
      }
    }
  }
}

@Composable
private fun GrantPermissionButton(text: String, enabled: Boolean, onClick: () -> Unit) {
  Row(
    Modifier
      .clickable(enabled = enabled, onClick = onClick)
      .heightIn(min = 30.dp)
      .background(WarningOrange.copy(0.3f), RoundedCornerShape(50)),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Text(text, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF), fontSize = 20.sp, color = WarningOrange)
  }
}

/**
 * The idea of this function is to ask system to show permission dialog and to see if it's really doing it.
 * Otherwise, show alert with a button that opens settings for manual permission granting
 * */
private fun MultiplePermissionsState.launchMultiplePermissionRequestWithFallback(buttonEnabled: MutableState<Boolean>, fallback: () -> Unit) {
  buttonEnabled.value = false
  val lifecycleOwner = ProcessLifecycleOwner.get().lifecycle
  var useFallback = true
  val observer = LifecycleEventObserver { _, event ->
    if (event == Lifecycle.Event.ON_PAUSE) {
      useFallback = false
      buttonEnabled.value = true
    }
  }
  lifecycleOwner.addObserver(observer)
  withBGApi {
    delay(2000)
    if (useFallback && chatModel.activeCall.value != null) {
      fallback()
    }
    buttonEnabled.value = true
  }.invokeOnCompletion {
    // Main thread only
    withApi {
      lifecycleOwner.removeObserver(observer)
    }
  }
  launchMultiplePermissionRequest()
}

@Composable
fun WebRTCView(callCommand: SnapshotStateList<WCallCommand>, onResponse: (WVAPIMessage) -> Unit) {
  val webView = remember { mutableStateOf<WebView?>(null) }
  fun processCommand(wv: WebView, cmd: WCallCommand) {
    val apiCall = WVAPICall(command = cmd)
    wv.evaluateJavascript("processCommand(${json.encodeToString(apiCall)})", null)
  }
  DisposableEffect(Unit) {
    onDispose {
      //      val wv = webView.value
      //      if (wv != null) processCommand(wv, WCallCommand.End)
      //      webView.value?.destroy()
      webView.value = null
    }
  }
  val wv = webView.value
  if (wv != null) {
    LaunchedEffect(Unit) {
      snapshotFlow { callCommand.firstOrNull() }
        .distinctUntilChanged()
        .filterNotNull()
        .collect {
          while (callCommand.isNotEmpty()) {
            val cmd = callCommand.removeFirst()
            Log.d(TAG, "WebRTCView LaunchedEffect executing $cmd")
            processCommand(wv, cmd)
          }
        }
    }
  }
  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/www/", WebViewAssetLoader.AssetsPathHandler(LocalContext.current))
    .build()

  Box(Modifier.fillMaxSize()) {
    AndroidView(
      factory = {
        try {
          (staticWebView ?: WebView(androidAppContext)).apply {
            reapplyLocale()
            layoutParams = ViewGroup.LayoutParams(
              ViewGroup.LayoutParams.MATCH_PARENT,
              ViewGroup.LayoutParams.MATCH_PARENT,
            )
            this.webChromeClient = object: WebChromeClient() {
              override fun onPermissionRequest(request: PermissionRequest) {
                if (request.origin.toString().startsWith("file:/")) {
                  request.grant(request.resources)
                } else {
                  Log.d(TAG, "Permission request from webview denied.")
                  request.deny()
                }
              }
            }
            this.webViewClient = LocalContentWebViewClient(webView, assetLoader)
            this.clearHistory()
            this.clearCache(true)
            this.addJavascriptInterface(WebRTCInterface(onResponse), "WebRTCInterface")
            this.setBackgroundColor(android.graphics.Color.BLACK)
            val webViewSettings = this.settings
            webViewSettings.allowFileAccess = true
            webViewSettings.allowContentAccess = true
            webViewSettings.javaScriptEnabled = true
            webViewSettings.mediaPlaybackRequiresUserGesture = false
            webViewSettings.cacheMode = WebSettings.LOAD_NO_CACHE
            if (staticWebView == null) {
              this.loadUrl("file:android_asset/www/android/call.html")
            } else {
              webView.value = this
            }
          }
        } catch (e: Exception) {
          Log.e(TAG, "Error initializing WebView: ${e.stackTraceToString()}")
          if (e.stackTraceToString().contains("/lib64")) {
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), generalGetString(MR.strings.error_initializing_web_view_wrong_arch).format(e.stackTraceToString()))
          } else {
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), generalGetString(MR.strings.error_initializing_web_view).format(e.stackTraceToString()))
          }
          return@AndroidView View(androidAppContext)
        }
      }
    ) { /* WebView */ }
  }
}

// for debugging
// class WebRTCInterface(private val onResponse: (String) -> Unit) {
class WebRTCInterface(private val onResponse: (WVAPIMessage) -> Unit) {
  @JavascriptInterface
  fun postMessage(message: String) {
    Log.d(TAG, "WebRTCInterface.postMessage")
    try {
      // for debugging
      // onResponse(message)
      onResponse(json.decodeFromString(message))
    } catch (e: Exception) {
      Log.e(TAG, "failed parsing WebView message: $message")
    }
  }
}

private fun updateActiveCall(initial: Call, transform: (Call) -> Call) {
  val activeCall = chatModel.activeCall.value
  if (activeCall != null && activeCall.contact.apiId == initial.contact.apiId) {
    chatModel.activeCall.value = transform(activeCall)
  } else {
    Log.d(TAG, "withActiveCall: ignoring, not in call with the contact ${activeCall?.contact?.id}")
  }
}

/*
* Creating WebView automatically drops user's custom app locale to default system locale.
* Preventing it by re-applying custom locale
* https://issuetracker.google.com/issues/109833940
* */
private fun reapplyLocale() {
  mainActivity.get()?.applyAppLocale(appPrefs.appLanguage)
  callActivity.get()?.applyAppLocale(appPrefs.appLanguage)
}

private class LocalContentWebViewClient(val webView: MutableState<WebView?>, private val assetLoader: WebViewAssetLoader) : WebViewClientCompat() {
  override fun shouldInterceptRequest(
    view: WebView,
    request: WebResourceRequest
  ): WebResourceResponse? {
    return assetLoader.shouldInterceptRequest(request.url)
  }

  override fun onPageFinished(view: WebView, url: String) {
    super.onPageFinished(view, url)
    view.evaluateJavascript("sendMessageToNative = (msg) => WebRTCInterface.postMessage(JSON.stringify(msg))", null)
    webView.value = view
    staticWebView = view
    Log.d(TAG, "WebRTCView: webview ready")
    // for debugging
    // view.evaluateJavascript("sendMessageToNative = ({resp}) => WebRTCInterface.postMessage(JSON.stringify({command: resp}))", null)
  }
}

@Preview
@Composable
fun PreviewActiveCallOverlayVideo() {
  SimpleXTheme {
    ActiveCallOverlayLayout(
      call = Call(
        remoteHostId = null,
        userProfile = Profile.sampleData,
        contact = Contact.sampleData,
        callState = CallState.Negotiated,
        initialCallType = CallMediaType.Video,
        peerMediaSources = CallMediaSources(),
        callUUID = "",
        connectionInfo = ConnectionInfo(
          RTCIceCandidate(RTCIceCandidateType.Host, "tcp"),
          RTCIceCandidate(RTCIceCandidateType.Host, "tcp")
        ),
        androidCallState = {}
      ),
      devices = emptyList(),
      currentDevice = remember { mutableStateOf(null) },
      dismiss = {},
      toggleAudio = {},
      selectDevice = {},
      toggleVideo = {},
      toggleSound = {},
      flipCamera = {},
    )
  }
}

@Preview
@Composable
fun PreviewActiveCallOverlayAudio() {
  SimpleXTheme {
    ActiveCallOverlayLayout(
      call = Call(
        remoteHostId = null,
        userProfile = Profile.sampleData,
        contact = Contact.sampleData,
        callState = CallState.Negotiated,
        initialCallType = CallMediaType.Audio,
        peerMediaSources = CallMediaSources(),
        callUUID = "",
        connectionInfo = ConnectionInfo(
          RTCIceCandidate(RTCIceCandidateType.Host, "udp"),
          RTCIceCandidate(RTCIceCandidateType.Host, "udp")
        ),
        androidCallState = {}
      ),
      devices = emptyList(),
      currentDevice = remember { mutableStateOf(null) },
      dismiss = {},
      toggleAudio = {},
      selectDevice = {},
      toggleVideo = {},
      toggleSound = {},
      flipCamera = {}
    )
  }
}
