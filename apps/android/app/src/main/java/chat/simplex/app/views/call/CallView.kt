package chat.simplex.app.views.call

import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.*
import android.content.pm.ActivityInfo
import android.media.*
import android.os.Build
import android.os.PowerManager
import android.os.PowerManager.PROXIMITY_SCREEN_OFF_WAKE_LOCK
import android.util.Log
import android.view.ViewGroup
import android.webkit.*
import androidx.activity.compose.BackHandler
import androidx.annotation.StringRes
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ProfileImage
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.usersettings.NotificationsMode
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.coroutines.*
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString

@SuppressLint("SourceLockedOrientationActivity")
@Composable
fun ActiveCallView(chatModel: ChatModel) {
  BackHandler(onBack = {
    val call = chatModel.activeCall.value
    if (call != null) withApi { chatModel.callManager.endCall(call) }
  })
  val audioViaBluetooth = rememberSaveable { mutableStateOf(false) }
  val ntfModeService = remember { chatModel.controller.appPrefs.notificationsMode.get() == NotificationsMode.SERVICE.name }
  LaunchedEffect(Unit) {
    // Start service when call happening since it's not already started.
    // It's needed to prevent Android from shutting down a microphone after a minute or so when screen is off
    if (!ntfModeService) SimplexService.start(SimplexApp.context)
  }
  DisposableEffect(Unit) {
    val am = SimplexApp.context.getSystemService(Context.AUDIO_SERVICE) as AudioManager
    var btDeviceCount = 0
    val audioCallback = object: AudioDeviceCallback() {
      override fun onAudioDevicesAdded(addedDevices: Array<out AudioDeviceInfo>) {
        Log.d(TAG, "Added audio devices: ${addedDevices.map { it.type }}")
        super.onAudioDevicesAdded(addedDevices)
        val addedCount = addedDevices.count { it.type == AudioDeviceInfo.TYPE_BLUETOOTH_SCO }
        btDeviceCount += addedCount
        audioViaBluetooth.value = btDeviceCount > 0
        if (addedCount > 0 && chatModel.activeCall.value?.callState == CallState.Connected) {
          // Setting params in Connected state makes sure that Bluetooth will NOT be broken on Android < 12
          setCallSound(chatModel.activeCall.value?.soundSpeaker ?: return, audioViaBluetooth)
        }
      }
      override fun onAudioDevicesRemoved(removedDevices: Array<out AudioDeviceInfo>) {
        Log.d(TAG, "Removed audio devices: ${removedDevices.map { it.type }}")
        super.onAudioDevicesRemoved(removedDevices)
        val removedCount = removedDevices.count { it.type == AudioDeviceInfo.TYPE_BLUETOOTH_SCO }
        btDeviceCount -= removedCount
        audioViaBluetooth.value = btDeviceCount > 0
        if (btDeviceCount == 0 && chatModel.activeCall.value?.callState == CallState.Connected) {
          // Setting params in Connected state makes sure that Bluetooth will NOT be broken on Android < 12
          setCallSound(chatModel.activeCall.value?.soundSpeaker ?: return, audioViaBluetooth)
        }
      }
    }
    am.registerAudioDeviceCallback(audioCallback, null)
    val pm = (SimplexApp.context.getSystemService(Context.POWER_SERVICE) as PowerManager)
    val proximityLock = if (pm.isWakeLockLevelSupported(PROXIMITY_SCREEN_OFF_WAKE_LOCK)) {
      pm.newWakeLock(PROXIMITY_SCREEN_OFF_WAKE_LOCK, SimplexApp.context.packageName + ":proximityLock")
    } else {
      null
    }
    proximityLock?.acquire()
    onDispose {
      // Stop it when call ended
      if (!ntfModeService) SimplexService.safeStopService(SimplexApp.context)
      dropAudioManagerOverrides()
      am.unregisterAudioDeviceCallback(audioCallback)
      proximityLock?.release()
    }
  }
  val scope = rememberCoroutineScope()
  Box(Modifier.fillMaxSize()) {
    WebRTCView(chatModel.callCommand) { apiMsg ->
      Log.d(TAG, "received from WebRTCView: $apiMsg")
      val call = chatModel.activeCall.value
      if (call != null) {
        Log.d(TAG, "has active call $call")
        when (val r = apiMsg.resp) {
          is WCallResponse.Capabilities -> withApi {
            val callType = CallType(call.localMedia, r.capabilities)
            chatModel.controller.apiSendCallInvitation(call.contact, callType)
            chatModel.activeCall.value = call.copy(callState = CallState.InvitationSent, localCapabilities = r.capabilities)
          }
          is WCallResponse.Offer -> withApi {
            chatModel.controller.apiSendCallOffer(call.contact, r.offer, r.iceCandidates, call.localMedia, r.capabilities)
            chatModel.activeCall.value = call.copy(callState = CallState.OfferSent, localCapabilities = r.capabilities)
          }
          is WCallResponse.Answer -> withApi {
            chatModel.controller.apiSendCallAnswer(call.contact, r.answer, r.iceCandidates)
            chatModel.activeCall.value = call.copy(callState = CallState.Negotiated)
          }
          is WCallResponse.Ice -> withApi {
            chatModel.controller.apiSendCallExtraInfo(call.contact, r.iceCandidates)
          }
          is WCallResponse.Connection ->
            try {
              val callStatus = json.decodeFromString<WebRTCCallStatus>("\"${r.state.connectionState}\"")
              if (callStatus == WebRTCCallStatus.Connected) {
                chatModel.activeCall.value = call.copy(callState = CallState.Connected)
                setCallSound(call.soundSpeaker, audioViaBluetooth)
              }
              withApi { chatModel.controller.apiCallStatus(call.contact, callStatus) }
            } catch (e: Error) {
              Log.d(TAG,"call status ${r.state.connectionState} not used")
            }
          is WCallResponse.Connected -> {
            chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectionInfo = r.connectionInfo)
            scope.launch {
              setCallSound(call.soundSpeaker, audioViaBluetooth)
            }
          }
          is WCallResponse.Ended -> {
            chatModel.activeCall.value = call.copy(callState = CallState.Ended)
            withApi { chatModel.callManager.endCall(call) }
            chatModel.showCallView.value = false
          }
          is WCallResponse.Ok -> when (val cmd = apiMsg.command) {
            is WCallCommand.Answer ->
              chatModel.activeCall.value = call.copy(callState = CallState.Negotiated)
            is WCallCommand.Media -> {
              when (cmd.media) {
                CallMediaType.Video -> chatModel.activeCall.value = call.copy(videoEnabled = cmd.enable)
                CallMediaType.Audio -> chatModel.activeCall.value = call.copy(audioEnabled = cmd.enable)
              }
            }
            is WCallCommand.Camera -> {
              chatModel.activeCall.value = call.copy(localCamera = cmd.camera)
              if (!call.audioEnabled) {
                chatModel.callCommand.value = WCallCommand.Media(CallMediaType.Audio, enable = false)
              }
            }
            is WCallCommand.End ->
              chatModel.showCallView.value = false
            else -> {}
          }
          is WCallResponse.Error -> {
            Log.e(TAG, "ActiveCallView: command error ${r.message}")
          }
        }
      }
    }
    val call = chatModel.activeCall.value
    if (call != null)  ActiveCallOverlay(call, chatModel, audioViaBluetooth)
  }

  val context = LocalContext.current
  DisposableEffect(Unit) {
    val activity = context as? Activity ?: return@DisposableEffect onDispose {}
    val prevVolumeControlStream = activity.volumeControlStream
    activity.volumeControlStream = AudioManager.STREAM_VOICE_CALL
    // Lock orientation to portrait in order to have good experience with calls
    activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT
    chatModel.activeCallViewIsVisible.value = true
    onDispose {
      activity.volumeControlStream = prevVolumeControlStream
      // Unlock orientation
      activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED
      chatModel.activeCallViewIsVisible.value = false
    }
  }
}

@Composable
private fun ActiveCallOverlay(call: Call, chatModel: ChatModel, audioViaBluetooth: MutableState<Boolean>) {
  ActiveCallOverlayLayout(
    call = call,
    speakerCanBeEnabled = !audioViaBluetooth.value,
    dismiss = { withApi { chatModel.callManager.endCall(call) } },
    toggleAudio = { chatModel.callCommand.value = WCallCommand.Media(CallMediaType.Audio, enable = !call.audioEnabled) },
    toggleVideo = { chatModel.callCommand.value = WCallCommand.Media(CallMediaType.Video, enable = !call.videoEnabled) },
    toggleSound = {
      var call = chatModel.activeCall.value
      if (call != null) {
        call = call.copy(soundSpeaker = !call.soundSpeaker)
        chatModel.activeCall.value = call
        setCallSound(call.soundSpeaker, audioViaBluetooth)
      }
    },
    flipCamera = { chatModel.callCommand.value = WCallCommand.Camera(call.localCamera.flipped) }
  )
}

private fun setCallSound(speaker: Boolean, audioViaBluetooth: MutableState<Boolean>) {
  val am = SimplexApp.context.getSystemService(Context.AUDIO_SERVICE) as AudioManager
  Log.d(TAG, "setCallSound: set audio mode, speaker enabled: $speaker")
  am.mode = AudioManager.MODE_IN_COMMUNICATION
  if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
    val btDevice = am.availableCommunicationDevices.lastOrNull { it.type == AudioDeviceInfo.TYPE_BLUETOOTH_SCO }
    val preferredSecondaryDevice = if (speaker) AudioDeviceInfo.TYPE_BUILTIN_SPEAKER else AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
    if (btDevice != null) {
      am.setCommunicationDevice(btDevice)
    } else if (am.communicationDevice?.type != preferredSecondaryDevice) {
      am.availableCommunicationDevices.firstOrNull { it.type == preferredSecondaryDevice }?.let {
        am.setCommunicationDevice(it)
      }
    }
  } else {
    if (audioViaBluetooth.value) {
      am.isSpeakerphoneOn = false
      am.startBluetoothSco()
    } else {
      am.stopBluetoothSco()
      am.isSpeakerphoneOn = speaker
    }
    am.isBluetoothScoOn = am.isBluetoothScoAvailableOffCall && audioViaBluetooth.value
  }
}

private fun dropAudioManagerOverrides() {
  val am = SimplexApp.context.getSystemService(Context.AUDIO_SERVICE) as AudioManager
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
  speakerCanBeEnabled: Boolean,
  dismiss: () -> Unit,
  toggleAudio: () -> Unit,
  toggleVideo: () -> Unit,
  toggleSound: () -> Unit,
  flipCamera: () -> Unit
) {
  Column(Modifier.padding(DEFAULT_PADDING)) {
    when (call.peerMedia ?: call.localMedia) {
      CallMediaType.Video -> {
        CallInfoView(call, alignment = Alignment.Start)
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Row(Modifier.fillMaxWidth().padding(horizontal = 6.dp), horizontalArrangement = Arrangement.SpaceBetween, verticalAlignment = Alignment.CenterVertically) {
          ToggleAudioButton(call, toggleAudio)
          Spacer(Modifier.size(40.dp))
          IconButton(onClick = dismiss) {
            Icon(painterResource(R.drawable.ic_call_end_filled), stringResource(R.string.icon_descr_hang_up), tint = Color.Red, modifier = Modifier.size(64.dp))
          }
          if (call.videoEnabled) {
            ControlButton(call, painterResource(R.drawable.ic_flip_camera_android_filled), R.string.icon_descr_flip_camera, flipCamera)
            ControlButton(call, painterResource(R.drawable.ic_videocam_filled), R.string.icon_descr_video_off, toggleVideo)
          } else {
            Spacer(Modifier.size(48.dp))
            ControlButton(call, painterResource(R.drawable.ic_videocam_off), R.string.icon_descr_video_on, toggleVideo)
          }
        }
      }
      CallMediaType.Audio -> {
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Column(
          Modifier.fillMaxWidth(),
          horizontalAlignment = Alignment.CenterHorizontally,
          verticalArrangement = Arrangement.Center
        ) {
          ProfileImage(size = 192.dp, image = call.contact.profile.image)
          CallInfoView(call, alignment = Alignment.CenterHorizontally)
        }
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Box(Modifier.fillMaxWidth().padding(bottom = DEFAULT_BOTTOM_PADDING), contentAlignment = Alignment.CenterStart) {
          Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
            IconButton(onClick = dismiss) {
              Icon(painterResource(R.drawable.ic_call_end_filled), stringResource(R.string.icon_descr_hang_up), tint = Color.Red, modifier = Modifier.size(64.dp))
            }
          }
          Box(Modifier.padding(start = 32.dp)) {
            ToggleAudioButton(call, toggleAudio)
          }
          Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.CenterEnd) {
            Box(Modifier.padding(end = 32.dp)) {
              ToggleSoundButton(call, speakerCanBeEnabled, toggleSound)
            }
          }
        }
      }
    }
  }
}

@Composable
private fun ControlButton(call: Call, icon: Painter, @StringRes iconText: Int, action: () -> Unit, enabled: Boolean = true) {
  if (call.hasMedia) {
    IconButton(onClick = action, enabled = enabled) {
      Icon(icon, stringResource(iconText), tint = if (enabled) Color(0xFFFFFFD8) else MaterialTheme.colors.secondary, modifier = Modifier.size(40.dp))
    }
  } else {
    Spacer(Modifier.size(40.dp))
  }
}

@Composable
private fun ToggleAudioButton(call: Call, toggleAudio: () -> Unit) {
  if (call.audioEnabled) {
    ControlButton(call, painterResource(R.drawable.ic_mic), R.string.icon_descr_audio_off, toggleAudio)
  } else {
    ControlButton(call, painterResource(R.drawable.ic_mic_off), R.string.icon_descr_audio_on, toggleAudio)
  }
}

@Composable
private fun ToggleSoundButton(call: Call, enabled: Boolean, toggleSound: () -> Unit) {
  if (call.soundSpeaker) {
    ControlButton(call, painterResource(R.drawable.ic_volume_up), R.string.icon_descr_speaker_off, toggleSound, enabled)
  } else {
    ControlButton(call, painterResource(R.drawable.ic_volume_down), R.string.icon_descr_speaker_on, toggleSound, enabled)
  }
}

@Composable
fun CallInfoView(call: Call, alignment: Alignment.Horizontal) {
  @Composable fun InfoText(text: String, style: TextStyle = MaterialTheme.typography.body2) =
    Text(text, color = Color(0xFFFFFFD8), style = style)
  Column(horizontalAlignment = alignment) {
    InfoText(call.contact.chatViewName, style = MaterialTheme.typography.h2)
    InfoText(call.callState.text)

    val connInfo = call.connectionInfo
//    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text}, ${connInfo.protocolText})"
    val connInfoText = if (connInfo == null) ""  else " (${connInfo.text})"
    InfoText(call.encryptionStatus + connInfoText)
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
fun WebRTCView(callCommand: MutableState<WCallCommand?>, onResponse: (WVAPIMessage) -> Unit) {
  val scope = rememberCoroutineScope()
  val webView = remember { mutableStateOf<WebView?>(null) }
  val permissionsState = rememberMultiplePermissionsState(
    permissions = listOf(
      Manifest.permission.CAMERA,
      Manifest.permission.RECORD_AUDIO,
      Manifest.permission.MODIFY_AUDIO_SETTINGS,
      Manifest.permission.INTERNET
    )
  )
  fun processCommand(wv: WebView, cmd: WCallCommand) {
    val apiCall = WVAPICall(command = cmd)
    wv.evaluateJavascript("processCommand(${json.encodeToString(apiCall)})", null)
  }
  val lifecycleOwner = LocalLifecycleOwner.current
  DisposableEffect(lifecycleOwner) {
    val observer = LifecycleEventObserver { _, event ->
      if (event == Lifecycle.Event.ON_RESUME || event == Lifecycle.Event.ON_START) {
        permissionsState.launchMultiplePermissionRequest()
      }
    }
    lifecycleOwner.lifecycle.addObserver(observer)
    onDispose {
      val wv = webView.value
      if (wv != null) processCommand(wv, WCallCommand.End)
      lifecycleOwner.lifecycle.removeObserver(observer)
      webView.value?.destroy()
      webView.value = null
    }
  }
  LaunchedEffect(callCommand.value, webView.value) {
    val cmd = callCommand.value
    val wv = webView.value
    if (cmd != null && wv != null) {
      Log.d(TAG, "WebRTCView LaunchedEffect executing $cmd")
      processCommand(wv, cmd)
      callCommand.value = null
    }
  }
  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/www/", WebViewAssetLoader.AssetsPathHandler(LocalContext.current))
    .build()

  if (permissionsState.allPermissionsGranted) {
    Box(Modifier.fillMaxSize()) {
      AndroidView(
        factory = { AndroidViewContext ->
          WebView(AndroidViewContext).apply {
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
            this.webViewClient = LocalContentWebViewClient(assetLoader)
            this.clearHistory()
            this.clearCache(true)
            this.addJavascriptInterface(WebRTCInterface(onResponse), "WebRTCInterface")
            val webViewSettings = this.settings
            webViewSettings.allowFileAccess = true
            webViewSettings.allowContentAccess = true
            webViewSettings.javaScriptEnabled = true
            webViewSettings.mediaPlaybackRequiresUserGesture = false
            webViewSettings.cacheMode = WebSettings.LOAD_NO_CACHE
            this.loadUrl("file:android_asset/www/call.html")
          }
        }
      ) { wv ->
        Log.d(TAG, "WebRTCView: webview ready")
        // for debugging
        // wv.evaluateJavascript("sendMessageToNative = ({resp}) => WebRTCInterface.postMessage(JSON.stringify({command: resp}))", null)
        scope.launch {
          delay(2000L)
          wv.evaluateJavascript("sendMessageToNative = (msg) => WebRTCInterface.postMessage(JSON.stringify(msg))", null)
          webView.value = wv
        }
      }
    }
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
    } catch (e: Error) {
      Log.e(TAG, "failed parsing WebView message: $message")
    }
  }
}

private class LocalContentWebViewClient(private val assetLoader: WebViewAssetLoader) : WebViewClientCompat() {
  override fun shouldInterceptRequest(
    view: WebView,
    request: WebResourceRequest
  ): WebResourceResponse? {
    return assetLoader.shouldInterceptRequest(request.url)
  }
}

@Preview
@Composable
fun PreviewActiveCallOverlayVideo() {
  SimpleXTheme {
    ActiveCallOverlayLayout(
      call = Call(
        contact = Contact.sampleData,
        callState = CallState.Negotiated,
        localMedia = CallMediaType.Video,
        peerMedia = CallMediaType.Video,
        connectionInfo = ConnectionInfo(
          RTCIceCandidate(RTCIceCandidateType.Host, "tcp", null),
          RTCIceCandidate(RTCIceCandidateType.Host, "tcp", null)
        )
      ),
      speakerCanBeEnabled = true,
      dismiss = {},
      toggleAudio = {},
      toggleVideo = {},
      toggleSound = {},
      flipCamera = {}
    )
  }
}

@Preview
@Composable
fun PreviewActiveCallOverlayAudio() {
  SimpleXTheme {
    ActiveCallOverlayLayout(
      call = Call(
        contact = Contact.sampleData,
        callState = CallState.Negotiated,
        localMedia = CallMediaType.Audio,
        peerMedia = CallMediaType.Audio,
        connectionInfo = ConnectionInfo(
          RTCIceCandidate(RTCIceCandidateType.Host, "udp", null),
          RTCIceCandidate(RTCIceCandidateType.Host, "udp", null)
        )
      ),
      speakerCanBeEnabled = true,
      dismiss = {},
      toggleAudio = {},
      toggleVideo = {},
      toggleSound = {},
      flipCamera = {}
    )
  }
}
