package chat.simplex.app.views.call

import android.Manifest
import android.util.Log
import android.view.ViewGroup
import android.webkit.*
import androidx.activity.compose.BackHandler
import androidx.annotation.StringRes
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.coroutines.delay
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString

@Composable
fun ActiveCallView(chatModel: ChatModel) {
  BackHandler(onBack = {
    val call = chatModel.activeCall.value
    if (call != null) withApi { chatModel.callManager.endCall(call) }
  })
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
              }
              withApi { chatModel.controller.apiCallStatus(call.contact, callStatus) }
            } catch (e: Error) {
              Log.d(TAG,"call status ${r.state.connectionState} not used")
            }
          is WCallResponse.Connected -> {
            chatModel.activeCall.value = call.copy(callState = CallState.Connected, connectionInfo = r.connectionInfo)
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
    if (call != null)  ActiveCallOverlay(call, chatModel)
  }
}

@Composable
private fun ActiveCallOverlay(call: Call, chatModel: ChatModel) {
  ActiveCallOverlayLayout(
    call = call,
    dismiss = { withApi { chatModel.callManager.endCall(call) } },
    toggleAudio = { chatModel.callCommand.value = WCallCommand.Media(CallMediaType.Audio, enable = !call.audioEnabled) },
    toggleVideo = { chatModel.callCommand.value = WCallCommand.Media(CallMediaType.Video, enable = !call.videoEnabled) },
    flipCamera = { chatModel.callCommand.value = WCallCommand.Camera(call.localCamera.flipped) }
  )
}

@Composable
private fun ActiveCallOverlayLayout(
  call: Call,
  dismiss: () -> Unit,
  toggleAudio: () -> Unit,
  toggleVideo: () -> Unit,
  flipCamera: () -> Unit
) {
  Column(Modifier.padding(16.dp)) {
    when (call.peerMedia ?: call.localMedia) {
      CallMediaType.Video -> {
        CallInfoView(call, alignment = Alignment.Start)
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Row(Modifier.fillMaxWidth().padding(horizontal = 6.dp), horizontalArrangement = Arrangement.SpaceBetween, verticalAlignment = Alignment.CenterVertically) {
          ToggleAudioButton(call, toggleAudio)
          Spacer(Modifier.size(40.dp))
          IconButton(onClick = dismiss) {
            Icon(Icons.Filled.CallEnd, stringResource(R.string.icon_descr_hang_up), tint = Color.Red, modifier = Modifier.size(64.dp))
          }
          if (call.videoEnabled) {
            ControlButton(call, Icons.Filled.FlipCameraAndroid, R.string.icon_descr_flip_camera, flipCamera)
            ControlButton(call, Icons.Filled.Videocam, R.string.icon_descr_video_off, toggleVideo)
          } else {
            Spacer(Modifier.size(48.dp))
            ControlButton(call, Icons.Outlined.VideocamOff, R.string.icon_descr_video_on, toggleVideo)
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
        Box(Modifier.fillMaxWidth().padding(bottom = 48.dp), contentAlignment = Alignment.CenterStart) {
          Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
            IconButton(onClick = dismiss) {
              Icon(Icons.Filled.CallEnd, stringResource(R.string.icon_descr_hang_up), tint = Color.Red, modifier = Modifier.size(64.dp))
            }
          }
          Box(Modifier.padding(start = 32.dp)) {
            ToggleAudioButton(call, toggleAudio)
          }
        }
      }
    }
  }
}

@Composable
private fun ControlButton(call: Call, icon: ImageVector, @StringRes iconText: Int, action: () -> Unit) {
  if (call.hasMedia) {
    IconButton(onClick = action) {
      Icon(icon, stringResource(iconText), tint = Color(0xFFFFFFD8), modifier = Modifier.size(40.dp))
    }
  } else {
    Spacer(Modifier.size(40.dp))
  }
}

@Composable
private fun ToggleAudioButton(call: Call, toggleAudio: () -> Unit) {
  if (call.audioEnabled) {
    ControlButton(call, Icons.Outlined.Mic, R.string.icon_descr_video_off, toggleAudio)
  } else {
    ControlButton(call, Icons.Outlined.MicOff, R.string.icon_descr_audio_on, toggleAudio)
  }
}

@Composable
fun CallInfoView(call: Call, alignment: Alignment.Horizontal) {
  @Composable fun InfoText(text: String, style: TextStyle = MaterialTheme.typography.body2) =
    Text(text, color = Color(0xFFFFFFD8), style = style)
  Column(horizontalAlignment = alignment) {
    InfoText(call.contact.chatViewName, style = MaterialTheme.typography.h2)
    InfoText(call.callState.text)

    val connInfo =
      if (call.connectionInfo == null) ""
      else " (${call.connectionInfo.text})"
    InfoText(call.encryptionStatus + connInfo)
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
//      .background(MaterialTheme.colors.background)
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
        withApi {
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
        connectionInfo = ConnectionInfo(RTCIceCandidate(RTCIceCandidateType.Host), RTCIceCandidate(RTCIceCandidateType.Host))
      ),
      dismiss = {},
      toggleAudio = {},
      toggleVideo = {},
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
        connectionInfo = ConnectionInfo(RTCIceCandidate(RTCIceCandidateType.Host), RTCIceCandidate(RTCIceCandidateType.Host))
      ),
      dismiss = {},
      toggleAudio = {},
      toggleVideo = {},
      flipCamera = {}
    )
  }
}
