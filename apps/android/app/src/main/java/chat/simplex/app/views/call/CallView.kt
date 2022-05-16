package chat.simplex.app.views.call

import android.Manifest
import android.content.ClipData
import android.content.ClipboardManager
import android.util.Log
import android.view.ViewGroup
import android.webkit.*
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat
import chat.simplex.app.TAG
import chat.simplex.app.model.json
import chat.simplex.app.views.helpers.TextEditor
import com.google.accompanist.permissions.rememberMultiplePermissionsState
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString

@Composable
fun VideoCallView(close: () -> Unit) {
  val callCommand = remember { mutableStateOf<WCallCommand?>(null)}
  val commandText = remember { mutableStateOf("{\"command\": {\"type\": \"start\", \"media\": \"video\", \"aesKey\": \"FwW+t6UbnwHoapYOfN4mUBUuqR7UtvYWxW16iBqM29U=\"}}") }
  val clipboard = ContextCompat.getSystemService(LocalContext.current, ClipboardManager::class.java)

  BackHandler(onBack = close)
  Column(
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(12.dp),
    modifier = Modifier
      .background(MaterialTheme.colors.background)
      .fillMaxSize()
  ) {
    WebRTCView(callCommand) { resp ->
      // for debugging
      // commandText.value = resp
      commandText.value = json.encodeToString(resp)
    }

    TextEditor(Modifier.height(180.dp), text = commandText)

    Row(
      Modifier
        .fillMaxWidth()
        .padding(bottom = 6.dp),
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Button(onClick = {
        val clip: ClipData = ClipData.newPlainText("js command", commandText.value)
        clipboard?.setPrimaryClip(clip)
      }) { Text("Copy") }
      Button(onClick = {
        try {
          val apiCall: WVAPICall = json.decodeFromString(commandText.value)
          commandText.value = ""
          println("sending: ${commandText.value}")
          callCommand.value = apiCall.command
        } catch(e: Error) {
          println("error parsing command: ${commandText.value}")
          println(e)
        }
      }) { Text("Send") }
      Button(onClick = {
        commandText.value = ""
      }) { Text("Clear") }
    }
  }
}

@Composable
// for debugging
// fun WebRTCView(callCommand: MutableState<WCallCommand?>, onResponse: (String) -> Unit) {
fun WebRTCView(callCommand: MutableState<WCallCommand?>, onResponse: (WVAPIMessage) -> Unit) {
  lateinit var wv: WebView
  val permissionsState = rememberMultiplePermissionsState(
    permissions = listOf(
      Manifest.permission.CAMERA,
      Manifest.permission.RECORD_AUDIO,
      Manifest.permission.MODIFY_AUDIO_SETTINGS,
      Manifest.permission.INTERNET
    )
  )
  fun processCommand(cmd: WCallCommand) {
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
      processCommand(WCallCommand.End())
      lifecycleOwner.lifecycle.removeObserver(observer)
    }
  }
  LaunchedEffect(callCommand.value) {
    val cmd = callCommand.value
    if (cmd != null) {
      callCommand.value = null
      processCommand(cmd)
    }
  }

  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/www/", WebViewAssetLoader.AssetsPathHandler(LocalContext.current))
    .build()

  if (permissionsState.allPermissionsGranted) {
    Box(
      Modifier
        .fillMaxWidth()
        .aspectRatio(ratio = 1F)
    ) {
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
            webViewSettings.allowFileAccessFromFileURLs = true;
            webViewSettings.cacheMode = WebSettings.LOAD_NO_CACHE
            this.loadUrl("file:android_asset/www/call.html")
          }
        }
      ) {
        wv = it
        // for debugging
        // wv.evaluateJavascript("sendMessageToNative = ({resp}) => WebRTCInterface.postMessage(JSON.stringify({command: resp}))", null)
        wv.evaluateJavascript("sendMessageToNative = (msg) => WebRTCInterface.postMessage(JSON.stringify(msg))", null)
      }
    }
  } else {
    Text("NEED PERMISSIONS")
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
