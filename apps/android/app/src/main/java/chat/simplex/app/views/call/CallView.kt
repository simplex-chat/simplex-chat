package chat.simplex.app.views.call

import android.Manifest
import android.annotation.SuppressLint
import android.content.ClipData
import android.content.ClipboardManager
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
import chat.simplex.app.views.helpers.TextEditor
import com.google.accompanist.permissions.rememberMultiplePermissionsState

@SuppressLint("JavascriptInterface")
@Composable
fun VideoCallView(close: () -> Unit) {
  BackHandler(onBack = close)
  val context = LocalContext.current
  val clipboard = ContextCompat.getSystemService(context, ClipboardManager::class.java)
  val permissionsState = rememberMultiplePermissionsState(
    permissions = listOf(
      Manifest.permission.CAMERA,
      Manifest.permission.RECORD_AUDIO,
      Manifest.permission.MODIFY_AUDIO_SETTINGS,
      Manifest.permission.INTERNET
    )
  )
  val lifecycleOwner = LocalLifecycleOwner.current
  DisposableEffect(lifecycleOwner) {
    val observer = LifecycleEventObserver { _, event ->
      if (event == Lifecycle.Event.ON_RESUME || event == Lifecycle.Event.ON_START) {
        permissionsState.launchMultiplePermissionRequest()
      }
    }
    lifecycleOwner.lifecycle.addObserver(observer)

    onDispose { lifecycleOwner.lifecycle.removeObserver(observer) }
  }
  val localContext = LocalContext.current
  var iceCandidateCommand = remember { mutableStateOf("") }
  var commandToShow = remember { mutableStateOf("await processCommand({action: \"initiateCall\"})") }
  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/www/", WebViewAssetLoader.AssetsPathHandler(localContext))
    .build()
  lateinit var wv: WebView
  Column(
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(12.dp),
    modifier = Modifier
      .background(MaterialTheme.colors.background)
      .fillMaxSize()
  ) {
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
                    println("DENIED")
                    request.deny()
                  }
                }

                override fun onConsoleMessage(consoleMessage: ConsoleMessage?): Boolean {
                  println("CONSOLE MESSAGE")
                  val rtnValue = super.onConsoleMessage(consoleMessage)
                  if (consoleMessage?.message().toString().startsWith("{action: \"processIceCandidates\"")) {
                    println("ICE Candidate made")
                    iceCandidateCommand.value = "await processCommand(${consoleMessage?.message()})"
                  } else {
                    commandToShow.value = "await processCommand(${consoleMessage?.message()})"
                  }
                  println("JS MESSAGE: ${consoleMessage?.message()}")
                  return rtnValue
                }
              }
              this.webViewClient = LocalContentWebViewClient(assetLoader)
              this.clearHistory()
              this.clearCache(true)
//        this.addJavascriptInterface(JavascriptInterface(), "Android")
              val webViewSettings = this.settings
              webViewSettings.allowFileAccess = true
              webViewSettings.allowContentAccess = true
              webViewSettings.javaScriptEnabled = true
              webViewSettings.mediaPlaybackRequiresUserGesture = false
              webViewSettings.cacheMode = WebSettings.LOAD_NO_CACHE
              this.loadUrl("file:android_asset/www/call.html")
            }
          }
        ) {
          wv = it
          wv.post { wv.evaluateJavascript("f()") { response ->
            println("JAVASCRIPT RESPONSE: $response")
          } }
        }
      }
    } else {
      Text("NEED PERMISSIONS")
    }

    TextEditor(Modifier.height(180.dp), text = commandToShow)

    Row(
      Modifier
        .fillMaxWidth()
        .padding(bottom = 6.dp),
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Button( onClick = {
        val clip: ClipData = ClipData.newPlainText("js command", commandToShow.value)
        clipboard?.setPrimaryClip(clip)
      }) {Text("Copy")}
      Button( onClick = {
        commandToShow.value = clipboard?.primaryClip?.getItemAt(0)?.coerceToText(context) as String
      }) {Text("Paste")}
      Button( onClick = {
        println("sending: ${commandToShow.value}")
        wv.post {
          wv.evaluateJavascript(commandToShow.value) { response ->
            println("JAVASCRIPT RESPONSE: $response")
          }
        }
        commandToShow.value = ""
      }) {Text("Send")}
      Button( onClick = {
        commandToShow.value = iceCandidateCommand.value
      }) {Text("ICE Candidates")}
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
