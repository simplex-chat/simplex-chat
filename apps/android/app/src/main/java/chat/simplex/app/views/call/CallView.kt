package chat.simplex.app.views.call

import android.Manifest
import android.annotation.SuppressLint
import android.view.ViewGroup
import android.webkit.*
import androidx.activity.compose.BackHandler
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.viewinterop.AndroidView
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat
import com.google.accompanist.permissions.rememberMultiplePermissionsState

@SuppressLint("JavascriptInterface")
@Composable
fun VideoCallView(close: () -> Unit) {
  BackHandler(onBack = close)
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
  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/www/", WebViewAssetLoader.AssetsPathHandler(localContext))
    .build()
  if (permissionsState.allPermissionsGranted) {
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
              val rtnValue = super.onConsoleMessage(consoleMessage)
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
//    webView -> webView.post { webView.evaluateJavascript("javascript:f();", null) }
    }
  } else {
    Text("NEED PERMISSIONS")
  }

}

private class LocalContentWebViewClient(private val assetLoader: WebViewAssetLoader) : WebViewClientCompat() {
  override fun shouldInterceptRequest(
    view: WebView,
    request: WebResourceRequest
  ): WebResourceResponse? {
    return assetLoader.shouldInterceptRequest(request.url)
  }
  override fun onPageFinished(view: WebView?, url: String?) {
    val msg = "{\"action\": \"initiateCall\", \"content\": {}}"
    view?.post {
      view.evaluateJavascript("processInbound($msg);") {
        response -> println("JAVASCRIPT RESPONSE: $response")
      }
    }
  }
}
