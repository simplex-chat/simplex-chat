package chat.simplex.app.views.call

import android.annotation.SuppressLint
import android.view.ViewGroup
import android.webkit.*
import androidx.activity.compose.BackHandler
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.viewinterop.AndroidView
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat

@SuppressLint("JavascriptInterface")
@Composable
fun VideoCallView(close: () -> Unit) {
  BackHandler(onBack = close)
  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/www/", WebViewAssetLoader.AssetsPathHandler(LocalContext.current))
    .build()
  AndroidView(
    factory = {
        AndroidViewContext ->
      WebView(AndroidViewContext).apply {
        layoutParams = ViewGroup.LayoutParams(
          ViewGroup.LayoutParams.MATCH_PARENT,
          ViewGroup.LayoutParams.MATCH_PARENT,
        )
        this.webChromeClient = object: WebChromeClient() {
          override fun onPermissionRequest(request: PermissionRequest?) {
            request?.grant(request.resources)
          }
        }
        this.webViewClient = LocalContentWebViewClient(assetLoader)
        this.clearHistory()
        this.clearCache(true)
        this.addJavascriptInterface(JavascriptInterface(), "Android")
        val webViewSettings = this.settings
        webViewSettings.allowFileAccess = true
        webViewSettings.allowContentAccess = true
        webViewSettings.javaScriptEnabled = true
        webViewSettings.mediaPlaybackRequiresUserGesture = false
        webViewSettings.useWideViewPort = true
        webViewSettings.cacheMode = WebSettings.LOAD_NO_CACHE
        this.loadUrl("file:android_asset/www/call.html")
      }
    }
  ) {
//    webView -> webView.post { webView.evaluateJavascript("javascript:f();", null) }
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
    view?.post { view.evaluateJavascript("javascript:f();", null) }
  }
}
