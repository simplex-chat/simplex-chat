package chat.simplex.app.views.call

import android.view.ViewGroup
import android.webkit.*
import androidx.activity.compose.BackHandler
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.viewinterop.AndroidView
import androidx.webkit.WebViewAssetLoader
import androidx.webkit.WebViewClientCompat

@Composable
fun VideoCallView(close: () -> Unit) {
  BackHandler(onBack = close)
  val assetLoader = WebViewAssetLoader.Builder()
    .addPathHandler("/assets/", WebViewAssetLoader.AssetsPathHandler(LocalContext.current))
    .build()
  AndroidView(
    factory = {
        AndroidViewContext ->
      WebView(AndroidViewContext).apply {
        layoutParams = ViewGroup.LayoutParams(
          ViewGroup.LayoutParams.MATCH_PARENT,
          ViewGroup.LayoutParams.MATCH_PARENT,
        )
        this.webViewClient = LocalContentWebViewClient(assetLoader)
        val webViewSettings = this.settings
        webViewSettings.allowFileAccess = true
        webViewSettings.allowContentAccess = true
        this.loadUrl("https://appassets.androidplatform.net/assets/www/call.html")
      }
    }
  )




}

private class LocalContentWebViewClient(private val assetLoader: WebViewAssetLoader) : WebViewClientCompat() {
  override fun shouldInterceptRequest(
    view: WebView,
    request: WebResourceRequest
  ): WebResourceResponse? {
    return assetLoader.shouldInterceptRequest(request.url)
  }
}