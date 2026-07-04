package chat.simplex.common.views.helpers

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.layoutId
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.LinkPreview
import chat.simplex.common.model.NetworkProxyAuth
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.chatViewScrollState
import chat.simplex.common.views.chat.item.CHAT_IMAGE_LAYOUT_ID
import chat.simplex.common.views.chat.item.imageViewFullWidth
import chat.simplex.res.MR
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlinx.coroutines.withContext
import org.jsoup.Jsoup
import java.net.Authenticator
import java.net.InetSocketAddress
import java.net.PasswordAuthentication
import java.net.Proxy
import java.net.URL
import java.util.UUID

private const val OG_SELECT_QUERY = "meta[property^=og:]"
private const val ICON_SELECT_QUERY = "link[rel^=icon],link[rel^=apple-touch-icon],link[rel^=shortcut icon]"
private val IMAGE_SUFFIXES = listOf(".jpg", ".png", ".ico", ".webp", ".gif")

// Authenticator.setDefault is process-global. The mutex serializes preview fetches
// so concurrent calls cannot clobber each other's authenticator, and so the
// snapshot/restore in getLinkPreview is race-free.
private val previewMutex = Mutex()

suspend fun getLinkPreview(url: String): LinkPreview? {
  return withContext(Dispatchers.IO) {
    previewMutex.withLock {
      try {
        try {
          val title: String?
          val u = kotlin.runCatching { URL(url) }.getOrNull() ?: return@withLock null
          val useSocksProxy = appPrefs.networkUseSocksProxy.get()
          val proxy: Proxy?
          if (useSocksProxy) {
            val networkProxy = appPrefs.networkProxy.get()
            proxy = Proxy(Proxy.Type.SOCKS, InetSocketAddress(networkProxy.host, networkProxy.port))
            val (authUser, authPass) = when (networkProxy.auth) {
              NetworkProxyAuth.USERNAME ->
                if (networkProxy.username.isNotEmpty() && networkProxy.password.isNotEmpty())
                  networkProxy.username to networkProxy.password
                else
                  null to null
              // Per-call random credentials drive Tor-style stream isolation: each
              // preview gets its own circuit, and previews don't share a circuit
              // with other unauthenticated traffic on the proxy.
              NetworkProxyAuth.ISOLATE ->
                UUID.randomUUID().toString() to UUID.randomUUID().toString()
            }
            if (authUser != null && authPass != null) {
              Authenticator.setDefault(object : Authenticator() {
                override fun getPasswordAuthentication(): PasswordAuthentication? =
                  // Only respond when the SOCKS proxy itself challenges. A destination
                  // server returning 401 also triggers RequestorType.SERVER; without
                  // this gate, the JDK's auto-retry would post our SOCKS credentials
                  // in an Authorization header to the destination.
                  if (requestingHost == networkProxy.host && requestingPort == networkProxy.port)
                    PasswordAuthentication(authUser, authPass.toCharArray())
                  else null
              })
            } else {
              Authenticator.setDefault(null)
            }
          } else {
            proxy = null
            Authenticator.setDefault(null)
          }
          var imageUri = when {
            IMAGE_SUFFIXES.any { u.path.lowercase().endsWith(it) } -> {
              title = u.path.substringAfterLast("/")
              url
            }
            else -> {
              val connection = Jsoup.connect(url)
                .ignoreContentType(true)
                .timeout(10000)
                .followRedirects(true)
                .proxy(proxy)

              val response = if (url.lowercase().startsWith("https://x.com/")) {
                // Apple sends request with special user-agent which handled differently by X.com.
                // Different response that includes video poster from post
                connection
                  .userAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_1) AppleWebKit/601.2.4 (KHTML, like Gecko) Version/9.0.1 Safari/601.2.4 facebookexternalhit/1.1 Facebot Twitterbot/1.0")
                  .execute()
              } else {
                connection
                  .execute()
              }
              val doc = response.parse()
              val ogTags = doc.select(OG_SELECT_QUERY)
              title = ogTags.firstOrNull { it.attr("property") == "og:title" }?.attr("content") ?: doc.title()
              ogTags.firstOrNull { it.attr("property") == "og:image" }?.attr("content")
                ?: doc.select(ICON_SELECT_QUERY).firstOrNull { it.attr("rel").contains("icon") }?.attr("href")
            }
          }
          if (imageUri != null) {
            imageUri = normalizeImageUri(u, imageUri)
            try {
              val conn = URL(imageUri).openConnection(proxy ?: Proxy.NO_PROXY)
              conn.connectTimeout = 20_000
              conn.readTimeout = 20_000
              val stream = conn.getInputStream()
              val image = resizeImageToStrSize(stream.use(::loadImageBitmap), maxDataSize = 14000)
              //          TODO add once supported in iOS
              //          val description = ogTags.firstOrNull {
              //            it.attr("property") == "og:description"
              //          }?.attr("content") ?: ""
              if (title != null) {
                return@withLock LinkPreview(url, title, description = "", image)
              }
            } catch (e: Exception) {
              e.printStackTrace()
            }
          }
        } catch (e: Exception) {
          e.printStackTrace()
        }
        return@withLock null
      } finally {
        Authenticator.setDefault(null)
      }
    }
  }
}

@Composable
fun ComposeLinkView(linkPreview: LinkPreview?, cancelPreview: () -> Unit, cancelEnabled: Boolean) {
  val sentColor = MaterialTheme.appColors.sentMessage
  Row(
    Modifier.fillMaxWidth().padding(top = 8.dp).background(sentColor),
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (linkPreview == null) {
      Box(
        Modifier.fillMaxWidth().weight(1f).height(60.dp).padding(start = 16.dp),
        contentAlignment = Alignment.CenterStart
      ) {
        CircularProgressIndicator(
          Modifier.size(16.dp),
          color = MaterialTheme.colors.secondary,
          strokeWidth = 2.dp
        )
      }
    } else {
      val imageBitmap = base64ToBitmap(linkPreview.image)
      Image(
        imageBitmap,
        stringResource(MR.strings.image_descr_link_preview),
        modifier = Modifier.width(80.dp).height(60.dp).padding(end = 8.dp)
      )
      Column(Modifier.fillMaxWidth().weight(1F)) {
        Text(linkPreview.title, maxLines = 1, overflow = TextOverflow.Ellipsis)
        Text(
          linkPreview.uri, maxLines = 1, overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body2
        )
      }
    }
    if (cancelEnabled) {
      IconButton(onClick = cancelPreview, modifier = Modifier.padding(0.dp)) {
        Icon(
          painterResource(MR.images.ic_close),
          contentDescription = stringResource(MR.strings.icon_descr_cancel_link_preview),
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.padding(10.dp)
        )
      }
    }
  }
}

@Composable
fun ChatItemLinkView(linkPreview: LinkPreview, showMenu: State<Boolean>, onLongClick: () -> Unit) {
  val image = base64ToBitmap(linkPreview.image)
  Column(
    Modifier
    .layoutId(CHAT_IMAGE_LAYOUT_ID)
    .width(if (image.width * 0.97 <= image.height) imageViewFullWidth() * 0.75f else DEFAULT_MAX_IMAGE_WIDTH)
  ) {
    val blurred = remember { mutableStateOf(appPrefs.privacyMediaBlurRadius.get() > 0) }
    Image(
      image,
      stringResource(MR.strings.image_descr_link_preview),
      modifier = Modifier
        .fillMaxWidth()
        .desktopModifyBlurredState(true, blurred, showMenu)
        .privacyBlur(true, blurred, chatViewScrollState.collectAsState(), onLongClick = onLongClick),
      contentScale = ContentScale.FillWidth,
    )
    Column(Modifier.padding(top = 6.dp).padding(horizontal = 12.dp)) {
      Text(linkPreview.title, maxLines = 3, overflow = TextOverflow.Ellipsis, lineHeight = 22.sp, modifier = Modifier.padding(bottom = 4.dp))
      if (linkPreview.description != "") {
        Text(linkPreview.description, maxLines = 12, overflow = TextOverflow.Ellipsis, fontSize = 14.sp, lineHeight = 20.sp)
      }
      Text(linkPreview.uri, maxLines = 1, overflow = TextOverflow.Ellipsis, fontSize = 12.sp, color = simplexSecondaryTint())
    }
  }
}

private fun normalizeImageUri(u: URL, imageUri: String) = when {
  !imageUri.lowercase().startsWith("http") -> {
    "${u.protocol}://${u.host}" +
        if (imageUri.startsWith("/"))
          imageUri
        else
        // When an icon is used as an image with relative href path like: site=site.com, <link rel="icon" href="icon.png">
          if (u.path.endsWith("/")) u.path + imageUri
          else u.path.substringBeforeLast("/") + "/$imageUri"
  }
  else -> imageUri
}

/*fun normalizeImageUriTest() {
  val expect = mapOf<Pair<URL, String>, String>(
    URL("https://example.com") to "icon.png" to "https://example.com/icon.png",
    URL("https://example.com/") to "icon.png" to "https://example.com/icon.png",
    URL("https://example.com/") to "/icon.png" to "https://example.com/icon.png",
    URL("https://example.com") to "assets/images/favicon.png" to "https://example.com/assets/images/favicon.png",
    URL("https://example.com/") to "assets/images/favicon.png" to "https://example.com/assets/images/favicon.png",
    URL("https://example.com/dir") to "/favicon.png" to "https://example.com/favicon.png",
    URL("https://example.com/dir/") to "favicon.png" to "https://example.com/dir/favicon.png",
    URL("https://example.com/dir/") to "/favicon.png" to "https://example.com/favicon.png",
    URL("https://example.com/dir/page") to "favicon.png" to "https://example.com/dir/favicon.png",
    URL("https://example.com/abcde.gif") to "https://example.com/abcde.gif" to "https://example.com/abcde.gif",
    URL("https://example.com/abcde.gif?a=b") to "https://example.com/abcde.gif?a=b" to "https://example.com/abcde.gif?a=b",
  )
  expect.forEach {
    Log.d(TAG, "Image URI ${normalizeImageUri(it.key.first, it.key.second)} == ${normalizeImageUri(it.key.first, it.key.second) == it.value}")
  }
}*/

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "ChatItemLinkView (Dark Mode)"
)*/
@Composable
fun PreviewChatItemLinkView() {
  SimpleXTheme {
    ChatItemLinkView(LinkPreview.sampleData, remember { mutableStateOf(false) }) {}
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "ComposeLinkView (Dark Mode)"
)*/
@Composable
fun PreviewComposeLinkView() {
  SimpleXTheme {
    ComposeLinkView(LinkPreview.sampleData, cancelPreview = { -> }, true)
  }
}

@Preview
@Composable
fun PreviewComposeLinkViewLoading() {
  SimpleXTheme {
    ComposeLinkView(null, cancelPreview = { -> }, true)
  }
}
