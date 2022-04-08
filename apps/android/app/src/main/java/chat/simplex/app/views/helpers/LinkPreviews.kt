package chat.simplex.app.views.helpers

import android.graphics.BitmapFactory
import androidx.compose.runtime.Composable
import chat.simplex.app.model.LinkPreview
import org.jsoup.Jsoup

private const val OG_SELECT_QUERY = "meta[property^=og:]"

fun getLinkPreview(url: String): LinkPreview? {
  try {
    val response = Jsoup.connect(url)
      .ignoreContentType(true)
      .timeout(10000)
      .followRedirects(true)
      .execute()
    val doc = response.parse()
    val ogTags = doc.select(OG_SELECT_QUERY)
    val imageUri = ogTags.firstOrNull {it.attr("property") == "og:image"}?.attr("content")
    if (imageUri != null) {
      try {
        val stream = java.net.URL(imageUri).openStream()
        val image = BitmapFactory.decodeStream(stream)
        val encodedImage = bitmapToBase64(image)
        val description = ogTags.firstOrNull {
          it.attr("property") == "og:description"
        }?.attr("content") ?: ""
        val title = ogTags.firstOrNull { it.attr("property") == "og:title" }?.attr("content")
        if (title != null) {
          return LinkPreview(url, title, description, encodedImage)
        }
      } catch (e: Exception) {
        e.printStackTrace()
      }
    }
  } catch (e: Exception) {
    e.printStackTrace()
  }
  return null
}



@Composable
fun ChatItemLinkPreview(metadata: LinkPreview) {

}

@Composable
fun ComposeLinkPreview(metadata: LinkPreview, cancelPreview: () -> Unit) {

}