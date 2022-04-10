package chat.simplex.app.views.helpers

import android.content.res.Configuration
import android.graphics.BitmapFactory
import android.text.Layout
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.LinkPreview
import chat.simplex.app.ui.theme.SimpleXTheme
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import org.jsoup.Jsoup

private const val OG_SELECT_QUERY = "meta[property^=og:]"

suspend fun getLinkPreview(url: String): LinkPreview? {
  return withContext(Dispatchers.IO) {
    try {
      val response = Jsoup.connect(url)
        .ignoreContentType(true)
        .timeout(10000)
        .followRedirects(true)
        .execute()
      val doc = response.parse()
      val ogTags = doc.select(OG_SELECT_QUERY)
      val imageUri = ogTags.firstOrNull { it.attr("property") == "og:image" }?.attr("content")
      if (imageUri != null) {
        try {
          val stream = java.net.URL(imageUri).openStream()
          val image = BitmapFactory.decodeStream(stream)
          val encodedImage = bitmapToBase64(image, maxStringLength = 14000)
          val description = ogTags.firstOrNull {
            it.attr("property") == "og:description"
          }?.attr("content") ?: ""
          val title = ogTags.firstOrNull { it.attr("property") == "og:title" }?.attr("content")
          if (title != null) {
            return@withContext LinkPreview(url, title, description, encodedImage)
          }
        } catch (e: Exception) {
          e.printStackTrace()
        }
      }
    } catch (e: Exception) {
      e.printStackTrace()
    }
    return@withContext null
  }
}



@Composable
fun ComposeLinkPreview(metadata: LinkPreview, cancelPreview: () -> Unit) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    val imageBitmap = base64ToBitmap(metadata.image).asImageBitmap()
    Image(
      imageBitmap,
      "preview image",
    )
    Column {
      Row(horizontalArrangement = Arrangement.SpaceBetween, modifier = Modifier.fillMaxWidth()) {
        Text(metadata.title)
        IconButton(onClick = cancelPreview, modifier = Modifier.padding(0.dp)) {
          Icon(
            Icons.Outlined.Close,
            contentDescription = "Cancel Preview",
            tint = MaterialTheme.colors.primary,
          )

        }
      }
      Text(metadata.description, maxLines = 1, overflow = TextOverflow.Ellipsis)
      Text(
        metadata.uri,
        style = MaterialTheme.typography.subtitle2.copy(color = MaterialTheme.colors.secondaryVariant)
      )
    }
  }
}

@Composable
fun ChatItemLinkPreview(metadata: LinkPreview) {
  Column {
    val imageBitmap = base64ToBitmap(metadata.image).asImageBitmap()
    Image(
      imageBitmap,
      "preview image",
      modifier = Modifier.fillMaxWidth()
    )
    Text(metadata.title)
    Text(metadata.description, maxLines = 1, overflow = TextOverflow.Ellipsis)
    Text(
      metadata.uri,
      style = MaterialTheme.typography.subtitle2.copy(color = MaterialTheme.colors.secondaryVariant)
    )
  }
}


@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Chat Item Link Preview (Dark Mode)"
)
@Composable
fun PreviewChatItemLinkPreview() {
  SimpleXTheme {
    ChatItemLinkPreview(LinkPreview.sampleData)
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Compose Link Preview (Dark Mode)"
)
@Composable
fun PreviewComposeLinkPreview() {
  SimpleXTheme {
    ComposeLinkPreview(LinkPreview.sampleData) { -> }
  }
}