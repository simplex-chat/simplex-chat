package chat.simplex.app.views.helpers

import android.content.res.Configuration
import android.graphics.BitmapFactory
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.LinkPreview
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.item.SentColorLight
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
          val image = resizeImageToStrSize(BitmapFactory.decodeStream(stream), maxDataSize = 14000)
//          TODO add once supported in iOS
//          val description = ogTags.firstOrNull {
//            it.attr("property") == "og:description"
//          }?.attr("content") ?: ""
          val title = ogTags.firstOrNull { it.attr("property") == "og:title" }?.attr("content")
          if (title != null) {
            return@withContext LinkPreview(url, title, description = "", image)
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
fun ComposeLinkView(linkPreview: LinkPreview?, cancelPreview: () -> Unit) {
  Row(
    Modifier.fillMaxWidth().padding(top = 8.dp).background(SentColorLight),
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (linkPreview == null) {
      Box(
        Modifier.fillMaxWidth().weight(1f).height(60.dp).padding(start = 16.dp),
        contentAlignment = Alignment.CenterStart
      ) {
        CircularProgressIndicator(
          Modifier.size(16.dp),
          color = HighOrLowlight,
          strokeWidth = 2.dp
        )
      }
    } else {
      val imageBitmap = base64ToBitmap(linkPreview.image).asImageBitmap()
      Image(
        imageBitmap,
        stringResource(R.string.image_descr_link_preview),
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
    IconButton(onClick = cancelPreview, modifier = Modifier.padding(0.dp)) {
      Icon(
        Icons.Outlined.Close,
        contentDescription = stringResource(R.string.icon_descr_cancel_link_preview),
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
  }
}

@Composable
fun ChatItemLinkView(linkPreview: LinkPreview) {
  Column {
    Image(
      base64ToBitmap(linkPreview.image).asImageBitmap(),
      stringResource(R.string.image_descr_link_preview),
      modifier = Modifier.fillMaxWidth(),
      contentScale = ContentScale.FillWidth,
    )
    Column(Modifier.padding(top = 6.dp).padding(horizontal = 12.dp)) {
      Text(linkPreview.title, maxLines = 3, overflow = TextOverflow.Ellipsis, lineHeight = 22.sp, modifier = Modifier.padding(bottom = 4.dp))
      if (linkPreview.description != "") {
        Text(linkPreview.description, maxLines = 12, overflow = TextOverflow.Ellipsis, fontSize = 14.sp, lineHeight = 20.sp)
      }
      Text(linkPreview.uri, maxLines = 1, overflow = TextOverflow.Ellipsis, fontSize = 12.sp, color = HighOrLowlight)
    }
  }
}


@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "ChatItemLinkView (Dark Mode)"
)
@Composable
fun PreviewChatItemLinkView() {
  SimpleXTheme {
    ChatItemLinkView(LinkPreview.sampleData)
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "ComposeLinkView (Dark Mode)"
)
@Composable
fun PreviewComposeLinkView() {
  SimpleXTheme {
    ComposeLinkView(LinkPreview.sampleData) { -> }
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewComposeLinkViewLoading() {
  SimpleXTheme {
    ComposeLinkView(null) { -> }
  }
}
