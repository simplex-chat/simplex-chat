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
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.chatViewScrollState
import chat.simplex.common.views.chat.item.CHAT_IMAGE_LAYOUT_ID
import chat.simplex.common.views.chat.item.fixedMediaWidth
import chat.simplex.common.views.chat.item.isTallMedia
import chat.simplex.common.views.chat.item.mediaWidth
import chat.simplex.res.*

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
        modifier = Modifier.padding(start = 12.dp).width(80.dp).height(60.dp)
      )
      Column(
        Modifier.fillMaxWidth().weight(1F).padding(start = 8.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.spacedBy(4.dp)
      ) {
        Text(linkPreview.title, maxLines = 1, overflow = TextOverflow.Ellipsis)
        Text(
          linkPreview.uri, maxLines = 1, overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary,
          fontSize = 12.sp
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
  val image = remember(linkPreview.image) { base64ToBitmap(linkPreview.image) }
  val tall = isTallMedia(image.width, image.height)
  Column(
    Modifier
    .layoutId(CHAT_IMAGE_LAYOUT_ID)
    .mediaWidth(fixedMediaWidth(tall), tall)
  ) {
    val blurred = remember { mutableStateOf(appPrefs.privacyMediaBlurRadius.get() > 0) }
    Image(
      image,
      stringResource(MR.strings.image_descr_link_preview),
      modifier = Modifier
        .fillMaxWidth()
        .desktopModifyBlurredState(blurred, showMenu)
        .privacyBlur(true, image, blurred, chatViewScrollState.collectAsState(), onLongClick = onLongClick),
      contentScale = ContentScale.FillWidth,
    )
    Column(Modifier.padding(top = 6.dp).padding(horizontal = 12.dp)) {
      Text(linkPreview.title, maxLines = 3, overflow = TextOverflow.Ellipsis, lineHeight = 22.sp, modifier = Modifier.padding(bottom = 4.dp))
      if (linkPreview.description != "") {
        Text(linkPreview.description, maxLines = 2, overflow = TextOverflow.Ellipsis, fontSize = 14.sp, lineHeight = 20.sp)
      }
      Text(linkPreview.uri, maxLines = 1, overflow = TextOverflow.Ellipsis, fontSize = 12.sp, color = MaterialTheme.colors.secondary)
    }
  }
}

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
