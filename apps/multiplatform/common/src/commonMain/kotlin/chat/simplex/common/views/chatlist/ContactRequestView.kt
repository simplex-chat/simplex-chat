package chat.simplex.common.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextStyle
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.getTimestampText
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun ContactRequestView(contactRequest: ChatInfo.ContactRequest) {
  Row {
    ChatInfoImage(contactRequest, size = 72.dp * fontSizeSqrtMultiplier)
    Column(
      modifier = Modifier
        .padding(horizontal = 8.sp.toDp())
        .weight(1F)
    ) {
      Text(
        contactRequest.chatViewName,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Bold,
        color = MaterialTheme.colors.primary,
      )
      val height = with(LocalDensity.current) { 46.sp.toDp() }
      Text(
        stringResource(MR.strings.contact_wants_to_connect_with_you),
        Modifier.heightIn(min = height).padding(top = 3.sp.toDp()),
        maxLines = 2,
        style = TextStyle(
          fontFamily = Inter,
          fontSize = 15.sp,
          color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight,
          lineHeight = 21.sp
        )
      )
    }
    val ts = getTimestampText(contactRequest.contactRequest.updatedAt)
    ChatListTimestampView(ts)
  }
}
