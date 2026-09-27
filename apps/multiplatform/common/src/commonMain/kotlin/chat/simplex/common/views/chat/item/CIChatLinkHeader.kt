package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.ProfileImage
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun CIChatLinkHeader(
  chatLink: MsgChatLink,
  ownerSig: LinkOwnerSig?,
  hasText: Boolean,
) {
  Column(
    Modifier
      .defaultMinSize(minWidth = 220.dp)
      .padding(start = 8.dp, end = 12.dp, top = 8.dp, bottom = 4.dp)
  ) {
    Row(
      Modifier.defaultMinSize(minWidth = 220.dp)
    ) {
      ProfileImage(
        size = 54.dp,
        image = chatLink.image,
        icon = chatLink.iconRes,
        color = if (isInDarkTheme()) FileDark else FileLight
      )
      Spacer(Modifier.width(8.dp))
      Column(
        Modifier.defaultMinSize(minHeight = 54.dp),
        verticalArrangement = Arrangement.Center
      ) {
        Text(
          chatLink.displayName,
          style = MaterialTheme.typography.caption,
          fontWeight = FontWeight.Medium,
          maxLines = 2,
          overflow = TextOverflow.Ellipsis
        )
        val fn = chatLink.fullName
        if (fn.isNotEmpty() && fn != chatLink.displayName) {
          Text(fn, maxLines = 2, overflow = TextOverflow.Ellipsis)
        }
      }
    }
    Divider(Modifier.fillMaxWidth().padding(top = 8.dp))
    Column(Modifier.padding(top = 8.dp, bottom = 4.dp, start = 4.dp), verticalArrangement = Arrangement.spacedBy(2.dp)) {
      chatLink.shortDescription?.let { descr ->
        Text(
          descr,
          color = MaterialTheme.colors.secondary,
          fontSize = 13.sp,
          lineHeight = 18.sp,
          maxLines = 2,
          overflow = TextOverflow.Ellipsis,
        )
      }
      Text(
        chatLink.infoLine(signed = ownerSig != null),
        color = MaterialTheme.colors.secondary,
        fontSize = 13.sp,
        lineHeight = 18.sp,
      )
      Text(
        stringResource(MR.strings.tap_to_open),
        color = MaterialTheme.colors.primary,
      )
    }
  }
}
