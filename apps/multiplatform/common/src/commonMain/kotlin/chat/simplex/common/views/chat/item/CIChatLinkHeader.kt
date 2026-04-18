package chat.simplex.common.views.chat.item

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.ProfileImage
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun CIChatLinkHeader(
  chatLink: MsgChatLink,
  ownerSig: LinkOwnerSig?,
  hasText: Boolean,
  onClick: () -> Unit
) {
  Column(
    Modifier
      .clickable(onClick = onClick)
      .padding(horizontal = 12.dp, vertical = 6.dp)
  ) {
    Row(
      Modifier.padding(horizontal = 2.dp).padding(top = 8.dp, bottom = 6.dp)
    ) {
      ProfileImage(
        size = 44.dp,
        image = chatLink.image,
        icon = chatLink.iconRes,
        color = MaterialTheme.colors.secondary
      )
      Spacer(Modifier.width(4.dp))
      Column(Modifier.heightIn(min = 44.dp)) {
        Text(
          chatLink.displayName,
          style = MaterialTheme.typography.h6.copy(fontSize = 16.sp),
          fontWeight = FontWeight.Bold,
          maxLines = 2,
        )
        val fn = chatLink.fullName
        if (fn.isNotEmpty() && fn != chatLink.displayName) {
          Text(fn, style = MaterialTheme.typography.body2, maxLines = 2)
        }
      }
    }
    Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
      chatLink.shortDescription?.let { descr ->
        Text(
          descr,
          style = MaterialTheme.typography.caption,
          color = MaterialTheme.colors.secondary,
          maxLines = 2,
        )
      }
      Text(
        chatLink.infoLine(signed = ownerSig != null),
        style = MaterialTheme.typography.caption,
        color = MaterialTheme.colors.secondary,
      )
      Text(
        stringResource(MR.strings.tap_to_open),
        color = MaterialTheme.colors.primary,
        style = MaterialTheme.typography.body2,
      )
    }
  }
}
