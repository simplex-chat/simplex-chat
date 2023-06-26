package chat.simplex.common.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.ChatInfoImage
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.getTimestampText

@Composable
fun ContactRequestView(chatModelIncognito: Boolean, contactRequest: ChatInfo.ContactRequest) {
  Row {
    ChatInfoImage(contactRequest, size = 72.dp)
    Column(
      modifier = Modifier
        .padding(horizontal = 8.dp)
        .weight(1F)
    ) {
      Text(
        contactRequest.chatViewName,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Bold,
        color = if (chatModelIncognito) Indigo else MaterialTheme.colors.primary
      )
      val height = with(LocalDensity.current) { 46.sp.toDp() }
      Text(stringResource(MR.strings.contact_wants_to_connect_with_you), Modifier.heightIn(min = height), maxLines = 2, color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight)
    }
    val ts = getTimestampText(contactRequest.contactRequest.updatedAt)
    Column(
      Modifier.fillMaxHeight(),
    ) {
      Text(
        ts,
        color = MaterialTheme.colors.secondary,
        style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(bottom = 5.dp)
      )
    }
  }
}
