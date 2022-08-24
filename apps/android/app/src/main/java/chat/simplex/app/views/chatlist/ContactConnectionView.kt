package chat.simplex.app.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.AddLink
import androidx.compose.material.icons.outlined.Link
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.PendingContactConnection
import chat.simplex.app.model.getTimestampText
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ProfileImage

@Composable
fun ContactConnectionView(contactConnection: PendingContactConnection) {
  Row {
    Box(Modifier.size(72.dp), contentAlignment = Alignment.Center) {
      ProfileImage(size = 54.dp, null, if (contactConnection.initiated) Icons.Outlined.AddLink else Icons.Outlined.Link)
    }
    Column(
      modifier = Modifier
        .padding(horizontal = 8.dp)
        .weight(1F)
    ) {
      Text(
        contactConnection.displayName,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Bold,
        color = HighOrLowlight
      )
      Text(contactConnection.description, maxLines = 2, color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight)
    }
    val ts = getTimestampText(contactConnection.updatedAt)
    Column(
      Modifier.fillMaxHeight(),
      verticalArrangement = Arrangement.Top
    ) {
      Text(
        ts,
        color = HighOrLowlight,
        style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(bottom = 5.dp)
      )
    }
  }
}
