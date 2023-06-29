package chat.simplex.app.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ProfileImage

@Composable
fun ContactConnectionView(contactConnection: PendingContactConnection) {
  Row {
    Box(Modifier.size(72.dp), contentAlignment = Alignment.Center) {
      ProfileImage(size = 54.dp, null, if (contactConnection.initiated) R.drawable.ic_add_link else R.drawable.ic_link)
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
        color = MaterialTheme.colors.secondary
      )
      val height = with(LocalDensity.current) { 46.sp.toDp() }
      Text(contactConnection.description, Modifier.heightIn(min = height), maxLines = 2, color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight)
    }
    val ts = getTimestampText(contactConnection.updatedAt)
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
