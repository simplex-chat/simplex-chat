package chat.simplex.common.views.chatlist

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.model.PendingContactConnection
import chat.simplex.common.model.getTimestampText
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun ContactConnectionView(contactConnection: PendingContactConnection) {
  Row {
    Box(Modifier.size(72.dp * fontSizeSqrtMultiplier), contentAlignment = Alignment.Center) {
      ProfileImage(size = 54.dp * fontSizeSqrtMultiplier, null, if (contactConnection.initiated) MR.images.ic_add_link else MR.images.ic_link)
    }
    Column(
      modifier = Modifier
        .padding(start = 8.dp / fontSizeSqrtMultiplier, end = 8.sp.toDp())
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
      Text(
        contactConnection.description,
        Modifier.heightIn(min = 46.sp.toDp()).padding(top = 3.sp.toDp()),
        maxLines = 2,
        style = TextStyle(
          fontFamily = Inter,
          fontSize = 15.sp,
          color = if (isInDarkTheme()) MessagePreviewDark else MessagePreviewLight,
          lineHeight = 21.sp
        )
      )
    }
    Box(
      contentAlignment = Alignment.TopEnd
    ) {
      val ts = getTimestampText(contactConnection.updatedAt)
      ChatListTimestampView(ts)
      Box(
        Modifier.padding(top = 50.sp.toDp()),
        contentAlignment = Alignment.Center
      ) {
        IncognitoIcon(contactConnection.incognito)
      }
    }
  }
}
