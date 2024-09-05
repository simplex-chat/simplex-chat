package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.fontSizeSqrtMultiplier
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun UserPickerInactiveUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onShowAllProfilesClicked: () -> Unit,
  onUserClicked: (user: User) -> Unit,
) {
  val scrollState = rememberScrollState()

  if (users.isNotEmpty()) {
    SectionItemView(minHeight = 80.dp, padding = PaddingValues(start = 16.dp), disabled = stopped) {
      Box {
        Row(modifier = Modifier.padding(end = DEFAULT_PADDING * 1.9f).horizontalScroll(scrollState)) {
          users.forEach { u ->
            UserPickerInactiveUserBadge(u, stopped) {
              onUserClicked(it)
            }
            Spacer(Modifier.width(20.dp))
          }
          Spacer(Modifier.width(60.dp))
        }
        Box(
          contentAlignment = Alignment.CenterEnd,
          modifier = Modifier.padding(end = DEFAULT_PADDING * 0.7f).fillMaxWidth()
        ) {
          Row(
            horizontalArrangement = Arrangement.End,
            modifier = Modifier.fillMaxWidth().padding(end = DEFAULT_PADDING).height(60.dp)
          ) {
            Canvas(modifier = Modifier.height(60.dp).width(100.dp)) {
              drawRect(
                brush = Brush.linearGradient(
                  colors = listOf(
                    Color.Transparent,
                    CurrentColors.value.colors.surface,
                  )
                ),
              )
            }
          }
          IconButton(
            onClick = onShowAllProfilesClicked,
            enabled = !stopped
          ) {
            Box(
              contentAlignment = Alignment.Center,
              modifier = Modifier.padding(end = DEFAULT_PADDING_HALF)
            ) {
              Icon(
                painterResource(MR.images.ic_arrow_forward_ios),
                stringResource(MR.strings.your_chat_profiles),
                tint = MaterialTheme.colors.secondary,
              )
            }
          }
        }
      }
    }
  } else {
    UserPickerOptionRow(
      painterResource(MR.images.ic_manage_accounts),
      stringResource(MR.strings.your_chat_profiles),
      onShowAllProfilesClicked
    )
  }
}
