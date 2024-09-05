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
    SectionItemView(padding = PaddingValues(start = 16.dp, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL, bottom = DEFAULT_PADDING_HALF), disabled = stopped) {
      Box {
        Row(
          modifier = Modifier.padding(end = DEFAULT_PADDING + 30.dp).horizontalScroll(scrollState)
        ) {
          users.forEach { u ->
            UserPickerInactiveUserBadge(u, stopped) {
              onUserClicked(it)
            }
            Spacer(Modifier.width(20.dp))
          }
          Spacer(Modifier.width(60.dp))
        }
        Row(
          horizontalArrangement = Arrangement.End,
          modifier = Modifier
            .fillMaxWidth()
            .padding(end = DEFAULT_PADDING + 30.dp)
            .height(60.dp)
        ) {
          Canvas(modifier = Modifier.size(60.dp)) {
            drawRect(
              brush = Brush.horizontalGradient(
                colors = listOf(
                  Color.Transparent,
                  CurrentColors.value.colors.surface,
                )
              ),
            )
          }
        }
        Row(
          horizontalArrangement = Arrangement.End,
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier
            .height(60.dp)
            .fillMaxWidth()
            .padding(end = DEFAULT_PADDING)
        ) {
          IconButton(
            onClick = onShowAllProfilesClicked,
            enabled = !stopped
          ) {
            Icon(
              painterResource(MR.images.ic_chevron_right),
              stringResource(MR.strings.your_chat_profiles),
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier.size(34.dp)
            )
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
