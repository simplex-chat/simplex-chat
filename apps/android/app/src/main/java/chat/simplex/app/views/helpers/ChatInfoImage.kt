package chat.simplex.app.views.helpers

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatInfo
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun ChatInfoImage(chatInfo: ChatInfo, size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant) {
  val icon =
    if (chatInfo is ChatInfo.Group) R.drawable.ic_supervised_user_circle_filled
                                    else R.drawable.ic_account_circle_filled
  ProfileImage(size, chatInfo.image, icon, iconColor)
}

@Composable
fun IncognitoImage(size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant) {
  Box(Modifier.size(size)) {
    Icon(
      painterResource(R.drawable.ic_theater_comedy_filled), stringResource(R.string.incognito),
      modifier = Modifier.size(size).padding(size / 12),
      iconColor
    )
  }
}

@Composable
fun ProfileImage(
  size: Dp,
  image: String? = null,
  icon: Int = R.drawable.ic_account_circle_filled,
  color: Color = MaterialTheme.colors.secondaryVariant
) {
  Box(Modifier.size(size)) {
    if (image == null) {
      val iconToReplace = when (icon) {
        R.drawable.ic_account_circle_filled -> AccountCircleFilled
        R.drawable.ic_supervised_user_circle_filled -> SupervisedUserCircleFilled
        else -> null
      }
      if (iconToReplace != null) {
        Icon(
          iconToReplace,
          contentDescription = stringResource(R.string.icon_descr_profile_image_placeholder),
          tint = color,
          modifier = Modifier.fillMaxSize()
        )
      } else {
        Icon(
          painterResource(icon),
          contentDescription = stringResource(R.string.icon_descr_profile_image_placeholder),
          tint = color,
          modifier = Modifier.fillMaxSize()
        )
      }
    } else {
      val imageBitmap = base64ToBitmap(image).asImageBitmap()
      Image(
        imageBitmap,
        stringResource(R.string.image_descr_profile_image),
        contentScale = ContentScale.Crop,
        modifier = Modifier.size(size).padding(size / 12).clip(CircleShape)
      )
    }
  }
}


@Preview
@Composable
fun PreviewChatInfoImage() {
  SimpleXTheme {
    ChatInfoImage(
      chatInfo = ChatInfo.Direct.sampleData,
      size = 55.dp
    )
  }
}
