package chat.simplex.app.views.helpers

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.*
import androidx.compose.material.icons.filled.AccountCircle
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.graphics.vector.ImageVector
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
fun ChatInfoImage(chatInfo: ChatInfo, size: Dp, iconColor: Color = MaterialTheme.colors.secondary) {
  val icon =
    if (chatInfo is ChatInfo.Group) painterResource(R.drawable.ic_supervised_user_circle_filled)
                                    else painterResource(R.drawable.ic_account_circle_filled)
  ProfileImage(size, chatInfo.image, icon, iconColor)
}

@Composable
fun IncognitoImage(size: Dp, iconColor: Color = MaterialTheme.colors.secondary) {
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
  icon: Painter = painterResource(R.drawable.ic_account_circle_filled),
  color: Color = MaterialTheme.colors.secondary
) {
  Box(Modifier.size(size)) {
    if (image == null || icon == painterResource(R.drawable.ic_account_circle_filled)) {
      Icon(
        AccountCircle,
        contentDescription = stringResource(R.string.icon_descr_profile_image_placeholder),
        tint = color,
        modifier = Modifier.fillMaxSize()
      )
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

private val AccountCircle: ImageVector
  get() {
    if (_accountCircle != null) {
      return _accountCircle!!
    }
    _accountCircle = materialIcon(name = "Filled.AccountCircle") {
      materialPath {
        moveTo(12.0f, 2.0f)
        curveTo(6.48f, 2.0f, 2.0f, 6.48f, 2.0f, 12.0f)
        reflectiveCurveToRelative(4.48f, 10.0f, 10.0f, 10.0f)
        reflectiveCurveToRelative(10.0f, -4.48f, 10.0f, -10.0f)
        reflectiveCurveTo(17.52f, 2.0f, 12.0f, 2.0f)
        close()
        moveTo(12.0f, 5.0f)
        curveToRelative(1.66f, 0.0f, 3.0f, 1.34f, 3.0f, 3.0f)
        reflectiveCurveToRelative(-1.34f, 3.0f, -3.0f, 3.0f)
        reflectiveCurveToRelative(-3.0f, -1.34f, -3.0f, -3.0f)
        reflectiveCurveToRelative(1.34f, -3.0f, 3.0f, -3.0f)
        close()
        moveTo(12.0f, 19.2f)
        curveToRelative(-2.5f, 0.0f, -4.71f, -1.28f, -6.0f, -3.22f)
        curveToRelative(0.03f, -1.99f, 4.0f, -3.08f, 6.0f, -3.08f)
        curveToRelative(1.99f, 0.0f, 5.97f, 1.09f, 6.0f, 3.08f)
        curveToRelative(-1.29f, 1.94f, -3.5f, 3.22f, -6.0f, 3.22f)
        close()
      }
    }
    return _accountCircle!!
  }

private var _accountCircle: ImageVector? = null


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
