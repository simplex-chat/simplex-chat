package chat.simplex.common.views.helpers

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.InspectableValue
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlin.math.max

@Composable
fun ChatInfoImage(chatInfo: ChatInfo, size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant) {
  val icon =
    when (chatInfo) {
      is ChatInfo.Group -> MR.images.ic_supervised_user_circle_filled
      is ChatInfo.Local -> MR.images.ic_folder_filled
      else -> MR.images.ic_account_circle_filled
    }
  ProfileImage(size, chatInfo.image, icon, if (chatInfo is ChatInfo.Local) NoteFolderIconColor else iconColor)
}

@Composable
fun IncognitoImage(size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant) {
  Box(Modifier.size(size)) {
    Icon(
      painterResource(MR.images.ic_theater_comedy_filled), stringResource(MR.strings.incognito),
      modifier = Modifier.size(size).padding(size / 12),
      iconColor
    )
  }
}

@Composable
fun ProfileImage(
  size: Dp,
  image: String? = null,
  icon: ImageResource = MR.images.ic_account_circle_filled,
  color: Color = MaterialTheme.colors.secondaryVariant
) {
  Box(Modifier.size(size)) {
    if (image == null) {
      val iconToReplace = when (icon) {
        MR.images.ic_account_circle_filled -> AccountCircleFilled
        MR.images.ic_supervised_user_circle_filled -> SupervisedUserCircleFilled
        else -> null
      }
      if (iconToReplace != null) {
        Icon(
          iconToReplace,
          contentDescription = stringResource(MR.strings.icon_descr_profile_image_placeholder),
          tint = color,
          modifier = Modifier.fillMaxSize()
        )
      } else {
        Icon(
          painterResource(icon),
          contentDescription = stringResource(MR.strings.icon_descr_profile_image_placeholder),
          tint = color,
          modifier = Modifier.fillMaxSize()
        )
      }
    } else {
      val imageBitmap = base64ToBitmap(image)
      Image(
        imageBitmap,
        stringResource(MR.strings.image_descr_profile_image),
        contentScale = ContentScale.Crop,
        modifier = ProfileIconModifier(size)
      )
    }
  }
}

@Composable
fun ProfileImage(size: Dp, image: ImageResource) {
  Image(
    painterResource(image),
    stringResource(MR.strings.image_descr_profile_image),
    contentScale = ContentScale.Crop,
    modifier = ProfileIconModifier(size)
  )
}

private const val squareToCircleRatio = 0.935f

private const val radiusFactor = (1 - squareToCircleRatio) / 50

@Composable
fun ProfileIconModifier(size: Dp, padding: Boolean = true): Modifier {
  val percent = remember { appPreferences.profileImageCornerRadius.state }
  val r = max(0f, percent.value)
  val pad = if (padding) size / 12 else 0.dp
  return if (r >= 50) {
    Modifier.size(size).padding(pad).clip(CircleShape)
  } else if (r <= 0) {
    val sz = (size - 2 * pad) * squareToCircleRatio
    Modifier.size(size).padding((size - sz) / 2)
  } else {
    val sz = (size - 2 * pad) * (squareToCircleRatio + r * radiusFactor)
    Modifier.size(size).padding((size - sz) / 2).clip(RoundedCornerShape(size = sz * r / 100))
  }
}

/** [AccountCircleFilled] has its inner padding which leads to visible border if there is background underneath.
 * This is workaround
 * */
@Composable
fun ProfileImageForActiveCall(
  size: Dp,
  image: String? = null,
  color: Color = MaterialTheme.colors.secondaryVariant,
) {
  if (image == null) {
    Box(Modifier.requiredSize(size).clip(CircleShape)) {
      Icon(
        AccountCircleFilled,
        contentDescription = stringResource(MR.strings.icon_descr_profile_image_placeholder),
        tint = color,
        modifier = Modifier.requiredSize(size + 14.dp)
      )
    }
  } else {
    val imageBitmap = base64ToBitmap(image)
    Image(
      imageBitmap,
      stringResource(MR.strings.image_descr_profile_image),
      contentScale = ContentScale.Crop,
      modifier = ProfileIconModifier(size, padding = false) // Modifier.size(size).clip(ProfileIconShape())
    )
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
