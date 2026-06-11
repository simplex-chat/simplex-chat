package chat.simplex.common.views.helpers

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeStatus
import chat.simplex.common.model.BadgeType
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.LocalBadge
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlin.math.max

@Composable
fun ChatInfoImage(chatInfo: ChatInfo, size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant, shadow: Boolean = false, tappableBadge: Boolean = false) {
  val icon =
    when (chatInfo) {
      is ChatInfo.Group -> chatInfo.groupInfo.chatIconName
      is ChatInfo.Local -> MR.images.ic_folder_filled
      is ChatInfo.Direct -> chatInfo.contact.chatIconName
      else -> MR.images.ic_account_circle_filled
    }
  // a deleted (inactive) contact shows the inactive icon instead of the badge
  val badge = when {
    chatInfo is ChatInfo.Direct && chatInfo.contact.active -> chatInfo.contact.profile.localBadge
    chatInfo is ChatInfo.ContactRequest -> chatInfo.contactRequest.profile.localBadge
    else -> null
  }
  BadgedProfileImage(size, badge, onBadgeClick = if (tappableBadge) badge?.let { b -> { showBadgeInfoAlert(b) } } else null) {
    ProfileImage(size, chatInfo.image, icon, if (chatInfo is ChatInfo.Local) NoteFolderIconColor else iconColor)
  }
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
  color: Color = MaterialTheme.colors.secondaryVariant,
  backgroundColor: Color? = null,
  blurred: Boolean = false,
  async: Boolean = false
) {
  Box(Modifier.size(size)) {
    if (image == null) {
      val iconToReplace = when (icon) {
        MR.images.ic_account_circle_filled -> AccountCircleFilled
        MR.images.ic_supervised_user_circle_filled -> SupervisedUserCircleFilled
        else -> null
      }
      if (iconToReplace != null) {
        if (backgroundColor != null) {
          Box(Modifier.size(size * 0.7f).align(Alignment.Center).background(backgroundColor, CircleShape))
        }
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
      if (async) {
        Base64AsyncImage(
          base64ImageString = image,
          contentDescription = stringResource(MR.strings.image_descr_profile_image),
          contentScale = ContentScale.Crop,
          modifier = ProfileIconModifier(size, blurred = blurred)
        )
      } else {
        val imageBitmap = base64ToBitmap(image)
        Image(
          bitmap = imageBitmap,
          contentDescription = stringResource(MR.strings.image_descr_profile_image),
          contentScale = ContentScale.Crop,
          modifier = ProfileIconModifier(size, blurred = blurred)
        )
      }
    }
  }
}

// Overlays a supporter badge on an avatar with zero layout impact: a custom Layout measures the badge but
// reports only the avatar's size, so the badge overflows bottom-right (not clamped) and nothing around it shifts.
@Composable
fun BadgedProfileImage(size: Dp, badge: LocalBadge?, onBadgeClick: (() -> Unit)? = null, avatar: @Composable () -> Unit) {
  if (badge == null) {
    avatar()
    return
  }
  Layout(content = {
    avatar()
    ProfileBadge(size, badge, onBadgeClick)
  }) { measurables, constraints ->
    val a = measurables[0].measure(constraints)
    val b = measurables[1].measure(Constraints())
    layout(a.width, a.height) {
      a.place(0, 0)
      // phone center sits 0.33*S right and down of the avatar center, overflowing the avatar bounds
      val off = (0.33f * a.width).toInt()
      b.place(x = a.width / 2 + off - b.width / 2, y = a.height / 2 + off - b.height / 2)
    }
  }
}

// tones the glyph down: slightly desaturated and dimmed, so the gradient is less bright against the avatar
private val badgeColorFilter = ColorFilter.colorMatrix(
  ColorMatrix().apply {
    setToSaturation(0.85f)
    timesAssign(
      ColorMatrix(
        floatArrayOf(
          0.92f, 0f, 0f, 0f, 0f,
          0f, 0.92f, 0f, 0f, 0f,
          0f, 0f, 0.92f, 0f, 0f,
          0f, 0f, 0f, 1f, 0f
        )
      )
    )
  }
)

// the phone glyph (or warning triangle) scales inversely to the avatar so it stays readable when the avatar is small.
@Composable
private fun ProfileBadge(size: Dp, badge: LocalBadge, onBadgeClick: (() -> Unit)?) {
  val s = size.value
  val mult = 1f + 0.3f * ((192f - s) / 156f).coerceIn(0f, 1f)
  // phone height ~0.28*S; set width only (= 0.7617*height) and let the height follow the glyph's aspect ratio
  val phoneW = 0.28f * 0.7617f * size * mult
  val mod = Modifier.width(phoneW).let { if (onBadgeClick != null) it.clickable(onClick = onBadgeClick) else it }
  if (badge.status == BadgeStatus.Failed) {
    Icon(painterResource(MR.images.ic_warning_filled), contentDescription = null, tint = WarningOrange, modifier = mod)
  } else {
    Image(
      painterResource(badgeImage(badge.badge.badgeType)),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      alpha = if (badge.status == BadgeStatus.Expired) 0.4f else 1f,
      colorFilter = badgeColorFilter,
      modifier = mod
    )
  }
}

fun showBadgeInfoAlert(badge: LocalBadge) {
  if (badge.status == BadgeStatus.Failed) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.badge_unverified_title),
      text = generalGetString(MR.strings.badge_unverified_desc)
    )
  } else {
    // a verified badge's type is signed and can't be faked, so the real (possibly unknown) type name is shown
    AlertManager.shared.showAlertMsg(
      title = badge.badge.badgeType.text.replaceFirstChar { it.uppercase() },
      text = generalGetString(MR.strings.badge_verified_desc)
    )
  }
}

private fun badgeImage(t: BadgeType): ImageResource = when (t) {
  is BadgeType.Legend -> MR.images.badge_legend
  is BadgeType.Investor -> MR.images.badge_investor
  else -> MR.images.badge_supporter // Supporter + Unknown
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
fun ProfileIconModifier(size: Dp, padding: Boolean = true, blurred: Boolean = false): Modifier {
  val percent = remember { appPreferences.profileImageCornerRadius.state }
  val r = max(0f, percent.value)
  val pad = if (padding) size / 12 else 0.dp
  val m = Modifier.size(size)
  val m1 = when {
    r >= 50 ->
      m.padding(pad).clip(CircleShape)
    r <= 0 -> {
      val sz = (size - 2 * pad) * squareToCircleRatio
      m.padding((size - sz) / 2)
    }
    else -> {
      val sz = (size - 2 * pad) * (squareToCircleRatio + r * radiusFactor)
      m.padding((size - sz) / 2).clip(RoundedCornerShape(size = sz * r / 100))
    }
  }
  return if (blurred) m1.blur(size / 4) else m1
}

/** [AccountCircleFilled] has its inner padding which leads to visible border if there is background underneath.
 * This is workaround
 * */
@Composable
fun ProfileImageForActiveCall(
  size: Dp,
  image: String? = null,
  color: Color = MaterialTheme.colors.secondaryVariant,
  backgroundColor: Color? = null,
  ) {
  if (image == null) {
    Box(Modifier.requiredSize(size).clip(CircleShape).then(if (backgroundColor != null) Modifier.background(backgroundColor) else Modifier)) {
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
      modifier = ProfileIconModifier(size, padding = false)
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
