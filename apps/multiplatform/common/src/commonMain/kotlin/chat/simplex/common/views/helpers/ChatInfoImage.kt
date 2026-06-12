package chat.simplex.common.views.helpers

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.foundation.shape.*
import androidx.compose.material.Icon
import androidx.compose.material.LocalTextStyle
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.Placeholder
import androidx.compose.ui.text.PlaceholderVerticalAlign
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeStatus
import chat.simplex.common.model.BadgeType
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupMember
import chat.simplex.common.model.LocalBadge
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlin.math.max
import kotlin.math.roundToInt

@Composable
fun ChatInfoImage(chatInfo: ChatInfo, size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant, shadow: Boolean = false) {
  val icon =
    when (chatInfo) {
      is ChatInfo.Group -> chatInfo.groupInfo.chatIconName
      is ChatInfo.Local -> MR.images.ic_folder_filled
      is ChatInfo.Direct -> chatInfo.contact.chatIconName
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

// the badge shown for a chat: an active contact's or a contact request's (groups have none)
val ChatInfo.nameBadge: LocalBadge? get() = when {
  this is ChatInfo.Direct && contact.active -> contact.profile.localBadge
  this is ChatInfo.ContactRequest -> contactRequest.profile.localBadge
  else -> null
}

val GroupMember.nameBadge: LocalBadge? get() = memberProfile.localBadge

// badge height in em: calibrated visually so the badge top matches capital letters and digits
// (Inter's declared cap height is 2048/2816 = 0.727em, but the rendered text is taller than the metrics predict)
private const val fontCapHeightRatio = 0.95f

// fraction of the badge height pushed below the text baseline (like the undershoot of round letters)
private const val badgeBaselineOffsetRatio = 0.05f

// the badge glyph's width / height (the SVGs are cropped to the glyph: 300 x 399)
private const val badgeAspectRatio = 300f / 399f

// A contact/member name with the badge right after it: the badge is baseline-aligned with the name
// and sized to its font (fontSize if given, otherwise style.fontSize), and a truncated name keeps it visible.
@Composable
fun NameWithBadge(
  name: String,
  badge: LocalBadge?,
  modifier: Modifier = Modifier,
  color: Color = Color.Unspecified,
  fontSize: TextUnit = TextUnit.Unspecified,
  fontStyle: FontStyle? = null,
  fontWeight: FontWeight? = null,
  overflow: TextOverflow = TextOverflow.Clip,
  maxLines: Int = Int.MAX_VALUE,
  style: TextStyle = LocalTextStyle.current
) {
  Row(modifier) {
    Text(
      name,
      Modifier.alignByBaseline().weight(1f, fill = false),
      color = color,
      fontSize = fontSize,
      fontStyle = fontStyle,
      fontWeight = fontWeight,
      overflow = overflow,
      maxLines = maxLines,
      style = style
    )
    NameBadge(badge, if (fontSize.isSpecified) fontSize else style.fontSize)
  }
}

// Badge next to the contact name in a Row: top aligned with capital letters, bottom just below the text baseline.
// Use NameWithBadge unless the row needs special arrangement; then the name Text must use Modifier.alignByBaseline().
@Composable
fun RowScope.NameBadge(badge: LocalBadge?, fontSize: TextUnit = LocalTextStyle.current.fontSize) {
  if (badge == null) return
  val height = with(LocalDensity.current) { (if (fontSize.isSpecified) fontSize else 14.sp).toDp() } * fontCapHeightRatio
  BadgeGlyph(
    badge,
    // the alignment line sits badgeBaselineOffsetRatio above the badge's bottom edge,
    // so the Row places the badge that much below the text baseline;
    // 6.dp matches the visible gap between the name and the verification shield:
    // the shield has 3.dp end padding plus ~17% internal glyph margin, the badge artwork has none
    Modifier.alignBy { (it.measuredHeight * (1 - badgeBaselineOffsetRatio)).roundToInt() }.padding(start = 6.dp).height(height).aspectRatio(badgeAspectRatio)
  )
}

// badge inside a Text via appendInlineContent(id): bottom on the baseline, cap-height tall.
// precede with append(" ") for the space between the name and the badge.
fun nameBadgeInline(badge: LocalBadge, fontSize: TextUnit, onBadgeClick: (() -> Unit)? = null): InlineTextContent {
  val height = fontSize * fontCapHeightRatio
  return InlineTextContent(
    Placeholder(height * badgeAspectRatio, height, PlaceholderVerticalAlign.AboveBaseline)
  ) {
    // the placeholder bottom sits on the baseline and can't extend below it,
    // so the badge is drawn shifted down by badgeBaselineOffsetRatio instead
    BadgeGlyph(badge, Modifier.fillMaxSize().graphicsLayer { translationY = size.height * badgeBaselineOffsetRatio }, onBadgeClick)
  }
}

@Composable
private fun BadgeGlyph(badge: LocalBadge, modifier: Modifier, onBadgeClick: (() -> Unit)? = null) {
  val mod = modifier.let { if (onBadgeClick != null) it.clickable(onClick = onBadgeClick) else it }
  if (badge.status == BadgeStatus.Failed) {
    Icon(painterResource(MR.images.ic_warning_filled), contentDescription = null, tint = WarningOrange, modifier = mod)
  } else {
    Image(
      painterResource(badgeImage(badge.badge.badgeType)),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      alpha = if (badge.status == BadgeStatus.Expired) 0.4f else 1f,
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
