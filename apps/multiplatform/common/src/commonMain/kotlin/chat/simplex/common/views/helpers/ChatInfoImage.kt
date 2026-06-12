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
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.Placeholder
import androidx.compose.ui.text.PlaceholderVerticalAlign
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
import kotlin.math.roundToInt

@Composable
fun ChatInfoImage(chatInfo: ChatInfo, size: Dp, iconColor: Color = MaterialTheme.colors.secondaryVariant, shadow: Boolean = false, tappableBadge: Boolean = false, scaled: Boolean = false) {
  val icon =
    when (chatInfo) {
      is ChatInfo.Group -> chatInfo.groupInfo.chatIconName
      is ChatInfo.Local -> MR.images.ic_folder_filled
      is ChatInfo.Direct -> chatInfo.contact.chatIconName
      else -> MR.images.ic_account_circle_filled
    }
  val badge = chatInfo.nameBadge
  ProfileImage(
    size,
    chatInfo.image,
    icon,
    if (chatInfo is ChatInfo.Local) NoteFolderIconColor else iconColor,
    badge = badge,
    onBadgeClick = if (tappableBadge) badge?.let { b -> { showBadgeInfoAlert(b) } } else null,
    scaled = scaled
  )
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

// `size` is the design size; `scaled = true` renders the avatar (and badge) at size * fontSizeSqrtMultiplier.
// The badge is measured but only the avatar's size is reported, so it overflows bottom-right
// and has no effect on the avatar's own layout or anything around it.
@Composable
fun ProfileImage(
  size: Dp,
  image: String? = null,
  icon: ImageResource = MR.images.ic_account_circle_filled,
  color: Color = MaterialTheme.colors.secondaryVariant,
  backgroundColor: Color? = null,
  blurred: Boolean = false,
  async: Boolean = false,
  badge: LocalBadge? = null,
  onBadgeClick: (() -> Unit)? = null,
  scaled: Boolean = false
) {
  val sz = if (scaled) size * fontSizeSqrtMultiplier else size
  ProfileImageBox(sz, image, icon, color, backgroundColor, blurred, async)
  // the badge on the avatar is replaced by NameBadge next to the contact name; kept to allow reverting:
  // if (badge == null) {
  //   ProfileImageBox(sz, image, icon, color, backgroundColor, blurred, async)
  // } else {
  //   val inset = profileImageInset(sz)
  //   Layout(content = {
  //     ProfileImageBox(sz, image, icon, color, backgroundColor, blurred, async)
  //     ProfileBadge(badgeWidthRatio(size) * sz, badge, onBadgeClick)
  //   }) { measurables, constraints ->
  //     val avatar = measurables[0].measure(constraints)
  //     val bdg = measurables[1].measure(Constraints())
  //     val insetPx = inset.roundToPx()
  //     layout(avatar.width, avatar.height) {
  //       avatar.place(0, 0)
  //       // badgeInsideShare of the badge width is inside the visible avatar (the box minus its inset);
  //       // the badge overhangs the avatar's bottom-right edge by the same amount on both axes
  //       val overhang = (badgeOverhang * bdg.width).roundToInt()
  //       bdg.place(
  //         x = avatar.width - insetPx - bdg.width + overhang,
  //         y = avatar.height - insetPx - bdg.height + overhang
  //       )
  //     }
  //   }
  // }
}

@Composable
private fun ProfileImageBox(size: Dp, image: String?, icon: ImageResource, color: Color, backgroundColor: Color?, blurred: Boolean, async: Boolean) {
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

// tones the glyph down: slightly desaturated and dimmed, so the gradient is less bright against the avatar
private val badgeColorFilter = ColorFilter.colorMatrix(
  ColorMatrix().apply {
    setToSaturation(0.85f)
    timesAssign(
      ColorMatrix(
        floatArrayOf(
          0.985f, 0f, 0f, 0f, 0f,
          0f, 0.985f, 0f, 0f, 0f,
          0f, 0f, 0.985f, 0f, 0f,
          0f, 0f, 0f, 1f, 0f
        )
      )
    )
  }
)

// Badge width as a fraction of the avatar design size, one entry per size in use - tune each placement here.
// Values match the original inverse-scaling formula 0.2133 * (1 + 0.5 * clamp((192 - s) / 156, 0, 1)):
// the smaller the avatar, the bigger the badge relative to it, so it stays readable at small sizes.
private fun badgeWidthRatio(size: Dp): Float {
  val s = size.value.roundToInt()
  return when {
    // 36: AddGroupMembersView (contact rows when adding members), ChatItemInfoView (member row in message info)
    // 37: MEMBER_IMAGE_SIZE - member avatar next to group message bubbles
    // 38: ChannelMembersView, ChannelRelaysView (member rows)
    // 40: ChatInfoToolbarTitle (chat header), MemberSupportChatToolbarTitle (member support chat header)
    // 42: ContactPreviewView (contacts list), ShareListNavLinkView (share-to list), MEMBER_ROW_AVATAR_SIZE (group info / member support member rows)
    s <= 42 -> 0.32f
    // 54: UserPicker (user switcher)
    // 57: ChatItemInfoView (forwarded-from chat)
    // 60: AddGroupMembersView toolbar (group avatar - shows no badge currently)
    s <= 60 -> 0.3f
    s <= 72 -> 0.25f // ChatPreviewView (chat list), ContactRequestView (contact request in chat list)
    s <= 138 -> 0.225f // alertProfileImageSize - open-chat and scan-link alerts
    s <= 192 -> 0.2f // ChatInfoView (contact info), GroupMemberInfoView (member info)
    else -> 0.20f
  }
}

// share of the badge width outside the avatar overhanging the corner
private const val badgeOverhang = 0.3f

// the badge glyph's width / height (the SVGs are cropped to the glyph: 300 x 399)
private const val badgeAspectRatio = 300f / 399f

@Composable
private fun ProfileBadge(width: Dp, badge: LocalBadge, onBadgeClick: (() -> Unit)?) {
  // aspectRatio makes the measured height the glyph's height - otherwise Image measures at the
  // SVG's intrinsic height, and placement relies on the measured edges
  val mod = Modifier.width(width).aspectRatio(badgeAspectRatio).let { if (onBadgeClick != null) it.clickable(onClick = onBadgeClick) else it }
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

// the badge shown for a chat: an active contact's or a contact request's (groups have none)
val ChatInfo.nameBadge: LocalBadge? get() = when {
  this is ChatInfo.Direct && contact.active -> contact.profile.localBadge
  this is ChatInfo.ContactRequest -> contactRequest.profile.localBadge
  else -> null
}

// badge height in em: calibrated visually so the badge top matches capital letters and digits.
// Inter's declared cap height is 2048/2816 = 0.727em (round glyphs reach 0.737em), but the
// rendered text is taller than these metrics predict, and 0.85 is what matches on screen.
private const val fontCapHeightRatio = 0.85f

// fraction of the badge height pushed below the text baseline (like the undershoot of round letters)
private const val badgeBaselineOffsetRatio = 0.05f

// Badge next to the contact name in a Row: top aligned with capital letters, bottom with the text baseline.
// The name Text must use Modifier.alignByBaseline(), and weight(1f, fill = false) when it can ellipsize,
// so a long name truncates and the badge stays visible right after it.
@Composable
fun RowScope.NameBadge(badge: LocalBadge?, fontSize: TextUnit = LocalTextStyle.current.fontSize, onBadgeClick: (() -> Unit)? = null) {
  if (badge == null) return
  val height = with(LocalDensity.current) { (if (fontSize.isSpecified) fontSize else 14.sp).toDp() } * fontCapHeightRatio
  Log.d("BADGE", "row fontSize=$fontSize specified=${fontSize.isSpecified} height=$height")
  BadgeGlyph(
    badge,
    // the alignment line sits badgeBaselineOffsetRatio above the badge's bottom edge,
    // so the Row places the badge that much below the text baseline
    Modifier.alignBy { (it.measuredHeight * (1 - badgeBaselineOffsetRatio)).roundToInt() }.padding(start = 4.dp).height(height).aspectRatio(badgeAspectRatio),
    onBadgeClick
  )
}

// badge inside a Text via appendInlineContent(id): bottom on the baseline, cap-height tall.
// precede with append(" ") for the space between the name and the badge.
fun nameBadgeInline(badge: LocalBadge, fontSize: TextUnit, onBadgeClick: (() -> Unit)? = null): InlineTextContent {
  val height = fontSize * fontCapHeightRatio
  Log.d("BADGE", "inline fontSize=$fontSize height=$height")
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

// the inset of the visible avatar inside its box, depending on the corner radius preference:
// size / 12 for a circle (the factor is exactly 1 at r = 50), larger for square shapes.
// single definition - used by ProfileIconModifier and to anchor the badge in ProfileImage
@Composable
fun profileImageInset(size: Dp, padding: Boolean = true): Dp {
  val percent = remember { appPreferences.profileImageCornerRadius.state }
  val r = max(0f, percent.value)
  val pad = if (padding) size / 12 else 0.dp
  return if (r >= 50) {
    pad
  } else {
    (size - (size - 2 * pad) * (squareToCircleRatio + r * radiusFactor)) / 2
  }
}

@Composable
fun ProfileIconModifier(size: Dp, padding: Boolean = true, blurred: Boolean = false): Modifier {
  val percent = remember { appPreferences.profileImageCornerRadius.state }
  val r = max(0f, percent.value)
  val inset = profileImageInset(size, padding)
  val visible = size - 2 * inset
  val m = Modifier.size(size).padding(inset)
  val m1 = when {
    r >= 50 -> m.clip(CircleShape)
    r <= 0 -> m
    else -> m.clip(RoundedCornerShape(size = visible * r / 100))
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
