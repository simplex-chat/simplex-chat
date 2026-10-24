package chat.simplex.common.views.chat.item

import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.getChatItemIndexOrNull
import chat.simplex.common.platform.onRightClick
import chat.simplex.common.views.helpers.toDp

@Composable
fun CIChatFeatureView(
  chatsCtx: ChatModel.ChatsContext,
  chatInfo: ChatInfo,
  chatItem: ChatItem,
  feature: Feature,
  iconColor: Color,
  icon: Painter? = null,
  revealed: State<Boolean>,
  showMenu: MutableState<Boolean>,
) {
  val merged = if (!revealed.value) mergedFeatures(chatsCtx, chatItem, chatInfo) else emptyList()
  Box(
    Modifier
      .combinedClickable(
        onLongClick = { showMenu.value = true },
        onClick = {}
      )
      .onRightClick { showMenu.value = true }
  ) {
    if (!revealed.value && merged != null) {
      val terminal = LocalTerminalLayout.current
      Row(
        Modifier.padding(chatEventPadding(vertical = 8.dp)).then(if (terminal) Modifier.height(chatEventTextStyle().lineHeight.toDp()) else Modifier),
        horizontalArrangement = Arrangement.spacedBy(4.dp),
        verticalAlignment = if (terminal) Alignment.CenterVertically else Alignment.Top
      ) {
        merged.forEach {
          FeatureIconView(it)
        }
      }
    } else {
      FullFeatureView(chatItem, feature, iconColor, icon)
    }
  }
}

private data class FeatureInfo(
  val icon: PainterBox,
  val color: Color,
  val param: String?
)

private class PainterBox(
  val featureName: String,
  val icon: Painter,
) {
  override fun hashCode(): Int = featureName.hashCode()
  override fun equals(other: Any?): Boolean = other is PainterBox && featureName == other.featureName
}

@Composable
private fun Feature.toFeatureInfo(color: Color, param: Int?, type: String): FeatureInfo =
  FeatureInfo(
    icon = PainterBox(type, iconFilled()),
    color = color,
    param = if (this.hasParam && param != null) timeText(param) else null
  )

@Composable
private fun mergedFeatures(chatsCtx: ChatModel.ChatsContext, chatItem: ChatItem, chatInfo: ChatInfo): List<FeatureInfo>? {
  val fs: ArrayList<FeatureInfo> = arrayListOf()
  val icons: MutableSet<PainterBox> = mutableSetOf()
  val reversedChatItems = chatsCtx.chatItems.value.asReversed()
  var i = getChatItemIndexOrNull(chatItem, reversedChatItems)
  if (i != null) {
    while (i < reversedChatItems.size) {
      val f = featureInfo(reversedChatItems[i], chatInfo) ?: break
      if (!icons.contains(f.icon)) {
        fs.add(0, f)
        icons.add(f.icon)
      }
      i++
    }
  }
  return if (fs.size > 1) fs else null
}

@Composable
private fun featureInfo(ci: ChatItem, chatInfo: ChatInfo): FeatureInfo? =
  when (ci.content) {
    is CIContent.RcvChatFeature -> ci.content.feature.toFeatureInfo(ci.content.enabled.iconColor, ci.content.param, ci.content.feature.name)
    is CIContent.SndChatFeature -> ci.content.feature.toFeatureInfo(ci.content.enabled.iconColor, ci.content.param, ci.content.feature.name)
    is CIContent.RcvGroupFeature -> ci.content.groupFeature.toFeatureInfo(ci.content.preference.enabled(ci.content.memberRole_, (chatInfo as? ChatInfo.Group)?.groupInfo?.membership).iconColor, ci.content.param, ci.content.groupFeature.name)
    is CIContent.SndGroupFeature -> ci.content.groupFeature.toFeatureInfo(ci.content.preference.enabled(ci.content.memberRole_, (chatInfo as? ChatInfo.Group)?.groupInfo?.membership).iconColor, ci.content.param, ci.content.groupFeature.name)
    else -> null
  }

@Composable
private fun FeatureIconView(f: FeatureInfo) {
  val icon = @Composable { Icon(f.icon.icon, null, Modifier.inlineIcon().size(20.dp), tint = f.color) }
  if (f.param != null) {
    Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
      icon()
      Text(chatEventText(f.param, AnnotatedString("  ")), maxLines = 1, style = chatEventTextStyle())
    }
  } else {
    icon()
  }
}

@Composable
private fun FullFeatureView(
  chatItem: ChatItem,
  feature: Feature,
  iconColor: Color,
  icon: Painter? = null
) {
  ChatEventWithMeta(chatItem) { separator, ts ->
    Row(
      Modifier.padding(chatEventPadding()),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      Icon(icon ?: feature.iconFilled(), feature.text, Modifier.inlineIcon().size(20.dp), tint = iconColor)
      Text(
        chatEventText(chatItem, ts = separator + ts),
        Modifier,
        // this is important. Otherwise, aligning will be bad because annotated string has a Span with size 12.sp
        fontSize = 12.sp,
        style = chatEventTextStyle()
      )
    }
  }
}
