package chat.simplex.common.views.chat.item

import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.getChatItemIndexOrNull
import chat.simplex.common.views.helpers.onRightClick

@Composable
fun CIChatFeatureView(
  chatItem: ChatItem,
  feature: Feature,
  iconColor: Color,
  icon: Painter? = null,
  revealed: MutableState<Boolean>,
  showMenu: MutableState<Boolean>,
) {
  val merged = if (!revealed.value) mergedFeatures(chatItem) else emptyList()
  Box(
    Modifier
      .combinedClickable(
        onLongClick = { showMenu.value = true },
        onClick = {}
      )
      .onRightClick { showMenu.value = true }
  ) {
    if (!revealed.value && merged != null) {
      Row(
        Modifier.padding(horizontal = 6.dp, vertical = 8.dp),
        horizontalArrangement = Arrangement.spacedBy(4.dp)
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
  val icon: Painter,
  val color: Color,
  val param: String?
)

@Composable
private fun Feature.toFeatureInfo(color: Color, param: Int?): FeatureInfo =
  FeatureInfo(
    icon = iconFilled(),
    color = color,
    param = if (this.hasParam && param != null) timeText(param) else null
  )

@Composable
private fun mergedFeatures(chatItem: ChatItem): List<FeatureInfo>? {
  val m = ChatModel
  val fs: ArrayList<FeatureInfo> = arrayListOf()
  val icons: MutableSet<Painter> = mutableSetOf()
  var i = getChatItemIndexOrNull(chatItem)
  val reversedChatItems = m.chatItems.asReversed()
  if (i != null) {
    while (i < reversedChatItems.size) {
      val f = featureInfo(reversedChatItems[i]) ?: break
      if (!icons.contains(f.icon)) {
        fs.add(0, f)
        icons.add(f.icon)
      }
      i += 1
    }
  }
  return if (fs.size > 1) fs else null
}

@Composable
private fun featureInfo(ci: ChatItem): FeatureInfo? =
  when (ci.content) {
    is CIContent.RcvChatFeature -> ci.content.feature.toFeatureInfo(ci.content.enabled.iconColor, ci.content.param)
    is CIContent.SndChatFeature -> ci.content.feature.toFeatureInfo(ci.content.enabled.iconColor, ci.content.param)
    is CIContent.RcvGroupFeature -> ci.content.groupFeature.toFeatureInfo(ci.content.preference.enable.iconColor, ci.content.param)
    is CIContent.SndGroupFeature -> ci.content.groupFeature.toFeatureInfo(ci.content.preference.enable.iconColor, ci.content.param)
    else -> null
  }

@Composable
private fun FeatureIconView(f: FeatureInfo) {
  val icon = @Composable { Icon(f.icon, null, tint = f.color) }
  if (f.param != null) {
    Row {
      icon()
      Text(chatEventText(f.param, ""), maxLines = 1)
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
  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Icon(icon ?: feature.iconFilled(), feature.text, Modifier.size(18.dp), tint = iconColor)
    Text(
      chatEventText(chatItem),
      Modifier,
      // this is important. Otherwise, aligning will be bad because annotated string has a Span with size 12.sp
      fontSize = 12.sp
    )
  }
}
