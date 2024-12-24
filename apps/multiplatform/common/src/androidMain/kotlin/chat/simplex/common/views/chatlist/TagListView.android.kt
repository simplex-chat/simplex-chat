package chat.simplex.common.views.chatlist

import SectionItemView
import android.view.ViewGroup
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.emoji2.emojipicker.EmojiPickerView
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
actual fun ChatTagInput(name: MutableState<String>, showError: State<Boolean>, emoji: MutableState<String?>) {
  SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING_HALF)) {
    Box(Modifier
      .clip(shape = CircleShape)
      .clickable {
        ModalManager.start.showModalCloseable { close ->
          EmojiPicker(close = {
            close()
            emoji.value = it
          })
        }
      }
      .padding(4.dp)
    ) {
      val emojiValue = emoji.value
      if (emojiValue != null) {
        Text(emojiValue)
      } else {
        Icon(
          painter = painterResource(MR.images.ic_add_reaction),
          contentDescription = null,
          tint = MaterialTheme.colors.secondary
        )
      }
    }
    Spacer(Modifier.width(8.dp))
    TagListNameTextField(name, showError = showError)
  }
}

@Composable
private fun EmojiPicker(close: (String?) -> Unit) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val topPaddingToContent = topPaddingToContent(false)

  Column (
    modifier = Modifier.fillMaxSize().navigationBarsPadding().padding(
      start = DEFAULT_PADDING_HALF,
      end = DEFAULT_PADDING_HALF,
      top = if (oneHandUI.value) WindowInsets.statusBars.asPaddingValues().calculateTopPadding() else topPaddingToContent,
      bottom = if (oneHandUI.value) WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + AppBarHeight * fontSizeSqrtMultiplier else 0.dp
    ),
  ) {
    AndroidView(
      factory = { context ->
        EmojiPickerView(context).apply {
          emojiGridColumns = 10
          layoutParams = ViewGroup.LayoutParams(
            ViewGroup.LayoutParams.MATCH_PARENT,
            ViewGroup.LayoutParams.MATCH_PARENT
          )
          setOnEmojiPickedListener { pickedEmoji ->
            close(pickedEmoji.emoji)
          }
        }
      }
    )
  }
}
