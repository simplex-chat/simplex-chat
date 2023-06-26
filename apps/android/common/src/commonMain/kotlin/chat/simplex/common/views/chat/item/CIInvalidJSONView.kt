package chat.simplex.common.views.chat.item

import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.shareText
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsActionItem

@Composable
fun CIInvalidJSONView(json: String) {
  Row(Modifier
    .clickable { ModalManager.shared.showModal(true) { InvalidJSONView(json) } }
    .padding(horizontal = 10.dp, vertical = 6.dp)
  ) {
    Text(stringResource(MR.strings.invalid_data), color = Color.Red, fontStyle = FontStyle.Italic)
  }
}

@Composable
fun InvalidJSONView(json: String) {
  Column {
    Spacer(Modifier.height(DEFAULT_PADDING))
    SectionView {
      val clipboard = LocalClipboardManager.current
      SettingsActionItem(painterResource(MR.images.ic_share), generalGetString(MR.strings.share_verb), click = {
        clipboard.shareText(json)
      })
    }
    Column(Modifier.padding(DEFAULT_PADDING).fillMaxWidth().verticalScroll(rememberScrollState())) {
      Text(json)
    }
  }
}
