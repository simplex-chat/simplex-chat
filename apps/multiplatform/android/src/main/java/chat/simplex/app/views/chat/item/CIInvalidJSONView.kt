package chat.simplex.app.views.chat.item

import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.SettingsActionItem

@Composable
fun CIInvalidJSONView(json: String) {
  Row(Modifier
    .clickable { ModalManager.shared.showModal(true) { InvalidJSONView(json) } }
    .padding(horizontal = 10.dp, vertical = 6.dp)
  ) {
    Text(stringResource(R.string.invalid_data), color = Color.Red, fontStyle = FontStyle.Italic)
  }
}

@Composable
fun InvalidJSONView(json: String) {
  Column {
    Spacer(Modifier.height(DEFAULT_PADDING))
    SectionView {
      val context = LocalContext.current
      SettingsActionItem(painterResource(R.drawable.ic_share), generalGetString(R.string.share_verb), click = {
        shareText(context, json)
      })
    }
    Column(Modifier.padding(DEFAULT_PADDING).fillMaxWidth().verticalScroll(rememberScrollState())) {
      Text(json)
    }
  }
}
