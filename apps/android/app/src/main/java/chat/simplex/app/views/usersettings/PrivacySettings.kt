package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Image
import androidx.compose.material.icons.outlined.TravelExplore
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel

@Composable
fun PrivacySettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit) {
  @Composable fun divider() = Divider(Modifier.padding(horizontal = 8.dp))
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start
  ) {
    Text(
      stringResource(R.string.your_privacy),
      style = MaterialTheme.typography.h1,
      modifier = Modifier.padding(start = 16.dp, bottom = 24.dp)
    )
    SettingsSectionView(stringResource(R.string.settings_section_title_device)) {
      ChatLockItem(chatModel.performLA, setPerformLA)
    }
    Spacer(Modifier.height(30.dp))

    SettingsSectionView(stringResource(R.string.settings_section_title_chats)) {
      SettingsPreferenceItem(Icons.Outlined.Image, stringResource(R.string.auto_accept_images), chatModel.controller.appPrefs.privacyAcceptImages)
      divider()
      SettingsPreferenceItem(Icons.Outlined.TravelExplore, stringResource(R.string.send_link_previews), chatModel.controller.appPrefs.privacyLinkPreviews)
    }
  }
}
