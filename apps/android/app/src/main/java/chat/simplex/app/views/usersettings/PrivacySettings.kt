package chat.simplex.app.views.usersettings

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SettingsBackgroundLight
import chat.simplex.app.views.helpers.ModalView

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
      AutoAcceptImagesSection(chatModel.controller.appPrefs.privacyAcceptImages)
      divider()
      LinkPreviewsSection(chatModel.controller.appPrefs.privacyLinkPreviews)
    }
  }
}

@Composable private fun AutoAcceptImagesSection(prefAcceptImages: Preference<Boolean>) {
  SettingsItemView() {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(
        Icons.Outlined.Image,
        contentDescription = stringResource(R.string.auto_accept_images),
        tint = HighOrLowlight,
      )
      Spacer(Modifier.padding(horizontal = 4.dp))
      SharedPreferenceToggle(stringResource(R.string.auto_accept_images), prefAcceptImages)
    }
  }
}

@Composable private fun LinkPreviewsSection(prefLinkPreviews: Preference<Boolean>) {
  SettingsItemView() {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(
        Icons.Outlined.TravelExplore,
        contentDescription = stringResource(R.string.send_link_previews),
        tint = HighOrLowlight,
      )
      Spacer(Modifier.padding(horizontal = 4.dp))
      SharedPreferenceToggle(stringResource(R.string.send_link_previews), prefLinkPreviews)
    }
  }
}