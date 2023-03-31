package chat.simplex.app.views.usersettings

import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.UploadFile
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.helpers.withApi

@Composable
fun ExperimentalFeaturesView(chatModel: ChatModel) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start
  ) {
    Text(
      stringResource(R.string.settings_experimental_features),
      style = MaterialTheme.typography.h1,
      modifier = Modifier.padding(start = 16.dp, bottom = 24.dp)
    )
    SectionView("") {
      SettingsPreferenceItem(Icons.Outlined.UploadFile, stringResource(R.string.settings_send_files_via_xftp), chatModel.controller.appPrefs.xftpSendEnabled) {
        withApi {
          chatModel.controller.apiSetXFTPConfig(chatModel.controller.getXFTPCfg())
        }
      }
    }
  }
}
