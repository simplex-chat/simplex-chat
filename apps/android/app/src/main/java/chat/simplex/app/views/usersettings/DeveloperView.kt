package chat.simplex.app.views.usersettings

import SectionDivider
import SectionView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.helpers.AppBarTitle

@Composable
fun DeveloperView(
  m: ChatModel,
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  withAuth: (block: () -> Unit) -> Unit
) {
  Column(Modifier.fillMaxWidth()) {
    val developerTools = m.controller.appPrefs.developerTools
    val confirmDBUpgrades = m.controller.appPrefs.confirmDBUpgrades
    val uriHandler = LocalUriHandler.current
    AppBarTitle(stringResource(R.string.settings_developer_tools))
    SectionView() {
      ChatConsoleItem { withAuth(showCustomModal { it, close -> TerminalView(it, close) }) }
      SectionDivider()
      val devTools = remember { mutableStateOf(developerTools.get()) }
      SettingsPreferenceItem(Icons.Outlined.Construction, stringResource(R.string.settings_developer_tools), developerTools, devTools)
      SectionDivider()
      var confirm = remember { mutableStateOf(confirmDBUpgrades.get()) }
      SettingsPreferenceItem(Icons.Outlined.DriveFolderUpload, stringResource(R.string.confirm_database_upgrades), confirmDBUpgrades, confirm)
      SectionDivider()
      InstallTerminalAppItem(uriHandler)
    }
  }
}

