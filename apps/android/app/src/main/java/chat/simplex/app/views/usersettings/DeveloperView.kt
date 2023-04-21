package chat.simplex.app.views.usersettings

import SectionDivider
import SectionSpacer
import SectionTextFooter
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
import chat.simplex.app.views.helpers.*

@Composable
fun DeveloperView(
  m: ChatModel,
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  withAuth: (block: () -> Unit) -> Unit
) {
  Column(Modifier.fillMaxWidth()) {
    val uriHandler = LocalUriHandler.current
    AppBarTitle(stringResource(R.string.settings_developer_tools))
    val developerTools = m.controller.appPrefs.developerTools
    val devTools = remember { mutableStateOf(developerTools.get()) }
    SectionView() {
      InstallTerminalAppItem(uriHandler)
      SectionDivider()
      ChatConsoleItem { withAuth(showCustomModal { it, close -> TerminalView(it, close) }) }
      SectionDivider()
      SettingsPreferenceItem(Icons.Outlined.DriveFolderUpload, stringResource(R.string.confirm_database_upgrades), m.controller.appPrefs.confirmDBUpgrades)
      SectionDivider()
      SettingsPreferenceItem(Icons.Outlined.Code, stringResource(R.string.show_developer_options), developerTools, devTools)
    }
    SectionTextFooter(
      generalGetString(if (devTools.value) R.string.show_dev_options else R.string.hide_dev_options) + " " +
        generalGetString(R.string.developer_options)
    )
    SectionSpacer()
  }
}
