package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR
import chat.simplex.common.model.ChatModel
import chat.simplex.common.views.TerminalView
import chat.simplex.common.views.helpers.*

@Composable
fun DeveloperView(
  m: ChatModel,
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
    val uriHandler = LocalUriHandler.current
    AppBarTitle(stringResource(R.string.settings_developer_tools))
    val developerTools = m.controller.appPrefs.developerTools
    val devTools = remember { developerTools.state }
    SectionView() {
      InstallTerminalAppItem(uriHandler)
      ChatConsoleItem { withAuth(generalGetString(R.string.auth_open_chat_console), generalGetString(R.string.auth_log_in_using_credential), showCustomModal { it, close -> TerminalView(it, close) })}
      SettingsPreferenceItem(painterResource(R.drawable.ic_drive_folder_upload), stringResource(R.string.confirm_database_upgrades), m.controller.appPrefs.confirmDBUpgrades)
      SettingsPreferenceItem(painterResource(R.drawable.ic_code), stringResource(R.string.show_developer_options), developerTools)
    }
    SectionTextFooter(
      generalGetString(if (devTools.value) R.string.show_dev_options else R.string.hide_dev_options) + " " +
        generalGetString(R.string.developer_options)
    )
    SectionBottomSpacer()
  }
}
