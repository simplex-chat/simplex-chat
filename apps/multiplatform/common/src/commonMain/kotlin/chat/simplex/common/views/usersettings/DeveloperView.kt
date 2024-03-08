package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import chat.simplex.common.model.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.appPreferences
import chat.simplex.common.views.TerminalView
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun DeveloperView(
  m: ChatModel,
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
    val uriHandler = LocalUriHandler.current
    AppBarTitle(stringResource(MR.strings.settings_developer_tools))
    val developerTools = m.controller.appPrefs.developerTools
    val devTools = remember { developerTools.state }
    SectionView {
      InstallTerminalAppItem(uriHandler)
      ChatConsoleItem { withAuth(generalGetString(MR.strings.auth_open_chat_console), generalGetString(MR.strings.auth_log_in_using_credential), showCustomModal { it, close -> TerminalView(it, close) }) }
      SettingsPreferenceItem(painterResource(MR.images.ic_code), stringResource(MR.strings.show_developer_options), developerTools)
      SectionTextFooter(
        generalGetString(if (devTools.value) MR.strings.show_dev_options else MR.strings.hide_dev_options) + " " +
            generalGetString(MR.strings.developer_options)
      )
    }
    if (devTools.value) {
      SectionSpacer()
      SectionView(stringResource(MR.strings.developer_options_section).uppercase()) {
        SettingsPreferenceItem(painterResource(MR.images.ic_drive_folder_upload), stringResource(MR.strings.confirm_database_upgrades), m.controller.appPrefs.confirmDBUpgrades)
        if (appPlatform.isDesktop) {
          TerminalAlwaysVisibleItem(m.controller.appPrefs.terminalAlwaysVisible) { checked ->
            if (checked) {
              withAuth(generalGetString(MR.strings.auth_open_chat_console), generalGetString(MR.strings.auth_log_in_using_credential)) {
                m.controller.appPrefs.terminalAlwaysVisible.set(true)
              }
            } else {
              m.controller.appPrefs.terminalAlwaysVisible.set(false)
            }
          }
        }
        SettingsPreferenceItem(painterResource(MR.images.ic_report), stringResource(MR.strings.show_internal_errors), appPreferences.showInternalErrors)
        SettingsPreferenceItem(painterResource(MR.images.ic_avg_pace), stringResource(MR.strings.show_slow_api_calls), appPreferences.showSlowApiCalls)
      }

      SectionSpacer()
      SectionView("Experimental".uppercase()) {
        SettingsPreferenceItem(painterResource(MR.images.ic_vpn_key_filled), "Post-quantum E2EE", m.controller.appPrefs.pqExperimentalEnabled, onChange = { enable ->
          withBGApi { m.controller.apiSetPQEnabled(enable) }
        })
        SectionTextFooter("In this version applies only to new contacts.")
      }
    }
    SectionBottomSpacer()
  }
}

fun showInDevelopingAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.in_developing_title),
    text = generalGetString(MR.strings.in_developing_desc)
  )
}
