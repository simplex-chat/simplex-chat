package chat.simplex.common.views.usersettings

import SectionItemView
import SectionView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import com.jakewharton.processphoenix.ProcessPhoenix
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun AdvancedSettingsAppSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
) {
  SectionView {
    SettingsActionItem(painterResource(MR.images.ic_code), stringResource(MR.strings.settings_developer_tools), showSettingsModal { DeveloperView(withAuth) })
  }
}

@Composable
actual fun AppShutdownItem() {
  SettingsActionItem(painterResource(MR.images.ic_power_settings_new), stringResource(MR.strings.settings_shutdown), ::shutdownAppAlert)
}

fun restartApp() {
  ProcessPhoenix.triggerRebirth(androidAppContext)
  shutdownApp()
}

private fun shutdownApp() {
  androidAppContext.getWorkManagerInstance().cancelAllWork()
  platform.androidServiceSafeStop()
  Runtime.getRuntime().exit(0)
}

private fun shutdownAppAlert() {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.shutdown_alert_question),
    text = generalGetString(MR.strings.shutdown_alert_desc),
    buttons = {
      Column {
        SectionItemView({ AlertManager.shared.hideAlert() }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          restartApp()
        }) {
          Text(stringResource(MR.strings.settings_restart_app), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          shutdownApp()
        }) {
          Text(stringResource(MR.strings.settings_shutdown), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
      }
    }
  )
}
