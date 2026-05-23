package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.runtime.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.NetworkAndServersView
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun AdvancedSettingsView(
  chatModel: ChatModel,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
) {
  val stopped = chatModel.chatRunning.value == false
  val notificationsMode = remember { chatModel.controller.appPrefs.notificationsMode.state }
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.advanced_settings))

    SectionView {
      SettingsActionItem(painterResource(MR.images.ic_wifi_tethering), stringResource(MR.strings.network_and_servers), showCustomModal { _, close -> NetworkAndServersView(close) }, disabled = stopped)
      if (appPlatform == AppPlatform.ANDROID) {
        SettingsActionItem(painterResource(if (notificationsMode.value == NotificationsMode.OFF) MR.images.ic_bolt_off else MR.images.ic_bolt), stringResource(MR.strings.notifications), showSettingsModal { NotificationsSettingsView(it) }, disabled = stopped)
      }
      SettingsActionItem(painterResource(MR.images.ic_videocam), stringResource(MR.strings.settings_audio_video_calls), showSettingsModal { CallSettingsView(it, showModal) }, disabled = stopped)
    }
    SectionDividerSpaced()

    AdvancedSettingsAppSection(showSettingsModal, withAuth)
    SectionBottomSpacer()
  }
}

@Composable
expect fun AdvancedSettingsAppSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
)
