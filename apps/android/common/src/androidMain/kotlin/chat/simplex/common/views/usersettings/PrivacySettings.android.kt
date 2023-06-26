package chat.simplex.common.views.usersettings

import SectionView
import android.view.WindowManager
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.LocalContext
import androidx.fragment.app.FragmentActivity
import chat.simplex.common.model.ChatModel
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun PrivacyDeviceSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean) -> Unit,
) {
  SectionView(stringResource(MR.strings.settings_section_title_device)) {
    ChatLockItem(showSettingsModal, setPerformLA)
    val context = LocalContext.current
    SettingsPreferenceItem(painterResource(MR.images.ic_visibility_off), stringResource(MR.strings.protect_app_screen), ChatModel.controller.appPrefs.privacyProtectScreen) { on ->
      if (on) {
        (context as? FragmentActivity)?.window?.setFlags(
          WindowManager.LayoutParams.FLAG_SECURE,
          WindowManager.LayoutParams.FLAG_SECURE
        )
      } else {
        (context as? FragmentActivity)?.window?.clearFlags(WindowManager.LayoutParams.FLAG_SECURE)
      }
    }
  }
}