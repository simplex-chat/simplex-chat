package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import android.view.WindowManager
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.res.stringResource
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.*

@Composable
fun PrivacySettingsView(
  chatModel: ChatModel,
  setPerformLA: (Boolean) -> Unit
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start
  ) {
    val simplexLinkMode = chatModel.controller.appPrefs.simplexLinkMode
    AppBarTitle(stringResource(R.string.your_privacy))
    SectionView(stringResource(R.string.settings_section_title_device)) {
      ChatLockItem(chatModel.performLA, setPerformLA)
      SectionDivider()
      val context = LocalContext.current
      SettingsPreferenceItem(Icons.Outlined.VisibilityOff, stringResource(R.string.protect_app_screen), chatModel.controller.appPrefs.privacyProtectScreen) { on ->
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
    SectionSpacer()

    SectionView(stringResource(R.string.settings_section_title_chats)) {
      SettingsPreferenceItem(Icons.Outlined.Image, stringResource(R.string.auto_accept_images), chatModel.controller.appPrefs.privacyAcceptImages)
      SectionDivider()
      SettingsPreferenceItem(Icons.Outlined.ImageAspectRatio, stringResource(R.string.transfer_images_faster), chatModel.controller.appPrefs.privacyTransferImagesInline)
      SectionDivider()
      SettingsPreferenceItem(Icons.Outlined.TravelExplore, stringResource(R.string.send_link_previews), chatModel.controller.appPrefs.privacyLinkPreviews)
      SectionDivider()
      SectionItemView { SimpleXLinkOptions(chatModel.simplexLinkMode, onSelected = {
        simplexLinkMode.set(it)
        chatModel.simplexLinkMode.value = it
      }) }
    }
    if (chatModel.simplexLinkMode.value == SimplexLinkMode.BROWSER) {
      SectionTextFooter(stringResource(R.string.simplex_link_mode_browser_warning))
    }
  }
}

@Composable
private fun SimpleXLinkOptions(simplexLinkModeState: State<SimplexLinkMode>, onSelected: (SimplexLinkMode) -> Unit) {
  val values = remember {
    SimplexLinkMode.values().map {
      when (it) {
        SimplexLinkMode.DESCRIPTION -> it to generalGetString(R.string.simplex_link_mode_description)
        SimplexLinkMode.FULL -> it to generalGetString(R.string.simplex_link_mode_full)
        SimplexLinkMode.BROWSER -> it to generalGetString(R.string.simplex_link_mode_browser)
      }
    }
  }
  ExposedDropDownSettingRow(
    generalGetString(R.string.simplex_link_mode),
    values,
    simplexLinkModeState,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}
