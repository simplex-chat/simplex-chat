package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.defaultLocale
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.AppearanceScope.ColorEditor
import chat.simplex.res.MR
import com.godaddy.android.colorpicker.ClassicColorPicker
import com.godaddy.android.colorpicker.HsvColor
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import java.util.Locale

@Composable
actual fun AppearanceView(m: ChatModel, showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)) {
  val darkTheme = isSystemInDarkTheme()
  val baseTheme = CurrentColors.collectAsState().value.base
  val backgroundImage = MaterialTheme.wallpaper.type?.image
  val backgroundImageType = MaterialTheme.wallpaper.type
  AppearanceScope.AppearanceLayout(
    m.controller.appPrefs.appLanguage,
    m.controller.appPrefs.systemDarkTheme,
    showSettingsModal = showSettingsModal,
    editColor = { name, initialColor ->
      ModalManager.start.showModal {
        ColorEditor(name, initialColor, baseTheme, backgroundImageType, backgroundImage, onColorChange = { color -> ThemeManager.saveAndApplyThemeColor(baseTheme, name, color) })
      }
    },
  )
}

@Composable
fun AppearanceScope.AppearanceLayout(
  languagePref: SharedPreference<String?>,
  systemDarkTheme: SharedPreference<String?>,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  editColor: (ThemeColor, Color) -> Unit,
) {
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.appearance_settings))
    SectionView(stringResource(MR.strings.settings_section_title_language), padding = PaddingValues()) {
      val state = rememberSaveable { mutableStateOf(languagePref.get() ?: "system") }
      LangSelector(state) {
        state.value = it
        withApi {
          delay(200)
          if (it == "system") {
            languagePref.set(null)
            Locale.setDefault(defaultLocale)
          } else {
            languagePref.set(it)
            Locale.setDefault(Locale.forLanguageTag(it))
          }
        }
      }
    }
    SectionDividerSpaced(maxTopPadding = true)
    ThemesSection(systemDarkTheme, showSettingsModal, editColor)

    SectionDividerSpaced(maxTopPadding = true)
    ProfileImageSection()

    SectionBottomSpacer()
  }
}

@Composable
actual fun ColorPicker(initialColor: Color, onColorChanged: (Color) -> Unit) {
  ClassicColorPicker(modifier = Modifier
    .fillMaxWidth()
    .height(300.dp),
    color = HsvColor.from(color = initialColor), showAlphaBar = true,
    onColorChanged = { color: HsvColor ->
      onColorChanged(color.toColor())
    }
  )
}
