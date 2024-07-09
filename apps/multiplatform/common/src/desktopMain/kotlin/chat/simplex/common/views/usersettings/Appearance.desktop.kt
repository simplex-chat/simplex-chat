package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import java.util.Locale
import kotlin.math.roundToInt

@Composable
actual fun AppearanceView(m: ChatModel) {
  AppearanceScope.AppearanceLayout(
    m.controller.appPrefs.appLanguage,
    m.controller.appPrefs.systemDarkTheme,
  )
}

@Composable
fun AppearanceScope.AppearanceLayout(
  languagePref: SharedPreference<String?>,
  systemDarkTheme: SharedPreference<String?>,
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
    ThemesSection(systemDarkTheme)

    SectionDividerSpaced(maxTopPadding = true)
    ProfileImageSection()

    SectionDividerSpaced(maxBottomPadding = true)
    FontScaleSection()

    SectionBottomSpacer()
  }
}

@Composable
fun FontScaleSection() {
  val scaleValues = (setOf(0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 1f, 1.1f, 1.2f, 1.3f, 1.4f, 1.5f, 1.6f, 1.7f, 1.8f, 1.9f, 2f) + appPrefs.fontScale.state.value)
    .toList().sortedBy { it }.map { it to "${(it * 100).roundToInt()}%" }
  SectionView(stringResource(MR.strings.interface_preferences_section_title).uppercase()) {
    ExposedDropDownSettingRow(
      stringResource(MR.strings.appearance_font_scale),
      values = scaleValues,
      selection = appPrefs.fontScale.state,
      onSelected = { appPrefs.fontScale.set(it) }
    )
  }
}