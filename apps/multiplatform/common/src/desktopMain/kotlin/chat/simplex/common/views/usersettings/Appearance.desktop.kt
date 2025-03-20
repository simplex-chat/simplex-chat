package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import java.util.Locale

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
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.appearance_settings))
    SectionView(stringResource(MR.strings.settings_section_title_language), contentPadding = PaddingValues()) {
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
      SettingsPreferenceItem(icon = null, stringResource(MR.strings.one_hand_ui), ChatModel.controller.appPrefs.oneHandUI)
      if (remember { appPrefs.oneHandUI.state }.value && !remember { appPrefs.chatBottomBar.state }.value) {
        SettingsPreferenceItem(icon = null, stringResource(MR.strings.chat_bottom_bar), ChatModel.controller.appPrefs.chatBottomBar)
      }
    }
    SectionDividerSpaced()
    ThemesSection(systemDarkTheme)

    SectionDividerSpaced()
    AppToolbarsSection()

    SectionDividerSpaced()
    MessageShapeSection()

    SectionDividerSpaced()
    ProfileImageSection()

    SectionDividerSpaced(maxTopPadding = true)
    FontScaleSection()

    SectionDividerSpaced(maxTopPadding = true)
    DensityScaleSection()

    SectionBottomSpacer()
  }
}

@Composable
fun DensityScaleSection() {
  val localDensityScale = remember { mutableStateOf(appPrefs.densityScale.get()) }
  SectionView(stringResource(MR.strings.appearance_zoom).uppercase(), contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Row(Modifier.padding(top = 10.dp), verticalAlignment = Alignment.CenterVertically) {
      Box(Modifier.size(50.dp)
        .background(MaterialTheme.colors.surface, RoundedCornerShape(percent = 22))
        .clip(RoundedCornerShape(percent = 22))
        .clickable {
          localDensityScale.value = 1f
          appPrefs.densityScale.set(localDensityScale.value)
        },
        contentAlignment = Alignment.Center) {
        CompositionLocalProvider(
          LocalDensity provides Density(LocalDensity.current.density * localDensityScale.value, LocalDensity.current.fontScale)
        ) {
          Text("${localDensityScale.value}",
            color = if (localDensityScale.value == 1f) MaterialTheme.colors.primary else MaterialTheme.colors.onBackground,
            fontSize = 12.sp,
            maxLines = 1
          )
        }
      }
      Spacer(Modifier.width(15.dp))
      Slider(
        localDensityScale.value,
        valueRange = 1f..2f,
        steps = 11,
        onValueChange = {
          val diff = it % 0.1f
          localDensityScale.value = String.format(Locale.US, "%.1f", it + (if (diff >= 0.05f) -diff + 0.1f else -diff)).toFloatOrNull() ?: 1f
        },
        onValueChangeFinished = {
          appPrefs.densityScale.set(localDensityScale.value)
        },
        colors = SliderDefaults.colors(
          activeTickColor = Color.Transparent,
          inactiveTickColor = Color.Transparent,
        )
      )
    }
  }
}
