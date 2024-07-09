package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
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
  val localFontScale = remember { mutableStateOf(appPrefs.fontScale.get()) }
  SectionView(stringResource(MR.strings.appearance_font_size).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Row(Modifier.padding(top = 10.dp), verticalAlignment = Alignment.CenterVertically) {
      Box(Modifier.size(60.dp).background(MaterialTheme.colors.surface, RoundedCornerShape(percent = 22)), contentAlignment = Alignment.Center) {
        CompositionLocalProvider(
          LocalDensity provides Density(LocalDensity.current.density, localFontScale.value)
        ) {
          Text("Aa", color = if (localFontScale.value == 1f) MaterialTheme.colors.primary else MaterialTheme.colors.onBackground)
        }
      }
      Spacer(Modifier.width(10.dp))
//      Text("${(localFontScale.value * 100).roundToInt()}%", Modifier.width(70.dp), textAlign = TextAlign.Center, fontSize = 12.sp)
      Slider(
        localFontScale.value,
        valueRange = 0.5f..2f,
        steps = 15,
        onValueChange = {
          val diff = it % 0.1f
          localFontScale.value = it + (if (diff >= 0.05f) -diff + 0.1f else -diff)
        },
        onValueChangeFinished = {
          appPrefs.fontScale.set(localFontScale.value)
        },
        colors = SliderDefaults.colors(
          activeTickColor = Color.Transparent,
          inactiveTickColor = Color.Transparent,
        )
      )
    }
  }
}
