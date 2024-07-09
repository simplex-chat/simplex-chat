package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
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

    SectionDividerSpaced(maxBottomPadding = true)
    DensityScaleSection()

    SectionBottomSpacer()
  }
}

@Composable
fun FontScaleSection() {
  val localFontScale = remember { mutableStateOf(appPrefs.fontScale.get()) }
  SectionView(stringResource(MR.strings.appearance_font_size).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Row(Modifier.padding(top = 10.dp), verticalAlignment = Alignment.CenterVertically) {
      Box(Modifier.size(60.dp)
        .background(MaterialTheme.colors.surface, RoundedCornerShape(percent = 22))
        .clip(RoundedCornerShape(percent = 22))
        .clickable {
          localFontScale.value = 1f
          appPrefs.fontScale.set(localFontScale.value)
        },
        contentAlignment = Alignment.Center) {
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

@Composable
fun DensityScaleSection() {
  val localDensityScale = remember { mutableStateOf(appPrefs.densityScale.get()) }
  SectionView(stringResource(MR.strings.appearance_zoom).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Row(Modifier.padding(top = 10.dp), verticalAlignment = Alignment.CenterVertically) {
      Box(Modifier.size(60.dp)
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
          Text("${(localDensityScale.value * 100).roundToInt()}%",
            color = if (localDensityScale.value == 1f) MaterialTheme.colors.primary else MaterialTheme.colors.onBackground,
            fontSize = 12.sp,
            maxLines = 1
          )
        }
      }
      Spacer(Modifier.width(10.dp))
      Slider(
        localDensityScale.value,
        valueRange = 1f..1.5f,
        steps = 10,
        onValueChange = {
          val diff = it % 0.05f
          localDensityScale.value = it + (if (diff >= 0.025f) -diff + 0.05f else -diff)
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
