package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import com.godaddy.android.colorpicker.*
import kotlinx.serialization.encodeToString
import java.io.File
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList

@Composable
expect fun AppearanceView(m: ChatModel, showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit))

object AppearanceScope {
  @Composable
  fun ThemesSection(
    systemDarkTheme: SharedPreference<String?>,
    showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
    editColor: (ThemeColor, Color) -> Unit
  ) {
    val currentTheme by CurrentColors.collectAsState()
    SectionView(stringResource(MR.strings.settings_section_title_themes)) {
      val darkTheme = isSystemInDarkTheme()
      val state = remember { derivedStateOf { currentTheme.name } }
      ThemeSelector(state) {
        ThemeManager.applyTheme(it, darkTheme)
      }
      if (state.value == DefaultTheme.SYSTEM.name) {
        DarkThemeSelector(remember { systemDarkTheme.state }) {
          ThemeManager.changeDarkTheme(it, darkTheme)
        }
      }
    }
    SectionItemView(showSettingsModal { _ -> CustomizeThemeView(editColor) }) { Text(stringResource(MR.strings.customize_theme_title)) }
  }

  @Composable
  fun CustomizeThemeView(editColor: (ThemeColor, Color) -> Unit) {
    Column(
      Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    ) {
      val currentTheme by CurrentColors.collectAsState()

      AppBarTitle(stringResource(MR.strings.customize_theme_title))

      SectionView(stringResource(MR.strings.theme_colors_section_title)) {
        SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY, currentTheme.colors.primary) }) {
          val title = generalGetString(MR.strings.color_primary)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = colors.primary)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY_VARIANT, currentTheme.colors.primaryVariant) }) {
          val title = generalGetString(MR.strings.color_primary_variant)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = colors.primaryVariant)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY, currentTheme.colors.secondary) }) {
          val title = generalGetString(MR.strings.color_secondary)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = colors.secondary)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY_VARIANT, currentTheme.colors.secondaryVariant) }) {
          val title = generalGetString(MR.strings.color_secondary_variant)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = colors.secondaryVariant)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.BACKGROUND, currentTheme.colors.background) }) {
          val title = generalGetString(MR.strings.color_background)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = colors.background)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.SURFACE, currentTheme.colors.surface) }) {
          val title = generalGetString(MR.strings.color_surface)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = colors.surface)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.TITLE, currentTheme.appColors.title) }) {
          val title = generalGetString(MR.strings.color_title)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.appColors.title)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.SENT_MESSAGE, currentTheme.appColors.sentMessage) }) {
          val title = generalGetString(MR.strings.color_sent_message)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.appColors.sentMessage)
        }
        SectionItemViewSpaceBetween({ editColor(ThemeColor.RECEIVED_MESSAGE, currentTheme.appColors.receivedMessage) }) {
          val title = generalGetString(MR.strings.color_received_message)
          Text(title)
          Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.appColors.receivedMessage)
        }
      }
      val isInDarkTheme = isInDarkTheme()
      if (currentTheme.base.hasChangedAnyColor(currentTheme.colors, currentTheme.appColors)) {
        SectionItemView({ ThemeManager.resetAllThemeColors(darkForSystemTheme = isInDarkTheme) }) {
          Text(generalGetString(MR.strings.reset_color), color = colors.primary)
        }
      }
      SectionSpacer()
      SectionView {
        val theme = remember { mutableStateOf(null as String?) }
        val exportThemeLauncher = rememberFileChooserLauncher(false) { to: URI? ->
          val themeValue = theme.value
          if (themeValue != null && to != null) {
            copyBytesToFile(themeValue.byteInputStream(), to) {
              theme.value = null
            }
          }
        }
        SectionItemView({
          val overrides = ThemeManager.currentThemeOverridesForExport(isInDarkTheme)
          theme.value = yaml.encodeToString<ThemeOverrides>(overrides)
          withApi { exportThemeLauncher.launch("simplex.theme")}
        }) {
          Text(generalGetString(MR.strings.export_theme), color = colors.primary)
        }
        val importThemeLauncher = rememberFileChooserLauncher(true) { to: URI? ->
          if (to != null) {
            val theme = getThemeFromUri(to)
            if (theme != null) {
              ThemeManager.saveAndApplyThemeOverrides(theme, isInDarkTheme)
            }
          }
        }
        // Can not limit to YAML mime type since it's unsupported by Android
        SectionItemView({ withApi { importThemeLauncher.launch("*/*") } }) {
          Text(generalGetString(MR.strings.import_theme), color = colors.primary)
        }
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun ColorEditor(
    name: ThemeColor,
    initialColor: Color,
    close: () -> Unit,
  ) {
    Column(
      Modifier
        .fillMaxWidth()
    ) {
      AppBarTitle(name.text)
      var currentColor by remember { mutableStateOf(initialColor) }
      ColorPicker(initialColor) {
        currentColor = it
      }

      SectionSpacer()
      val isInDarkTheme = isInDarkTheme()
      TextButton(
        onClick = {
          ThemeManager.saveAndApplyThemeColor(name, currentColor, isInDarkTheme)
          close()
        },
        Modifier.align(Alignment.CenterHorizontally),
        colors = ButtonDefaults.textButtonColors(contentColor = currentColor)
      ) {
        Text(generalGetString(MR.strings.save_color))
      }
    }
  }

  @Composable
  fun ColorPicker(initialColor: Color, onColorChanged: (Color) -> Unit) {
    ClassicColorPicker(modifier = Modifier
      .fillMaxWidth()
      .height(300.dp),
      color = HsvColor.from(color = initialColor), showAlphaBar = true,
      onColorChanged = { color: HsvColor ->
        onColorChanged(color.toColor())
      }
    )
  }

  @Composable
  fun LangSelector(state: State<String>, onSelected: (String) -> Unit) {
    // Should be the same as in app/build.gradle's `android.defaultConfig.resConfigs`
    val supportedLanguages = mapOf(
      "system" to generalGetString(MR.strings.language_system),
      "en" to "English",
      "cs" to "Čeština",
      "de" to "Deutsch",
      "es" to "Español",
      "fr" to "Français",
      "it" to "Italiano",
      "ja" to "日本語",
      "nl" to "Nederlands",
      "pl" to "Polski",
      "pt-BR" to "Português (Brasil)",
      "ru" to "Русский",
      "zh-CN" to "简体中文"
    )
    val values by remember { mutableStateOf(supportedLanguages.map { it.key to it.value }) }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.settings_section_title_language).lowercase().replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.US) else it.toString() },
      values,
      state,
      icon = null,
      enabled = remember { mutableStateOf(true) },
      onSelected = onSelected
    )
  }

  @Composable
  private fun ThemeSelector(state: State<String>, onSelected: (String) -> Unit) {
    val darkTheme = isSystemInDarkTheme()
    val values by remember { mutableStateOf(ThemeManager.allThemes(darkTheme).map { it.second.name to it.third }) }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.theme),
      values,
      state,
      icon = null,
      enabled = remember { mutableStateOf(true) },
      onSelected = onSelected
    )
  }

  @Composable
  private fun DarkThemeSelector(state: State<String?>, onSelected: (String) -> Unit) {
    val values by remember {
      val darkThemes = ArrayList<Pair<String, String>>()
      darkThemes.add(DefaultTheme.DARK.name to generalGetString(MR.strings.theme_dark))
      darkThemes.add(DefaultTheme.SIMPLEX.name to generalGetString(MR.strings.theme_simplex))
      mutableStateOf(darkThemes.toList())
    }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.dark_theme),
      values,
      state,
      icon = null,
      enabled = remember { mutableStateOf(true) },
      onSelected = { if (it != null) onSelected(it) }
    )
  }
  //private fun openSystemLangPicker(activity: Activity) {
  //  activity.startActivity(Intent(Settings.ACTION_APP_LOCALE_SETTINGS, Uri.parse("package:" + SimplexApp.context.packageName)))
  //}
}

