package chat.simplex.app.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionView
import android.app.Activity
import android.content.ComponentName
import android.content.Context
import android.content.pm.PackageManager
import android.content.pm.PackageManager.COMPONENT_ENABLED_STATE_DEFAULT
import android.content.pm.PackageManager.COMPONENT_ENABLED_STATE_ENABLED
import android.net.Uri
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.graphics.drawable.toBitmap
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import com.godaddy.android.colorpicker.*
import kotlinx.coroutines.delay
import kotlinx.serialization.encodeToString
import java.io.BufferedOutputStream
import java.util.*
import kotlin.collections.ArrayList

enum class AppIcon(val resId: Int) {
  DEFAULT(R.mipmap.icon),
  DARK_BLUE(R.mipmap.icon_dark_blue),
}

@Composable
fun AppearanceView(m: ChatModel, showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)) {
  val appIcon = remember { mutableStateOf(findEnabledIcon()) }

  fun setAppIcon(newIcon: AppIcon) {
    if (appIcon.value == newIcon) return
    val newComponent = ComponentName(BuildConfig.APPLICATION_ID, "chat.simplex.app.MainActivity_${newIcon.name.lowercase()}")
    val oldComponent = ComponentName(BuildConfig.APPLICATION_ID, "chat.simplex.app.MainActivity_${appIcon.value.name.lowercase()}")
    SimplexApp.context.packageManager.setComponentEnabledSetting(
      newComponent,
      COMPONENT_ENABLED_STATE_ENABLED, PackageManager.DONT_KILL_APP
    )

    SimplexApp.context.packageManager.setComponentEnabledSetting(
      oldComponent,
      PackageManager.COMPONENT_ENABLED_STATE_DISABLED, PackageManager.DONT_KILL_APP
    )

    appIcon.value = newIcon
  }

  AppearanceLayout(
    appIcon,
    m.controller.appPrefs.appLanguage,
    m.controller.appPrefs.systemDarkTheme,
    changeIcon = ::setAppIcon,
    showSettingsModal = showSettingsModal,
    editColor = { name, initialColor ->
      ModalManager.shared.showModalCloseable { close ->
        ColorEditor(name, initialColor, close)
      }
    },
  )
}

@Composable fun AppearanceLayout(
  icon: MutableState<AppIcon>,
  languagePref: SharedPreference<String?>,
  systemDarkTheme: SharedPreference<String?>,
  changeIcon: (AppIcon) -> Unit,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  editColor: (ThemeColor, Color) -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.appearance_settings))
    SectionView(stringResource(R.string.settings_section_title_language), padding = PaddingValues()) {
      val context = LocalContext.current
//      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
//        SectionItemWithValue(
//          generalGetString(R.string.settings_section_title_language).lowercase().replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.US) else it.toString() },
//          remember { mutableStateOf("system") },
//          listOf(ValueTitleDesc("system", generalGetString(R.string.change_verb), "")),
//          onSelected = { openSystemLangPicker(context as? Activity ?: return@SectionItemWithValue) }
//        )
//      } else {
      val state = rememberSaveable { mutableStateOf(languagePref.get() ?: "system") }
      LangSelector(state) {
        state.value = it
        withApi {
          delay(200)
          val activity = context as? Activity
          if (activity != null) {
            if (it == "system") {
              saveAppLocale(languagePref, activity)
            } else {
              saveAppLocale(languagePref, activity, it)
            }
          }
        }
      }
//      }
    }
    SectionDividerSpaced()

    SectionView(stringResource(R.string.settings_section_title_icon), padding = PaddingValues(horizontal = DEFAULT_PADDING_HALF)) {
      LazyRow {
        items(AppIcon.values().size, { index -> AppIcon.values()[index] }) { index ->
          val item = AppIcon.values()[index]
          val mipmap = ContextCompat.getDrawable(LocalContext.current, item.resId)!!
          Image(
            bitmap = mipmap.toBitmap().asImageBitmap(),
            contentDescription = "",
            contentScale = ContentScale.Fit,
            modifier = Modifier
              .shadow(if (item == icon.value) 1.dp else 0.dp, ambientColor = colors.secondaryVariant)
              .size(70.dp)
              .clickable { changeIcon(item) }
              .padding(10.dp)
          )

          if (index + 1 != AppIcon.values().size) {
            Spacer(Modifier.padding(horizontal = 4.dp))
          }
        }
      }
    }

    SectionDividerSpaced(maxTopPadding = true)
    val currentTheme by CurrentColors.collectAsState()
    SectionView(stringResource(R.string.settings_section_title_themes)) {
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
    SectionItemView(showSettingsModal { _ -> CustomizeThemeView(editColor) }) { Text(stringResource(R.string.customize_theme_title)) }
    SectionBottomSpacer()
  }
}

@Composable
fun CustomizeThemeView(editColor: (ThemeColor, Color) -> Unit) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    val currentTheme by CurrentColors.collectAsState()

    AppBarTitle(stringResource(R.string.customize_theme_title))

    SectionView(stringResource(R.string.theme_colors_section_title)) {
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY, currentTheme.colors.primary) }) {
        val title = generalGetString(R.string.color_primary)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = colors.primary)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY_VARIANT, currentTheme.colors.primaryVariant) }) {
        val title = generalGetString(R.string.color_primary_variant)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = colors.primaryVariant)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY, currentTheme.colors.secondary) }) {
        val title = generalGetString(R.string.color_secondary)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = colors.secondary)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY_VARIANT, currentTheme.colors.secondaryVariant) }) {
        val title = generalGetString(R.string.color_secondary_variant)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = colors.secondaryVariant)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.BACKGROUND, currentTheme.colors.background) }) {
        val title = generalGetString(R.string.color_background)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = colors.background)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SURFACE, currentTheme.colors.surface) }) {
        val title = generalGetString(R.string.color_surface)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = colors.surface)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.TITLE, currentTheme.appColors.title) }) {
        val title = generalGetString(R.string.color_title)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = currentTheme.appColors.title)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SENT_MESSAGE, currentTheme.appColors.sentMessage) }) {
        val title = generalGetString(R.string.color_sent_message)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = currentTheme.appColors.sentMessage)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.RECEIVED_MESSAGE, currentTheme.appColors.receivedMessage) }) {
        val title = generalGetString(R.string.color_received_message)
        Text(title)
        Icon(painterResource(R.drawable.ic_circle_filled), title, tint = currentTheme.appColors.receivedMessage)
      }
    }
    val isInDarkTheme = isInDarkTheme()
    if (currentTheme.base.hasChangedAnyColor(currentTheme.colors, currentTheme.appColors)) {
      SectionItemView({ ThemeManager.resetAllThemeColors(darkForSystemTheme = isInDarkTheme) }) {
        Text(generalGetString(R.string.reset_color), color = colors.primary)
      }
    }
    SectionSpacer()
    SectionView {
      val context = LocalContext.current
      val theme = remember { mutableStateOf(null as String?) }
      val exportThemeLauncher = rememberSaveThemeLauncher(context, theme)
      SectionItemView({
        val overrides = ThemeManager.currentThemeOverridesForExport(isInDarkTheme)
        theme.value = yaml.encodeToString<ThemeOverrides>(overrides)
        exportThemeLauncher.launch("simplex.theme")
      }) {
        Text(generalGetString(R.string.export_theme), color = colors.primary)
      }

      val importThemeLauncher = rememberGetContentLauncher { uri: Uri? ->
        if (uri != null) {
          val theme = getThemeFromUri(uri)
          if (theme != null) {
            ThemeManager.saveAndApplyThemeOverrides(theme, isInDarkTheme)
          }
        }
      }
      // Can not limit to YAML mime type since it's unsupported by Android
      SectionItemView({ importThemeLauncher.launch("*/*") }) {
        Text(generalGetString(R.string.import_theme), color = colors.primary)
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
      Text(generalGetString(R.string.save_color))
    }
  }
}

@Composable
fun ColorPicker(initialColor: Color, onColorChanged: (Color) -> Unit) {
  ClassicColorPicker(
    color = initialColor,
    modifier = Modifier
      .fillMaxWidth()
      .height(300.dp),
    showAlphaBar = true,
    onColorChanged = { color: HsvColor ->
      onColorChanged(color.toColor())
    }
  )
}

@Composable
private fun LangSelector(state: State<String>, onSelected: (String) -> Unit) {
  // Should be the same as in app/build.gradle's `android.defaultConfig.resConfigs`
  val supportedLanguages = mapOf(
    "system" to generalGetString(R.string.language_system),
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
    generalGetString(R.string.settings_section_title_language).lowercase().replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.US) else it.toString() },
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
    generalGetString(R.string.theme),
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
    darkThemes.add(DefaultTheme.DARK.name to generalGetString(R.string.theme_dark))
    darkThemes.add(DefaultTheme.SIMPLEX.name to generalGetString(R.string.theme_simplex))
    mutableStateOf(darkThemes.toList())
  }
  ExposedDropDownSettingRow(
    generalGetString(R.string.dark_theme),
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

@Composable
private fun rememberSaveThemeLauncher(cxt: Context, theme: MutableState<String?>): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      try {
        destination?.let {
          val theme = theme.value
          if (theme != null) {
            val contentResolver = cxt.contentResolver
            contentResolver.openOutputStream(destination)?.let { stream ->
              BufferedOutputStream(stream).use { outputStream ->
                theme.byteInputStream().use { it.copyTo(outputStream) }
              }
              Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
            }
          }
        }
      } catch (e: Error) {
        Toast.makeText(cxt, generalGetString(R.string.error_saving_file), Toast.LENGTH_SHORT).show()
        Log.e(TAG, "rememberSaveThemeLauncher error saving theme $e")
      } finally {
        theme.value = null
      }
    }
  )

private fun findEnabledIcon(): AppIcon = AppIcon.values().first { icon ->
  SimplexApp.context.packageManager.getComponentEnabledSetting(
    ComponentName(BuildConfig.APPLICATION_ID, "chat.simplex.app.MainActivity_${icon.name.lowercase()}")
  ).let { it == COMPONENT_ENABLED_STATE_DEFAULT || it == COMPONENT_ENABLED_STATE_ENABLED }
}

@Preview(showBackground = true)
@Composable
fun PreviewAppearanceSettings() {
  SimpleXTheme {
    AppearanceLayout(
      icon = remember { mutableStateOf(AppIcon.DARK_BLUE) },
      languagePref = SharedPreference({ null }, {}),
      systemDarkTheme = SharedPreference({ null }, {}),
      changeIcon = {},
      showSettingsModal = { {} },
      editColor = { _, _ -> },
    )
  }
}
