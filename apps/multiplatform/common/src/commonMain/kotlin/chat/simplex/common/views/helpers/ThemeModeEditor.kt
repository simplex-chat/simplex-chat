package chat.simplex.common.views.helpers

import SectionBottomSpacer
import SectionItemView
import SectionSpacer
import SectionView
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.MaterialTheme
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.yaml
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.AppearanceScope.WallpaperPresetSelector
import chat.simplex.common.views.usersettings.AppearanceScope.editColor
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.serialization.encodeToString
import java.net.URI

@Composable
fun ModalData.UserWallpaperEditor(
  theme: ThemeModeOverride,
  applyToMode: DefaultThemeMode?,
  globalThemeUsed: MutableState<Boolean>,
  save: suspend (applyToMode: DefaultThemeMode?, ThemeModeOverride?) -> Unit
) {
  ColumnWithScrollBar(
    Modifier
      .fillMaxSize()
  ) {
    val applyToMode = remember { stateGetOrPutNullable("applyToMode") { applyToMode } }
    var showMore by remember { stateGetOrPut("showMore") { false } }
    val themeModeOverride = remember { stateGetOrPut("themeModeOverride") { theme } }
    val currentTheme by CurrentColors.collectAsState()

    AppBarTitle(stringResource(MR.strings.settings_section_title_user_theme))
    val wallpaperImage = MaterialTheme.wallpaper.type.image
    val wallpaperType = MaterialTheme.wallpaper.type

    val onTypeCopyFromSameTheme = { type: WallpaperType? ->
      if (type is WallpaperType.Image && chatModel.remoteHostId() != null) {
        false
      } else {
        ThemeManager.copyFromSameThemeOverrides(type, null, themeModeOverride)
        withBGApi { save(applyToMode.value, themeModeOverride.value) }
        globalThemeUsed.value = false
        true
      }
    }
    val preApplyGlobalIfNeeded = { type: WallpaperType? ->
      if (globalThemeUsed.value) {
        onTypeCopyFromSameTheme(type)
      }
    }
    val onTypeChange: (WallpaperType?) -> Unit = { type: WallpaperType? ->
      if (globalThemeUsed.value) {
        preApplyGlobalIfNeeded(type)
        // Saves copied static image instead of original from global theme
        ThemeManager.applyWallpaper(themeModeOverride.value.type, themeModeOverride)
      } else {
        ThemeManager.applyWallpaper(type, themeModeOverride)
      }
      withBGApi { save(applyToMode.value, themeModeOverride.value) }
    }

    val importWallpaperLauncher = rememberFileChooserLauncher(true) { to: URI? ->
      if (to != null) {
        val filename = saveWallpaperFile(to)
        if (filename != null) {
          onTypeChange(WallpaperType.Image(filename, 1f, WallpaperScaleType.FILL))
        }
      }
    }

    val currentColors = { type: WallpaperType? ->
      // If applying for :
      // - all themes: no overrides needed
      // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
      val perUserOverride = if (wallpaperType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
      ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.get())
    }
    val onChooseType: (WallpaperType?) -> Unit = { type: WallpaperType? ->
      when {
        // don't have image in parent or already selected wallpaper with custom image
        type is WallpaperType.Image && chatModel.remoteHostId() != null -> { /* do nothing */ }
        type is WallpaperType.Image && (wallpaperType is WallpaperType.Image || currentColors(type).wallpaper.type.image == null) -> withLongRunningApi { importWallpaperLauncher.launch("image/*") }
        type is WallpaperType.Image -> onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)
        themeModeOverride.value.type != type || currentTheme.wallpaper.type != type -> onTypeCopyFromSameTheme(type)
        else -> onTypeChange(type)
      }
    }

    val editColor = { name: ThemeColor ->
      editColor(
        name,
        wallpaperType,
        wallpaperImage,
        onColorChange = { color ->
          preApplyGlobalIfNeeded(themeModeOverride.value.type)
          ThemeManager.applyThemeColor(name, color, themeModeOverride)
          withBGApi { save(applyToMode.value, themeModeOverride.value) }
        }
      )
    }

    WallpaperPresetSelector(
      selectedWallpaper = wallpaperType,
      baseTheme = currentTheme.base,
      currentColors = { type ->
        // If applying for :
        // - all themes: no overrides needed
        // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
        val perUserOverride = if (wallpaperType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
        ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.get())
      },
      onChooseType = onChooseType
    )

    WallpaperSetupView(
      themeModeOverride.value.type,
      CurrentColors.collectAsState().value.base,
      currentTheme.wallpaper,
      currentTheme.appColors.sentMessage,
      currentTheme.appColors.sentQuote,
      currentTheme.appColors.receivedMessage,
      currentTheme.appColors.receivedQuote,
      editColor = { name -> editColor(name) },
      onTypeChange = onTypeChange,
    )

    SectionSpacer()

    if (!globalThemeUsed.value) {
      ResetToGlobalThemeButton(true) {
        themeModeOverride.value = ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
        globalThemeUsed.value = true
        withBGApi { save(applyToMode.value, null) }
      }
    }

    SetDefaultThemeButton {
      globalThemeUsed.value = false
      val lightBase = DefaultTheme.LIGHT
      val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else if (appPrefs.systemDarkTheme.get() == DefaultTheme.BLACK.themeName) DefaultTheme.BLACK else DefaultTheme.SIMPLEX
      val mode = themeModeOverride.value.mode
      withBGApi {
        // Saving for both modes in one place by changing mode once per save
        if (applyToMode.value == null) {
          val oppositeMode = if (mode == DefaultThemeMode.LIGHT) DefaultThemeMode.DARK else DefaultThemeMode.LIGHT
          save(oppositeMode, ThemeModeOverride.withFilledAppDefaults(oppositeMode, if (oppositeMode == DefaultThemeMode.LIGHT) lightBase else darkBase))
        }
        themeModeOverride.value = ThemeModeOverride.withFilledAppDefaults(mode, if (mode == DefaultThemeMode.LIGHT) lightBase else darkBase)
        save(themeModeOverride.value.mode, themeModeOverride.value)
      }
    }

    KeyChangeEffect(theme.mode) {
      themeModeOverride.value = theme
      if (applyToMode.value != null) {
        applyToMode.value = theme.mode
      }
    }

    // Applies updated global theme if current one tracks global theme
    KeyChangeEffect(CurrentColors.collectAsState().value) {
      if (globalThemeUsed.value) {
        themeModeOverride.value = ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
        globalThemeUsed.value = true
      }
    }

    SectionSpacer()

    if (showMore) {
      val values by remember { mutableStateOf(
        listOf(
          null to generalGetString(MR.strings.chat_theme_apply_to_all_modes),
          DefaultThemeMode.LIGHT to generalGetString(MR.strings.chat_theme_apply_to_light_mode),
          DefaultThemeMode.DARK to generalGetString(MR.strings.chat_theme_apply_to_dark_mode),
        )
      )
      }
      ExposedDropDownSettingRow(
        generalGetString(MR.strings.chat_theme_apply_to_mode),
        values,
        applyToMode,
        icon = null,
        enabled = remember { mutableStateOf(true) },
        onSelected = {
          applyToMode.value = it
          if (it != null && it != CurrentColors.value.base.mode) {
            val lightBase = DefaultTheme.LIGHT
            val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else if (appPrefs.systemDarkTheme.get() == DefaultTheme.BLACK.themeName) DefaultTheme.BLACK else DefaultTheme.SIMPLEX
            ThemeManager.applyTheme(if (it == DefaultThemeMode.LIGHT) lightBase.themeName else darkBase.themeName)
          }
        }
      )

      SectionSpacer()

      AppearanceScope.CustomizeThemeColorsSection(currentTheme, editColor = editColor)

      SectionSpacer()
      ImportExportThemeSection(null, remember { chatModel.currentUser }.value?.uiThemes) {
        withBGApi {
          themeModeOverride.value = it
          save(applyToMode.value, it)
        }
      }
    } else {
      AdvancedSettingsButton { showMore = true }
    }

    SectionBottomSpacer()
  }
}

@Composable
fun ModalData.ChatWallpaperEditor(
  theme: ThemeModeOverride,
  applyToMode: DefaultThemeMode?,
  globalThemeUsed: MutableState<Boolean>,
  save: suspend (applyToMode: DefaultThemeMode?, ThemeModeOverride?) -> Unit
) {
  ColumnWithScrollBar(
    Modifier
      .fillMaxSize()
  ) {
    val applyToMode = remember { stateGetOrPutNullable("applyToMode") { applyToMode } }
    var showMore by remember { stateGetOrPut("showMore") { false } }
    val themeModeOverride = remember { stateGetOrPut("themeModeOverride") { theme } }
    val currentTheme by remember(themeModeOverride.value, CurrentColors.collectAsState().value) {
      mutableStateOf(
        ThemeManager.currentColors(null, if (globalThemeUsed.value) null else themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
      )
    }

    AppBarTitle(stringResource(MR.strings.settings_section_title_chat_theme))

    val onTypeCopyFromSameTheme: (WallpaperType?) -> Boolean = { type ->
      if (type is WallpaperType.Image && chatModel.remoteHostId() != null) {
        false
      } else {
        val success = ThemeManager.copyFromSameThemeOverrides(type, chatModel.currentUser.value?.uiThemes?.preferredMode(!CurrentColors.value.colors.isLight), themeModeOverride)
        if (success) {
          withBGApi { save(applyToMode.value, themeModeOverride.value) }
          globalThemeUsed.value = false
        }
        success
      }
    }
    val preApplyGlobalIfNeeded = { type: WallpaperType? ->
      if (globalThemeUsed.value) {
        onTypeCopyFromSameTheme(type)
      }
    }
    val onTypeChange: (WallpaperType?) -> Unit = { type ->
      if (globalThemeUsed.value) {
        preApplyGlobalIfNeeded(type)
        // Saves copied static image instead of original from global theme
        ThemeManager.applyWallpaper(themeModeOverride.value.type, themeModeOverride)
      } else {
        ThemeManager.applyWallpaper(type, themeModeOverride)
      }
      withBGApi { save(applyToMode.value, themeModeOverride.value) }
    }

    val editColor: (ThemeColor) -> Unit = { name: ThemeColor ->
      ModalManager.end.showModal {
        val currentTheme by remember(themeModeOverride.value, CurrentColors.collectAsState().value) {
          mutableStateOf(
            ThemeManager.currentColors(null, themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
          )
        }
        val initialColor: Color = when (name) {
          ThemeColor.WALLPAPER_BACKGROUND -> currentTheme.wallpaper.background ?: Color.Transparent
          ThemeColor.WALLPAPER_TINT -> currentTheme.wallpaper.tint ?: Color.Transparent
          ThemeColor.PRIMARY -> currentTheme.colors.primary
          ThemeColor.PRIMARY_VARIANT -> currentTheme.colors.primaryVariant
          ThemeColor.SECONDARY -> currentTheme.colors.secondary
          ThemeColor.SECONDARY_VARIANT -> currentTheme.colors.secondaryVariant
          ThemeColor.BACKGROUND -> currentTheme.colors.background
          ThemeColor.SURFACE -> currentTheme.colors.surface
          ThemeColor.TITLE -> currentTheme.appColors.title
          ThemeColor.PRIMARY_VARIANT2 -> currentTheme.appColors.primaryVariant2
          ThemeColor.SENT_MESSAGE -> currentTheme.appColors.sentMessage
          ThemeColor.SENT_QUOTE -> currentTheme.appColors.sentQuote
          ThemeColor.RECEIVED_MESSAGE -> currentTheme.appColors.receivedMessage
          ThemeColor.RECEIVED_QUOTE -> currentTheme.appColors.receivedQuote
        }
        AppearanceScope.ColorEditor(
          name,
          initialColor,
          CurrentColors.collectAsState().value.base,
          themeModeOverride.value.type,
          themeModeOverride.value.type?.image,
          currentTheme.wallpaper.background,
          currentTheme.wallpaper.tint,
          currentColors = {
            ThemeManager.currentColors(null, themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
          },
          onColorChange = { color ->
            preApplyGlobalIfNeeded(themeModeOverride.value.type)
            ThemeManager.applyThemeColor(name, color, themeModeOverride)
            withBGApi { save(applyToMode.value, themeModeOverride.value) }
          }
        )
      }
    }

    val importWallpaperLauncher = rememberFileChooserLauncher(true) { to: URI? ->
      if (to != null) {
        val filename = saveWallpaperFile(to)
        if (filename != null) {
          // Delete only non-user image
          if (!globalThemeUsed.value) {
            removeWallpaperFile((themeModeOverride.value.type as? WallpaperType.Image)?.filename)
          }
          globalThemeUsed.value = false
          onTypeChange(WallpaperType.Image(filename, 1f, WallpaperScaleType.FILL))
        }
      }
    }

    val currentColors = { type: WallpaperType? ->
      ThemeManager.currentColors(type, if (type?.sameType(themeModeOverride.value.type) == true) themeModeOverride.value else null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
    }

    WallpaperPresetSelector(
      selectedWallpaper = currentTheme.wallpaper.type,
      activeBackgroundColor = currentTheme.wallpaper.background,
      activeTintColor = currentTheme.wallpaper.tint,
      baseTheme = CurrentColors.collectAsState().value.base,
      currentColors = { type -> currentColors(type) },
      onChooseType = { type ->
        when {
          type is WallpaperType.Image && chatModel.remoteHostId() != null -> { /* do nothing */ }
          type is WallpaperType.Image && ((themeModeOverride.value.type is WallpaperType.Image && !globalThemeUsed.value) || currentColors(type).wallpaper.type.image == null) -> {
            withLongRunningApi { importWallpaperLauncher.launch("image/*") }
          }
          type is WallpaperType.Image -> {
            if (!onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)) {
              withLongRunningApi { importWallpaperLauncher.launch("image/*") }
            }
          }
          globalThemeUsed.value || themeModeOverride.value.type != type -> {
            onTypeCopyFromSameTheme(type)
          }
          else -> {
            onTypeChange(type)
          }
        }
      },
    )

    WallpaperSetupView(
      themeModeOverride.value.type,
      CurrentColors.collectAsState().value.base,
      currentTheme.wallpaper,
      currentTheme.appColors.sentMessage,
      currentTheme.appColors.sentQuote,
      currentTheme.appColors.receivedMessage,
      currentTheme.appColors.receivedQuote,
      editColor = editColor,
      onTypeChange = onTypeChange,
    )

    SectionSpacer()

    if (!globalThemeUsed.value) {
      ResetToGlobalThemeButton(remember { chatModel.currentUser }.value?.uiThemes?.preferredMode(isInDarkTheme()) == null) {
        themeModeOverride.value = ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
        globalThemeUsed.value = true
        withBGApi { save(applyToMode.value, null) }
      }
    }

    SetDefaultThemeButton {
      globalThemeUsed.value = false
      val lightBase = DefaultTheme.LIGHT
      val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else if (appPrefs.systemDarkTheme.get() == DefaultTheme.BLACK.themeName) DefaultTheme.BLACK else DefaultTheme.SIMPLEX
      val mode = themeModeOverride.value.mode
      withBGApi {
        // Saving for both modes in one place by changing mode once per save
        if (applyToMode.value == null) {
          val oppositeMode = if (mode == DefaultThemeMode.LIGHT) DefaultThemeMode.DARK else DefaultThemeMode.LIGHT
          save(oppositeMode, ThemeModeOverride.withFilledAppDefaults(oppositeMode, if (oppositeMode == DefaultThemeMode.LIGHT) lightBase else darkBase))
        }
        themeModeOverride.value = ThemeModeOverride.withFilledAppDefaults(mode, if (mode == DefaultThemeMode.LIGHT) lightBase else darkBase)
        save(themeModeOverride.value.mode, themeModeOverride.value)
      }
    }

    KeyChangeEffect(theme.mode) {
      themeModeOverride.value = theme
      if (applyToMode.value != null) {
        applyToMode.value = theme.mode
      }
    }

    // Applies updated global theme if current one tracks global theme
    KeyChangeEffect(CurrentColors.collectAsState()) {
      if (globalThemeUsed.value) {
        themeModeOverride.value = ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
        globalThemeUsed.value = true
      }
    }

    SectionSpacer()

    if (showMore) {
      val values by remember { mutableStateOf(
        listOf(
          null to generalGetString(MR.strings.chat_theme_apply_to_all_modes),
          DefaultThemeMode.LIGHT to generalGetString(MR.strings.chat_theme_apply_to_light_mode),
          DefaultThemeMode.DARK to generalGetString(MR.strings.chat_theme_apply_to_dark_mode),
        )
      )
      }
      ExposedDropDownSettingRow(
        generalGetString(MR.strings.chat_theme_apply_to_mode),
        values,
        applyToMode,
        icon = null,
        enabled = remember { mutableStateOf(true) },
        onSelected = {
          applyToMode.value = it
          if (it != null && it != CurrentColors.value.base.mode) {
            val lightBase = DefaultTheme.LIGHT
            val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else if (appPrefs.systemDarkTheme.get() == DefaultTheme.BLACK.themeName) DefaultTheme.BLACK else DefaultTheme.SIMPLEX
            ThemeManager.applyTheme(if (it == DefaultThemeMode.LIGHT) lightBase.themeName else darkBase.themeName)
          }
        }
      )

      SectionSpacer()

      AppearanceScope.CustomizeThemeColorsSection(currentTheme, editColor = editColor)

      SectionSpacer()
      ImportExportThemeSection(themeModeOverride.value, remember { chatModel.currentUser }.value?.uiThemes) {
        withBGApi {
          themeModeOverride.value = it
          save(applyToMode.value, it)
        }
      }
    } else {
      AdvancedSettingsButton { showMore = true }
    }

    SectionBottomSpacer()
  }
}

@Composable
private fun ImportExportThemeSection(perChat: ThemeModeOverride?, perUser: ThemeModeOverrides?, save: (ThemeModeOverride) -> Unit) {
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
      val overrides = ThemeManager.currentThemeOverridesForExport(perChat, perUser)
      val lines = yaml.encodeToString<ThemeOverrides>(overrides).lines()
      // Removing theme id without using custom serializer or data class
      theme.value = lines.subList(1, lines.size).joinToString("\n")
      withLongRunningApi { exportThemeLauncher.launch("simplex.theme") }
    }) {
      Text(generalGetString(MR.strings.export_theme), color = colors.primary)
    }
    val importThemeLauncher = rememberFileChooserLauncher(true) { to: URI? ->
      if (to != null) {
        val theme = getThemeFromUri(to)
        if (theme != null) {
          val res = ThemeModeOverride(mode = theme.base.mode, colors = theme.colors, wallpaper = theme.wallpaper?.importFromString()).removeSameColors(theme.base)
          save(res)
        }
      }
    }
    // Can not limit to YAML mime type since it's unsupported by Android
    SectionItemView({ withLongRunningApi { importThemeLauncher.launch("*/*") } }) {
      Text(generalGetString(MR.strings.import_theme), color = colors.primary)
    }
  }
}

@Composable
private fun ResetToGlobalThemeButton(app: Boolean, onClick: () -> Unit) {
  SectionItemView(onClick) {
    Text(stringResource(if (app) MR.strings.chat_theme_reset_to_app_theme else MR.strings.chat_theme_reset_to_user_theme), color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun SetDefaultThemeButton(onClick: () -> Unit) {
  SectionItemView(onClick) {
    Text(stringResource(MR.strings.chat_theme_set_default_theme), color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun AdvancedSettingsButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_arrow_downward),
    stringResource(MR.strings.wallpaper_advanced_settings),
    click = onClick
  )
}
