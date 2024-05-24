package chat.simplex.common.views.helpers

import SectionBottomSpacer
import SectionItemView
import SectionSpacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.AppearanceScope.WallpaperPresetSelector
import chat.simplex.common.views.usersettings.AppearanceScope.editColor
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
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
    val backgroundImage = MaterialTheme.wallpaper.type.image
    val backgroundImageType = MaterialTheme.wallpaper.type

    val onTypeCopyFromSameTheme = { type: BackgroundImageType? ->
      if (type is BackgroundImageType.Static && chatModel.remoteHostId() != null) {
        false
      } else {
        ThemeManager.copyFromSameThemeOverrides(type, null, themeModeOverride)
        withBGApi { save(applyToMode.value, themeModeOverride.value) }
        globalThemeUsed.value = false
        true
      }
    }
    val preApplyGlobalIfNeeded = { type: BackgroundImageType? ->
      if (globalThemeUsed.value) {
        onTypeCopyFromSameTheme(type)
      }
    }
    val onTypeChange: (BackgroundImageType?) -> Unit = { type: BackgroundImageType? ->
      if (globalThemeUsed.value) {
        preApplyGlobalIfNeeded(type)
        // Saves copied static image instead of original from global theme
        ThemeManager.applyBackgroundImage(themeModeOverride.value.type, themeModeOverride)
      } else {
        ThemeManager.applyBackgroundImage(type, themeModeOverride)
      }
      withBGApi { save(applyToMode.value, themeModeOverride.value) }
    }

    val importBackgroundImageLauncher = rememberFileChooserLauncher(true) { to: URI? ->
      if (to != null) {
        val filename = saveBackgroundImage(to)
        if (filename != null) {
          onTypeChange(BackgroundImageType.Static(filename, 1f, BackgroundImageScaleType.FILL))
        }
      }
    }

    val currentColors = { type: BackgroundImageType? ->
      // If applying for :
      // - all themes: no overrides needed
      // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
      val perUserOverride = if (backgroundImageType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
      ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.get())
    }
    val onChooseType: (BackgroundImageType?) -> Unit = { type: BackgroundImageType? ->
      when {
        // don't have image in parent or already selected wallpaper with custom image
        type is BackgroundImageType.Static && chatModel.remoteHostId() != null -> { /* do nothing */ }
        type is BackgroundImageType.Static && (backgroundImageType is BackgroundImageType.Static || currentColors(type).wallpaper.type.image == null) -> withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
        type is BackgroundImageType.Static -> onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)
        themeModeOverride.value.type != type || currentTheme.wallpaper.type != type -> onTypeCopyFromSameTheme(type)
        else -> onTypeChange(type)
      }
    }

    val editColor = { name: ThemeColor ->
      editColor(
        name,
        backgroundImageType,
        backgroundImage,
        onColorChange = { color ->
          preApplyGlobalIfNeeded(themeModeOverride.value.type)
          ThemeManager.applyThemeColor(name, color, themeModeOverride)
          withBGApi { save(applyToMode.value, themeModeOverride.value) }
        }
      )
    }

    WallpaperPresetSelector(
      selectedBackground = backgroundImageType,
      baseTheme = currentTheme.base,
      currentColors = { type ->
        // If applying for :
        // - all themes: no overrides needed
        // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
        val perUserOverride = if (backgroundImageType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
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
      ResetToGlobalThemeButton {
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
    KeyChangeEffect(CurrentColors.value) {
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
        ThemeManager.currentColors(null, if (themeModeOverride.value == ThemeModeOverride()) null else themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
      )
    }

    AppBarTitle(stringResource(MR.strings.settings_section_title_chat_theme))

    val onTypeCopyFromSameTheme: (BackgroundImageType?) -> Boolean = { type ->
      if (type is BackgroundImageType.Static && chatModel.remoteHostId() != null) {
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
    val preApplyGlobalIfNeeded = { type: BackgroundImageType? ->
      if (globalThemeUsed.value) {
        onTypeCopyFromSameTheme(type)
      }
    }
    val onTypeChange: (BackgroundImageType?) -> Unit = { type ->
      if (globalThemeUsed.value) {
        preApplyGlobalIfNeeded(type)
        // Saves copied static image instead of original from global theme
        ThemeManager.applyBackgroundImage(themeModeOverride.value.type, themeModeOverride)
      } else {
        ThemeManager.applyBackgroundImage(type, themeModeOverride)
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

    val importBackgroundImageLauncher = rememberFileChooserLauncher(true) { to: URI? ->
      if (to != null) {
        val filename = saveBackgroundImage(to)
        if (filename != null) {
          // Delete only non-user image
          if (!globalThemeUsed.value) {
            removeBackgroundImage((themeModeOverride.value.type as? BackgroundImageType.Static)?.filename)
          }
          globalThemeUsed.value = false
          onTypeChange(BackgroundImageType.Static(filename, 1f, BackgroundImageScaleType.FILL))
        }
      }
    }

    val currentColors = { type: BackgroundImageType? ->
      ThemeManager.currentColors(type, if (type?.sameType(themeModeOverride.value.type) == true) themeModeOverride.value else null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
    }

    WallpaperPresetSelector(
      selectedBackground = currentTheme.wallpaper.type,
      activeBackgroundColor = currentTheme.wallpaper.background,
      activeTintColor = currentTheme.wallpaper.tint,
      baseTheme = CurrentColors.collectAsState().value.base,
      currentColors = { type -> currentColors(type) },
      onChooseType = { type ->
        when {
          type is BackgroundImageType.Static && chatModel.remoteHostId() != null -> { /* do nothing */ }
          type is BackgroundImageType.Static && ((themeModeOverride.value.type is BackgroundImageType.Static && !globalThemeUsed.value) || currentColors(type).wallpaper.type.image == null) -> {
            withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
          }
          type is BackgroundImageType.Static -> {
            if (!onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)) {
              withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
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
      ResetToGlobalThemeButton {
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
    KeyChangeEffect(CurrentColors.value) {
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
    } else {
      AdvancedSettingsButton { showMore = true }
    }

    SectionBottomSpacer()
  }
}

@Composable
private fun ResetToGlobalThemeButton(onClick: () -> Unit) {
  SectionItemView(onClick) {
    Text(stringResource(MR.strings.chat_theme_reset_to_global_theme), color = MaterialTheme.colors.primary)
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
    stringResource(MR.strings.background_image_advanced_settings),
    click = onClick
  )
}
