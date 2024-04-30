package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionView
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalClipboardManager
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.views.chat.item.PreviewChatItemView
import chat.simplex.res.MR
import com.godaddy.android.colorpicker.*
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList

@Composable
expect fun AppearanceView(m: ChatModel, showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit))

object AppearanceScope {
  @Composable
  fun ProfileImageSection() {
    SectionView(stringResource(MR.strings.settings_section_title_profile_images).uppercase(), padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
      val image = remember { chatModel.currentUser }.value?.image
      Row(Modifier.padding(top = 10.dp), horizontalArrangement = Arrangement.Center, verticalAlignment = Alignment.CenterVertically) {
        val size = 60
        Box(Modifier.offset(x = -(size / 12).dp)) {
          if (!image.isNullOrEmpty()) {
            ProfileImage(size.dp, image, MR.images.ic_simplex_light, color = Color.Unspecified)
          } else {
            ProfileImage(size.dp, if (isInDarkTheme()) MR.images.ic_simplex_light else MR.images.ic_simplex_dark)
          }
        }
        Spacer(Modifier.width(DEFAULT_PADDING_HALF - (size / 12).dp))
        Slider(
          remember { appPreferences.profileImageCornerRadius.state }.value,
          valueRange = 0f..50f,
          steps = 20,
          onValueChange = {
            val diff = it % 2.5f
            appPreferences.profileImageCornerRadius.set(it + (if (diff >= 1.25f) -diff + 2.5f else -diff))
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
  fun BackgroundImageSection(
    showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  ) {
    SectionView(stringResource(MR.strings.settings_section_title_background_image).uppercase()) {
      SectionItemView(showSettingsModal{ _ -> CustomizeBackgroundImageView() }) { Text(stringResource(MR.strings.choose_background_image_title)) }
    }
  }

  @Composable
  fun CustomizeBackgroundImageView() {
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
    ) {
      AppBarTitle(stringResource(MR.strings.choose_background_image_title))

      val backgroundImage = remember { chatModel.backgroundImage }
      val backgroundImageType = remember { appPrefs.backgroundImageType.state }
      val defaultBackgroundColor = backgroundImageType.value.defaultBackgroundColor
      val defaultTintColor = backgroundImageType.value.defaultTintColor
      Column(Modifier
        .drawBehind { chatViewBackground(backgroundImage.value, backgroundImageType.value, defaultBackgroundColor, defaultTintColor) }
        .padding(DEFAULT_PADDING_HALF)
      ) {
        PreviewChatItemView(ChatItem.getSampleData(1, CIDirection.DirectRcv(), Clock.System.now(), stringResource(MR.strings.background_image_preview_hello_bob)))
        PreviewChatItemView(ChatItem.getSampleData(2, CIDirection.DirectSnd(), Clock.System.now(), stringResource(MR.strings.background_image_preview_hello_alice)))
      }

      SectionSpacer()

      val resetColors = { appPrefs.backgroundImageType.set(backgroundImageType.value.copyBackgroundColor(null).copyTintColor(null)) }

      val imageTypeState = remember {
        val type = appPrefs.backgroundImageType.get()
        mutableStateOf(if (type.custom) "" else type.filename)
      }
      val imageTypeValues = remember {
        PredefinedBackgroundImage.entries.map { it.filename to generalGetString(it.text) } + ("" to generalGetString(MR.strings.background_choose_own_image))
      }
      val importBackgroundImageLauncher = rememberFileChooserLauncher(true) { to: URI? ->
        if (to != null) {
          val res = saveBackgroundImage(to)
          if (res != null) {
            val (filename, backgroundImage) = res
            imageTypeState.value = ""
            chatModel.backgroundImage.value = backgroundImage
            appPrefs.backgroundImageType.set(BackgroundImageType.Static(custom = true, filename, BackgroundImageScale.CROP, null))
            removeBackgroundImages(filename)
            resetColors()
          }
        }
      }
      ExposedDropDownSettingRow(
        stringResource(MR.strings.settings_section_title_background_image),
        imageTypeValues,
        imageTypeState,
        onSelected = { filename ->
          if (filename.isEmpty()) {
            withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
          } else {
            imageTypeState.value = filename
            appPrefs.backgroundImageType.set(PredefinedBackgroundImage.from(filename)!!.type)
            chatModel.backgroundImage.value = getBackgroundImageOrDefault()
            removeBackgroundImages()
          }
        }
      )

      val type = backgroundImageType.value
      if (type is BackgroundImageType.Repeated) {
        val state = remember(type.scale) { mutableStateOf(type.scale) }
        val values = remember {
          listOf(
            0.25f to "0.25x",
            0.5f to "0.5x",
            0.75f to "0.75x",
            1f to "1x",
          )
        }
        ExposedDropDownSettingRow(
          stringResource(MR.strings.background_image_scale),
          values,
          state,
          onSelected = { scale ->
            appPrefs.backgroundImageType.set(type.copy(scale = scale))
          }
        )
      } else if (type is BackgroundImageType.Static) {
        val state = remember(type.scale) { mutableStateOf(type.scale) }
        val values = remember {
          BackgroundImageScale.entries.map { it to generalGetString(it.text) }
        }
        ExposedDropDownSettingRow(
          stringResource(MR.strings.background_image_scale),
          values,
          state,
          onSelected = { scale ->
            appPrefs.backgroundImageType.set(type.copy(scale = scale))
          }
        )
      }

      SectionSpacer()

      var selectedTab by rememberSaveable { mutableStateOf(0) }
      val availableTabs = listOf(
        stringResource(MR.strings.background_image_background_color),
        stringResource(MR.strings.background_image_tint_color),
      )
      TabRow(
        selectedTabIndex = selectedTab,
        backgroundColor = Color.Transparent,
        contentColor = MaterialTheme.colors.primary,
      ) {
        availableTabs.forEachIndexed { index, title ->
          Tab(
            selected = selectedTab == index,
            onClick = {
              selectedTab = index
            },
            text = { Text(title, fontSize = 13.sp) },
            selectedContentColor = MaterialTheme.colors.primary,
            unselectedContentColor = MaterialTheme.colors.secondary,
          )
        }
      }

      if (selectedTab == 0) {
        var currentColor by remember(backgroundImageType.value.background) { mutableStateOf(backgroundImageType.value.background ?: defaultBackgroundColor) }
        ColorPicker(backgroundImageType.value.background ?: defaultBackgroundColor) {
          currentColor = it
          appPrefs.backgroundImageType.set(appPrefs.backgroundImageType.get().copyBackgroundColor(currentColor))
        }

        val clipboard = LocalClipboardManager.current
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceEvenly) {
          Text(currentColor.toReadableHex(), modifier = Modifier.clickable { clipboard.shareText(currentColor.toReadableHex()) })
          Text("#" + currentColor.toReadableHex().substring(3), modifier = Modifier.clickable { clipboard.shareText("#" + currentColor.toReadableHex().substring(3)) })
        }
      } else {
        var currentColor by remember(backgroundImageType.value.tint) { mutableStateOf(backgroundImageType.value.tint ?: defaultTintColor) }
        ColorPicker(backgroundImageType.value.tint ?: defaultTintColor) {
          currentColor = it
          appPrefs.backgroundImageType.set(appPrefs.backgroundImageType.get().copyTintColor(currentColor))
        }

        val clipboard = LocalClipboardManager.current
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceEvenly) {
          Text(currentColor.toReadableHex(), modifier = Modifier.clickable { clipboard.shareText(currentColor.toReadableHex()) })
          Text("#" + currentColor.toReadableHex().substring(3), modifier = Modifier.clickable { clipboard.shareText("#" + currentColor.toReadableHex().substring(3)) })
        }
      }

      if (backgroundImageType.value.background != null || backgroundImageType.value.tint != null) {
        SectionSpacer()
        SectionItemView(resetColors) {
          Text(generalGetString(MR.strings.reset_color), color = colors.primary)
        }
      }
      SectionBottomSpacer()
    }
  }

  @Composable
  fun CustomizeThemeView(editColor: (ThemeColor, Color) -> Unit) {
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
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
          withLongRunningApi { exportThemeLauncher.launch("simplex.theme")}
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
        SectionItemView({ withLongRunningApi { importThemeLauncher.launch("*/*") } }) {
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
      "ar" to "العربية",
      "bg" to "Български",
      "cs" to "Čeština",
      "de" to "Deutsch",
      "es" to "Español",
      "fi" to "Suomi",
      "fr" to "Français",
      "hu" to "Magyar",
      "it" to "Italiano",
      "iw" to "עִברִית",
      "ja" to "日本語",
      "lt" to "Lietuvių",
      "nl" to "Nederlands",
      "pl" to "Polski",
      "pt-BR" to "Português, Brasil",
      "ru" to "Русский",
      "th" to "ภาษาไทย",
      "tr" to "Türkçe",
      "uk" to "Українська",
      "zh-CN" to "简体中文"
    )
    val values by remember(ChatController.appPrefs.appLanguage.state.value) { mutableStateOf(supportedLanguages.map { it.key to it.value }) }
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
    val darkTheme = chat.simplex.common.ui.theme.isSystemInDarkTheme()
    val values by remember(ChatController.appPrefs.appLanguage.state.value) {
      mutableStateOf(ThemeManager.allThemes(darkTheme).map { it.second.name to it.third })
    }
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

