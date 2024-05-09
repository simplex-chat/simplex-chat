package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.grid.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.ui.theme.isSystemInDarkTheme
import chat.simplex.common.views.chat.item.PreviewChatItemView
import chat.simplex.common.views.usersettings.AppearanceScope.WallpaperPresetSelector
import chat.simplex.res.MR
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.*

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
            saveThemeToDatabase()
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
  fun ChatThemePreview(
    theme: DefaultTheme,
    backgroundImage: ImageBitmap?,
    backgroundImageType: BackgroundImageType?,
    backgroundColor: Color? = MaterialTheme.wallpaper.background,
    tintColor: Color? = MaterialTheme.wallpaper.tint,
    withMessages: Boolean = true
  ) {
    val themeBackgroundColor = MaterialTheme.colors.background
    val backgroundColor =  backgroundColor ?: backgroundImageType?.defaultBackgroundColor(theme)
    val tintColor = tintColor ?: backgroundImageType?.defaultTintColor(theme)
    Column(Modifier
      .drawBehind {
        if (backgroundImage != null && backgroundImageType != null && backgroundColor != null && tintColor != null) {
          chatViewBackground(backgroundImage, backgroundImageType, backgroundColor, tintColor)
        } else {
          drawRect(themeBackgroundColor)
        }
      }
      .padding(DEFAULT_PADDING_HALF)
    ) {
      if (withMessages) {
        PreviewChatItemView(ChatItem.getSampleData(1, CIDirection.DirectRcv(), Clock.System.now(), stringResource(MR.strings.background_image_preview_hello_bob)))
        PreviewChatItemView(ChatItem.getSampleData(2, CIDirection.DirectSnd(), Clock.System.now(), stringResource(MR.strings.background_image_preview_hello_alice)))
      } else {
        Box(Modifier.fillMaxSize())
      }
    }
  }

  @Composable
  fun WallpaperPresetSelector(
    selectedBackground: BackgroundImageType?,
    baseTheme: DefaultTheme,
    initialBackgroundColor: Color?,
    initialTintColor: Color?,
    onColorChange: (ThemeColor, Color?) -> Unit,
    onTypeChange: (BackgroundImageType?) -> Unit
  ) {
    val resetColors = {
      onColorChange(ThemeColor.WALLPAPER_BACKGROUND, null)
      onColorChange(ThemeColor.WALLPAPER_TINT, null)
    }

    val cornerRadius = 22
    fun setBackground(type: BackgroundImageType?) {
      onTypeChange(type)
    }

    @Composable
    fun Plus() {
      Icon(painterResource(MR.images.ic_add), null, Modifier.size(25.dp), tint = MaterialTheme.colors.primary)
    }

    val backgrounds = PredefinedBackgroundImage.entries.toList()

    fun LazyGridScope.gridContent(width: Dp, height: Dp) {
      @Composable
      fun BackgroundItem(background: PredefinedBackgroundImage?) {
        val checked = (background == null && selectedBackground == null) || (selectedBackground?.filename == background?.filename)
        Box(
          Modifier
            .size(width, height)
            .background(MaterialTheme.colors.background)
            .clip(RoundedCornerShape(percent = cornerRadius))
            .border(1.dp, if (checked) MaterialTheme.colors.primary.copy(0.8f) else MaterialTheme.colors.onBackground.copy(0.1f), RoundedCornerShape(percent = cornerRadius))
            .clickable { setBackground(background?.toType()) },
          contentAlignment = Alignment.Center
        ) {
          if (background != null) {
            val backgroundImage = remember(background.filename) { PredefinedBackgroundImage.from(background.filename)?.res?.toComposeImageBitmap() }
            ChatThemePreview(baseTheme, backgroundImage, background.toType(), withMessages = false, backgroundColor = initialBackgroundColor, tintColor = initialTintColor)
          }
        }
      }

      @Composable
      fun OwnBackgroundItem(type: BackgroundImageType?) {
        val backgroundImage = selectedBackground?.image
        val checked = type is BackgroundImageType.Static && backgroundImage != null
        val importBackgroundImageLauncher = rememberFileChooserLauncher(true) { to: URI? ->
          if (to != null) {
            val filename = saveBackgroundImage(to)
            if (filename != null) {
              setBackground(BackgroundImageType.Static(filename, 1f, BackgroundImageScaleType.FILL))
            }
          }
        }
        Box(
          Modifier
            .size(width, height)
            .background(MaterialTheme.colors.background).clip(RoundedCornerShape(percent = cornerRadius))
            .border(1.dp, if (checked) MaterialTheme.colors.primary.copy(0.8f) else MaterialTheme.colors.onBackground.copy(0.1f), RoundedCornerShape(percent = cornerRadius))
            .clickable {
              withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
            },
          contentAlignment = Alignment.Center
        ) {
          if (checked) {
            ChatThemePreview(baseTheme, backgroundImage, type, withMessages = false, backgroundColor = initialBackgroundColor, tintColor = initialTintColor)
          } else {
            Plus()
          }
        }
      }

      item {
        BackgroundItem(null)
      }
      items(items = backgrounds) { background ->
        BackgroundItem(background)
      }
      item {
        OwnBackgroundItem(selectedBackground)
      }
    }
    val backgroundImage = selectedBackground?.image
    ChatThemePreview(baseTheme, backgroundImage, selectedBackground, backgroundColor = initialBackgroundColor, tintColor = initialTintColor)

    if (appPlatform.isDesktop) {
      val itemWidth = (DEFAULT_START_MODAL_WIDTH - DEFAULT_PADDING * 2 - DEFAULT_PADDING_HALF * 3) / 4
      val itemHeight = (DEFAULT_START_MODAL_WIDTH - DEFAULT_PADDING * 2) / 4
      val rows = ceil((PredefinedBackgroundImage.entries.size + 2) / 4f).roundToInt()
      LazyVerticalGrid(
        columns = GridCells.Fixed(4),
        Modifier.height(itemHeight * rows + DEFAULT_PADDING_HALF * (rows - 1) + DEFAULT_PADDING * 2),
        contentPadding = PaddingValues(DEFAULT_PADDING),
        verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF),
        horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF),
      ) {
        gridContent(itemWidth, itemHeight)
      }
    } else {
      LazyHorizontalGrid(
        rows = GridCells.Fixed(1),
        Modifier.height(80.dp + DEFAULT_PADDING * 2),
        contentPadding = PaddingValues(DEFAULT_PADDING),
        horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF),
      ) {
        gridContent(80.dp, 80.dp)
      }
    }
  }

  private var job: Job = Job()
  private fun saveThemeToDatabase() {
    job.cancel()
    job = withBGApi {
      delay(3000)
      controller.apiSaveAppSettings(AppSettings.current.prepareForExport())
    }
  }

  @Composable
  fun ThemesSection(
    systemDarkTheme: SharedPreference<String?>,
    showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
    editColor: (ThemeColor, Color) -> Unit
  ) {
    val currentTheme by CurrentColors.collectAsState()
    val baseTheme = CurrentColors.value.base
    val backgroundImageType = MaterialTheme.wallpaper.type
    SectionView(stringResource(MR.strings.settings_section_title_themes)) {
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      WallpaperPresetSelector(
        selectedBackground = backgroundImageType,
        baseTheme = currentTheme.base,
        initialBackgroundColor = MaterialTheme.wallpaper.background,
        initialTintColor = MaterialTheme.wallpaper.tint,
        onColorChange = { name, color ->
          ThemeManager.saveAndApplyThemeColor(baseTheme, name, color)
          saveThemeToDatabase()
        },
        onTypeChange = { type ->
          removeBackgroundImage(backgroundImageType?.filename)
          ThemeManager.saveAndApplyBackgroundImage(baseTheme, type)
          saveThemeToDatabase()
        }
      )

      val darkTheme = isSystemInDarkTheme()
      val state = remember { derivedStateOf { currentTheme.name } }
      ThemeSelector(state) {
        ThemeManager.applyTheme(it, darkTheme)
        saveThemeToDatabase()
      }
      if (state.value == DefaultTheme.SYSTEM.themeName) {
        DarkThemeSelector(remember { systemDarkTheme.state }) {
          ThemeManager.changeDarkTheme(it, darkTheme)
          saveThemeToDatabase()
        }
      }
    }
    SectionItemView(showSettingsModal { _ -> CustomizeThemeView(editColor) }) { Text(stringResource(MR.strings.customize_theme_title)) }
  }

  @Composable
  fun CustomizeThemeView(editColor: (ThemeColor, Color) -> Unit) {
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
    ) {
      val currentTheme by CurrentColors.collectAsState()

      AppBarTitle(stringResource(MR.strings.customize_theme_title))
      val backgroundImage = MaterialTheme.wallpaper.type?.image
      val backgroundImageType = MaterialTheme.wallpaper.type
      val baseTheme = CurrentColors.value.base
      ChatThemePreview(baseTheme, backgroundImage, backgroundImageType)
      SectionSpacer()

      if (backgroundImageType != null) {
        SectionView(stringResource(MR.strings.settings_section_title_wallpaper).uppercase()) {
          WallpaperSetupView(
            backgroundImageType,
            baseTheme,
            MaterialTheme.wallpaper.background,
            MaterialTheme.wallpaper.tint,
            showPresetSelection = false,
            editColor,
            onColorChange = { name, color ->
              ThemeManager.saveAndApplyThemeColor(baseTheme, name, color)
              saveThemeToDatabase()
            },
            onTypeChange = { type ->
              removeBackgroundImage(backgroundImageType.filename)
              ThemeManager.saveAndApplyBackgroundImage(baseTheme, type)
              saveThemeToDatabase()
            }
          )
        }
        SectionDividerSpaced(maxTopPadding = true)
      }

      CustomizeThemeColorsSection(currentTheme) { name, color ->
        editColor(name, color)
        saveThemeToDatabase()
      }

      val isInDarkTheme = isInDarkTheme()
      if (currentTheme.base.hasChangedAnyColor(currentTheme.colors, currentTheme.appColors, currentTheme.wallpaper)) {
        SectionItemView({
          ThemeManager.resetAllThemeColors(darkForSystemTheme = isInDarkTheme)
          saveThemeToDatabase()
        }) {
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
          val overrides = ThemeManager.currentThemeOverridesForExport(isInDarkTheme, null, chatModel.currentUser.value?.uiThemes)
          theme.value = yaml.encodeToString<ThemeOverrides>(overrides)
          withLongRunningApi { exportThemeLauncher.launch("simplex.theme")}
        }) {
          Text(generalGetString(MR.strings.export_theme), color = colors.primary)
        }
        val importThemeLauncher = rememberFileChooserLauncher(true) { to: URI? ->
          if (to != null) {
            val theme = getThemeFromUri(to)
            if (theme != null) {
              ThemeManager.saveAndApplyThemeOverrides(theme)
              saveThemeToDatabase()
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
  fun CustomizeThemeColorsSection(currentTheme: ThemeManager.ActiveTheme, editColor: (ThemeColor, Color) -> Unit) {
    SectionView(stringResource(MR.strings.theme_colors_section_title)) {
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY, currentTheme.colors.primary) }) {
        val title = generalGetString(MR.strings.color_primary)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.primary)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY_VARIANT, currentTheme.colors.primaryVariant) }) {
        val title = generalGetString(MR.strings.color_primary_variant)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.primaryVariant)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY, currentTheme.colors.secondary) }) {
        val title = generalGetString(MR.strings.color_secondary)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.secondary)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY_VARIANT, currentTheme.colors.secondaryVariant) }) {
        val title = generalGetString(MR.strings.color_secondary_variant)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.secondaryVariant)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.BACKGROUND, currentTheme.colors.background) }) {
        val title = generalGetString(MR.strings.color_background)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.background)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SURFACE, currentTheme.colors.surface) }) {
        val title = generalGetString(MR.strings.color_surface)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.surface)
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
  }

  @Composable
  fun ColorEditor(
    name: ThemeColor,
    initialColor: Color,
    theme: DefaultTheme,
    backgroundImageType: BackgroundImageType?,
    backgroundImage: ImageBitmap?,
    previewBackgroundColor: Color? = MaterialTheme.wallpaper.background,
    previewTintColor: Color? = MaterialTheme.wallpaper.tint,
    onColorChange: (Color) -> Unit,
    close: () -> Unit,
  ) {
    ColumnWithScrollBar(
      Modifier
        .fillMaxWidth()
    ) {
      AppBarTitle(name.text)

      val supportedLiveChange = name in listOf(ThemeColor.SECONDARY, ThemeColor.RECEIVED_MESSAGE, ThemeColor.SENT_MESSAGE, ThemeColor.WALLPAPER_BACKGROUND, ThemeColor.WALLPAPER_TINT)
      if (supportedLiveChange) {
        ChatThemePreview(theme, backgroundImage, backgroundImageType, previewBackgroundColor, previewTintColor)
        SectionSpacer()
      }

      var currentColor by remember { mutableStateOf(initialColor) }
      ColorPicker(initialColor) {
        currentColor = it
        if (supportedLiveChange) {
          onColorChange(currentColor)
        }
      }
      val clipboard = LocalClipboardManager.current
      Row(Modifier.fillMaxWidth().padding(if (appPlatform.isAndroid) 50.dp else 0.dp), horizontalArrangement = Arrangement.SpaceEvenly) {
        Text(currentColor.toReadableHex(), modifier = Modifier.clickable { clipboard.shareText(currentColor.toReadableHex()) })
        Text("#" + currentColor.toReadableHex().substring(3), modifier = Modifier.clickable { clipboard.shareText("#" + currentColor.toReadableHex().substring(3)) })
      }
      val savedColor = remember { mutableStateOf(initialColor) }
      DisposableEffect(Unit) {
        onDispose {
          if (currentColor != savedColor.value) {
            // Rollback changes since they weren't saved
            onColorChange(savedColor.value)
          }
        }
      }

      SectionSpacer()
      Row(Modifier.align(Alignment.CenterHorizontally)) {
        Box(Modifier.size(80.dp, 40.dp).background(savedColor.value).clickable {
          onColorChange(savedColor.value)
          currentColor = savedColor.value
        })
        Box(Modifier.size(80.dp, 40.dp).background(currentColor))
      }
      SectionSpacer()
      TextButton(
        onClick = {
          onColorChange(currentColor)
          savedColor.value = currentColor
          close()
        },
        Modifier.align(Alignment.CenterHorizontally),
        colors = ButtonDefaults.textButtonColors(contentColor = MaterialTheme.colors.primary)
      ) {
        Text(generalGetString(MR.strings.save_color))
      }
    }
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
    val darkTheme = isSystemInDarkTheme()
    val values by remember(ChatController.appPrefs.appLanguage.state.value) {
      mutableStateOf(ThemeManager.allThemes(darkTheme).map { it.second.themeName to it.third })
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
      darkThemes.add(DefaultTheme.DARK.themeName to generalGetString(MR.strings.theme_dark))
      darkThemes.add(DefaultTheme.SIMPLEX.themeName to generalGetString(MR.strings.theme_simplex))
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

@Composable
fun WallpaperSetupView(
  backgroundImageType: BackgroundImageType?,
  theme: DefaultTheme,
  initialBackgroundColor: Color?,
  initialTintColor: Color?,
  showPresetSelection: Boolean,
  editColor: (ThemeColor, Color) -> Unit,
  onColorChange: (ThemeColor, Color?) -> Unit,
  onTypeChange: (BackgroundImageType?) -> Unit
) {
  if (showPresetSelection) {
    WallpaperPresetSelector(
      selectedBackground = backgroundImageType,
      baseTheme = theme,
      initialBackgroundColor = initialBackgroundColor,
      initialTintColor = initialTintColor,
      onColorChange = onColorChange,
      onTypeChange = onTypeChange,
    )
  }

  if (backgroundImageType is BackgroundImageType.Static) {
    val state = remember(backgroundImageType.scaleType) { mutableStateOf(backgroundImageType.scaleType) }
    val values = remember {
      BackgroundImageScaleType.entries.map { it to generalGetString(it.text) }
    }
    ExposedDropDownSettingRow(
      stringResource(MR.strings.background_image_scale),
      values,
      state,
      onSelected = { scaleType ->
        onTypeChange(backgroundImageType.copy(scaleType = scaleType))
      }
    )
  }

  if (backgroundImageType is BackgroundImageType.Repeated || backgroundImageType is BackgroundImageType.Static && backgroundImageType.scaleType == BackgroundImageScaleType.REPEAT) {
    val state = remember(backgroundImageType.scale) { mutableStateOf(backgroundImageType.scale) }
    Row(Modifier.padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
      Text("${state.value}".substring(0, min("${state.value}".length, 4)), Modifier.width(50.dp))
      Slider(
        state.value,
        valueRange = 0.2f..2f,
        onValueChange = {
          if (backgroundImageType is BackgroundImageType.Repeated) {
            onTypeChange(backgroundImageType.copy(scale = it))
          } else if (backgroundImageType is BackgroundImageType.Static) {
            onTypeChange(backgroundImageType.copy(scale = it))
          }
        }
      )
    }
  }

  if (backgroundImageType != null) {
    val wallpaperBackgroundColor = initialBackgroundColor ?: backgroundImageType.defaultBackgroundColor(theme)
    SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_BACKGROUND, wallpaperBackgroundColor) }) {
      val title = generalGetString(MR.strings.color_wallpaper_background)
      Text(title)
      Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperBackgroundColor)
    }
    val wallpaperTintColor = initialTintColor ?: backgroundImageType.defaultTintColor(theme)
    SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_TINT, wallpaperTintColor) }) {
      val title = generalGetString(MR.strings.color_wallpaper_tint)
      Text(title)
      Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperTintColor)
    }
  }
}

@Composable
expect fun ColorPicker(initialColor: Color, onColorChanged: (Color) -> Unit)
