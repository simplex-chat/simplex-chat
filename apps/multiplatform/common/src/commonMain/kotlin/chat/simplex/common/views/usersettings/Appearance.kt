package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionView
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.grid.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
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
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.views.chat.item.PreviewChatItemView
import chat.simplex.res.MR
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.ceil
import kotlin.math.roundToInt

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
  fun ChatThemePreview(backgroundImage: ImageBitmap?, backgroundImageType: BackgroundImageType?, withMessages: Boolean = true) {
    val themeBackgroundColor = MaterialTheme.colors.background
    val defaultBackgroundColor = backgroundImageType?.defaultBackgroundColor
    val defaultTintColor = backgroundImageType?.defaultTintColor
    Column(Modifier
      .drawBehind {
        if (backgroundImage != null && backgroundImageType != null && defaultBackgroundColor != null && defaultTintColor != null) {
          chatViewBackground(backgroundImage, backgroundImageType, defaultBackgroundColor, defaultTintColor)
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
  fun CustomizeBackgroundImageView() {
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
    ) {
      AppBarTitle(stringResource(MR.strings.choose_background_image_title))

      val backgroundImage = CurrentColors.collectAsState().value.wallpaper.type?.image
      val backgroundImageType = CurrentColors.collectAsState().value.wallpaper.type
      if (backgroundImage != null && backgroundImageType != null) {
        ChatThemePreview(backgroundImage, backgroundImageType)

        SectionSpacer()
        val isInDarkTheme = isInDarkTheme()
        val resetColors = {
          ThemeManager.saveAndApplyThemeColor(ThemeColor.WALLPAPER_BACKGROUND, null, isInDarkTheme)
          ThemeManager.saveAndApplyThemeColor(ThemeColor.WALLPAPER_TINT, null, isInDarkTheme)
        }
        val imageTypeState = remember {
          mutableStateOf(if (backgroundImageType is BackgroundImageType.Static) "" else backgroundImageType.filename)
        }
        val imageTypeValues = remember {
          PredefinedBackgroundImage.entries.map { it.filename to generalGetString(it.text) } + ("" to generalGetString(MR.strings.background_choose_own_image))
        }
        val systemDark = isSystemInDarkTheme()
        val importBackgroundImageLauncher = rememberFileChooserLauncher(true) { to: URI? ->
          if (to != null) {
            val filename = saveBackgroundImage(to)
            if (filename != null) {
              imageTypeState.value = ""
              ThemeManager.saveAndApplyBackgroundImage(BackgroundImageType.Static(filename, 1f, BackgroundImageScaleType.FILL), systemDark)
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
              ThemeManager.saveAndApplyBackgroundImage(PredefinedBackgroundImage.from(filename)!!.toType(), systemDark)
              removeBackgroundImages()
            }
          }
        )
        if (backgroundImageType is BackgroundImageType.Repeated) {
          val state = remember(backgroundImageType.scale) { mutableStateOf(backgroundImageType.scale) }
          Row {
            Text("${state.value}", Modifier.width(50.dp))
            Slider(
              state.value,
              valueRange = 0.2f..2f,
              onValueChange = {
                ThemeManager.saveAndApplyBackgroundImage(backgroundImageType.copy(scale = it), systemDark)
              }
            )
          }
        } else if (backgroundImageType is BackgroundImageType.Static) {
          val state = remember(backgroundImageType.scaleType) { mutableStateOf(backgroundImageType.scaleType) }
          val values = remember {
            BackgroundImageScaleType.entries.map { it to generalGetString(it.text) }
          }
          ExposedDropDownSettingRow(
            stringResource(MR.strings.background_image_scale),
            values,
            state,
            onSelected = { scaleType ->
              ThemeManager.saveAndApplyBackgroundImage(backgroundImageType.copy(scaleType = scaleType), systemDark)
            }
          )

          if (backgroundImageType.scaleType == BackgroundImageScaleType.REPEAT) {
            val state = remember(backgroundImageType.scale) { mutableStateOf(backgroundImageType.scale) }
            Row {
              Text("${state.value}", Modifier.width(50.dp))
              Slider(
                state.value,
                valueRange = 0.2f..2f,
                onValueChange = {
                  ThemeManager.saveAndApplyBackgroundImage(backgroundImageType.copy(scale = it), systemDark)
                }
              )
            }
          }
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
        val defaultBackgroundColor = backgroundImageType.defaultBackgroundColor
        val defaultTintColor = backgroundImageType.defaultTintColor
        if (selectedTab == 0) {
          var currentColor by remember(backgroundImageType.background) { mutableStateOf(backgroundImageType.background ?: defaultBackgroundColor) }
          ColorPicker(backgroundImageType.background ?: defaultBackgroundColor) {
            currentColor = it
            ThemeManager.saveAndApplyThemeColor(ThemeColor.WALLPAPER_BACKGROUND, it, isInDarkTheme)
          }
          val clipboard = LocalClipboardManager.current
          Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceEvenly) {
            Text(currentColor.toReadableHex(), modifier = Modifier.clickable { clipboard.shareText(currentColor.toReadableHex()) })
            Text("#" + currentColor.toReadableHex().substring(3), modifier = Modifier.clickable { clipboard.shareText("#" + currentColor.toReadableHex().substring(3)) })
          }
        } else {
          var currentColor by remember(backgroundImageType.tint) { mutableStateOf(backgroundImageType.tint ?: defaultTintColor) }
          ColorPicker(backgroundImageType.tint ?: defaultTintColor) {
            currentColor = it
            ThemeManager.saveAndApplyThemeColor(ThemeColor.WALLPAPER_TINT, it, isInDarkTheme)
          }
          val clipboard = LocalClipboardManager.current
          Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceEvenly) {
            Text(currentColor.toReadableHex(), modifier = Modifier.clickable { clipboard.shareText(currentColor.toReadableHex()) })
            Text("#" + currentColor.toReadableHex().substring(3), modifier = Modifier.clickable { clipboard.shareText("#" + currentColor.toReadableHex().substring(3)) })
          }
        }

        if (backgroundImageType.background != null || backgroundImageType.tint != null) {
          SectionSpacer()
          SectionItemView(resetColors) {
            Text(generalGetString(MR.strings.reset_color), color = colors.primary)
          }
        }
        SectionBottomSpacer()
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
    val systemDark = isSystemInDarkTheme()
    SectionView(stringResource(MR.strings.settings_section_title_themes)) {
      val selectedBackground = CurrentColors.collectAsState().value.wallpaper.type
      val cornerRadius = remember { appPreferences.profileImageCornerRadius.state }
      fun setBackground(type: BackgroundImageType?) {
        if (type is BackgroundImageType.Static) {
          ThemeManager.saveAndApplyThemeColor(ThemeColor.WALLPAPER_BACKGROUND, null, systemDark)
          ThemeManager.saveAndApplyThemeColor(ThemeColor.WALLPAPER_TINT, null, systemDark)
        }
        ThemeManager.saveAndApplyBackgroundImage(type, systemDark)
        removeBackgroundImages(type?.filename)
      }
      @Composable
      fun Checked() {
        Box(Modifier.size(40.dp).background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.9f), RoundedCornerShape(cornerRadius.value.roundToInt())))
        Icon(painterResource(MR.images.ic_check_filled), null, Modifier.size(25.dp), tint = MaterialTheme.colors.primary)
      }
      @Composable
      fun Plus() {
        Icon(painterResource(MR.images.ic_add), null, Modifier.size(25.dp), tint = MaterialTheme.colors.primary)
      }

      val backgrounds = PredefinedBackgroundImage.entries.toList()

      fun LazyGridScope.gridContent(width: Dp, height: Dp) {
        @Composable
        fun BackgroundItem(background: PredefinedBackgroundImage?) {
          Box(
            Modifier
            .size(width, height)
            .background(MaterialTheme.colors.background).clip(RoundedCornerShape(percent = cornerRadius.value.roundToInt()))
            .clickable { setBackground(background?.toType()) },
            contentAlignment = Alignment.Center
          ) {
            if (background != null) {
              val backgroundImage = remember(background.filename) { PredefinedBackgroundImage.from(background.filename)?.res?.toComposeImageBitmap() }
              ChatThemePreview(backgroundImage, background.toType(), withMessages = false)
            }
            if ((background == null && selectedBackground == null) || (selectedBackground?.filename == background?.filename)) {
              Checked()
            }
          }
        }

        @Composable
        fun OwnBackgroundItem(type: BackgroundImageType?) {
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
              .background(MaterialTheme.colors.background).clip(RoundedCornerShape(percent = cornerRadius.value.roundToInt()))
              .clickable {
                withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
              },
            contentAlignment = Alignment.Center
          ) {
            val backgroundImage = CurrentColors.collectAsState().value.wallpaper.type?.image
            if (type is BackgroundImageType.Static && backgroundImage != null) {
              ChatThemePreview(backgroundImage, type, withMessages = false)
              Checked()
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
          OwnBackgroundItem(CurrentColors.collectAsState().value.wallpaper.type)
        }
      }
      if (appPlatform.isDesktop) {
        val itemWidth = (DEFAULT_START_MODAL_WIDTH - DEFAULT_PADDING * 2 - DEFAULT_PADDING_HALF * 3) / 4
        val itemHeight = (DEFAULT_START_MODAL_WIDTH - DEFAULT_PADDING * 2) / 3
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
          Modifier.height(120.dp + DEFAULT_PADDING * 2),
          contentPadding = PaddingValues(DEFAULT_PADDING),
          horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF),
        ) {
          gridContent(80.dp, 120.dp)
        }
      }

      SectionItemView(showSettingsModal{ _ -> CustomizeBackgroundImageView() }) { Text(stringResource(MR.strings.choose_background_image_title)) }


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
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
    ) {
      val currentTheme by CurrentColors.collectAsState()

      AppBarTitle(stringResource(MR.strings.customize_theme_title))

      val backgroundImage = CurrentColors.collectAsState().value.wallpaper.type?.image
      val backgroundImageType = CurrentColors.collectAsState().value.wallpaper.type
      ChatThemePreview(backgroundImage, backgroundImageType)
      SectionSpacer()

      if (backgroundImageType != null) {
        SectionView(stringResource(MR.strings.settings_section_title_background_image).uppercase()) {
          val wallpaperBackgroundColor = currentTheme.wallpaper.background ?: backgroundImageType.defaultBackgroundColor
            SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_BACKGROUND, wallpaperBackgroundColor) }) {
              val title = generalGetString(MR.strings.color_wallpaper_background)
              Text(title)
              Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperBackgroundColor)
            }
          val wallpaperTintColor = currentTheme.wallpaper.tint ?: backgroundImageType.defaultTintColor
          SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_TINT, wallpaperTintColor) }) {
            val title = generalGetString(MR.strings.color_wallpaper_tint)
            Text(title)
            Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperTintColor)
          }
        }
        SectionSpacer()
      }

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
      if (currentTheme.base.hasChangedAnyColor(currentTheme.colors, currentTheme.appColors, currentTheme.wallpaper)) {
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

      val supportedLiveChange = name in listOf(ThemeColor.SECONDARY, ThemeColor.RECEIVED_MESSAGE, ThemeColor.SENT_MESSAGE, ThemeColor.WALLPAPER_BACKGROUND, ThemeColor.WALLPAPER_TINT)
      if (supportedLiveChange) {
        val backgroundImage = CurrentColors.collectAsState().value.wallpaper.type?.image
        val backgroundImageType = CurrentColors.collectAsState().value.wallpaper.type
        ChatThemePreview(backgroundImage, backgroundImageType)
        SectionSpacer()
      }

      val isInDarkTheme = isInDarkTheme()
      var currentColor by remember { mutableStateOf(initialColor) }
      ColorPicker(initialColor) {
        currentColor = it
        if (supportedLiveChange) {
          ThemeManager.saveAndApplyThemeColor(name, currentColor, isInDarkTheme)
        }
      }
      val savedColor = remember { mutableStateOf(initialColor) }
      DisposableEffect(Unit) {
        onDispose {
          if (currentColor != savedColor.value) {
            // Rollback changes since they weren't saved
            ThemeManager.saveAndApplyThemeColor(name, savedColor.value, isInDarkTheme)
          }
        }
      }

      SectionSpacer()
      TextButton(
        onClick = {
          ThemeManager.saveAndApplyThemeColor(name, currentColor, isInDarkTheme)
          savedColor.value = currentColor
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

@Composable
expect fun ColorPicker(initialColor: Color, onColorChanged: (Color) -> Unit)
