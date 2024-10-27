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
import androidx.compose.foundation.shape.CircleShape
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
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import chat.simplex.common.views.chat.item.PreviewChatItemView
import chat.simplex.common.views.chat.item.msgTailWidthDp
import chat.simplex.res.MR
import com.godaddy.android.colorpicker.ClassicColorPicker
import com.godaddy.android.colorpicker.HsvColor
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.io.File
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.*

@Composable
expect fun AppearanceView(m: ChatModel)

object AppearanceScope {
  @Composable
  fun ProfileImageSection() {
    SectionView(stringResource(MR.strings.settings_section_title_profile_images).uppercase(), contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
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
            saveThemeToDatabase(null)
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
  fun MessageShapeSection() {
    SectionView(stringResource(MR.strings.settings_section_title_message_shape).uppercase(), contentPadding = PaddingValues()) {
      Row(modifier = Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING + 4.dp ) ,verticalAlignment = Alignment.CenterVertically) {
        Text(stringResource(MR.strings.settings_message_shape_corner), color = colors.onBackground)
        Spacer(Modifier.width(10.dp))
        Slider(
          remember { appPreferences.chatItemRoundness.state }.value,
          valueRange = 0f..1f,
          steps = 20,
          onValueChange = {
            val diff = it % 0.05f
            appPreferences.chatItemRoundness.set(it + (if (diff >= 0.025f) -diff + 0.05f else -diff))
            saveThemeToDatabase(null)
          },
          colors = SliderDefaults.colors(
            activeTickColor = Color.Transparent,
            inactiveTickColor = Color.Transparent,
          )
        )
      }
      SettingsPreferenceItem(icon = null, stringResource(MR.strings.settings_message_shape_tail), appPreferences.chatItemTail)
    }
  }

  @Composable
  fun FontScaleSection() {
    val localFontScale = remember { mutableStateOf(appPrefs.fontScale.get()) }
    SectionView(stringResource(MR.strings.appearance_font_size).uppercase(), contentPadding = PaddingValues(horizontal = DEFAULT_PADDING)) {
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
        if (appPlatform.isAndroid) {
          Slider(
            localFontScale.value,
            valueRange = 0.75f..1.25f,
            steps = 11,
            onValueChange = {
              val diff = it % 0.05f
              localFontScale.value = String.format(Locale.US, "%.2f", it + (if (diff >= 0.025f) -diff + 0.05f else -diff)).toFloatOrNull() ?: 1f
            },
            onValueChangeFinished = {
              appPrefs.fontScale.set(localFontScale.value)
            },
            colors = SliderDefaults.colors(
              activeTickColor = Color.Transparent,
              inactiveTickColor = Color.Transparent,
            )
          )
        } else {
          Slider(
            localFontScale.value,
            valueRange = 0.7f..1.5f,
            steps = 9,
            onValueChange = {
              val diff = it % 0.1f
              localFontScale.value = String.format(Locale.US, "%.1f", it + (if (diff >= 0.05f) -diff + 0.1f else -diff)).toFloatOrNull() ?: 1f
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
  }

  @Composable
  fun ChatThemePreview(
    theme: DefaultTheme,
    wallpaperImage: ImageBitmap?,
    wallpaperType: WallpaperType?,
    backgroundColor: Color? = MaterialTheme.wallpaper.background,
    tintColor: Color? = MaterialTheme.wallpaper.tint,
    withMessages: Boolean = true
  ) {
    val themeBackgroundColor = MaterialTheme.colors.background
    val backgroundColor =  backgroundColor ?: wallpaperType?.defaultBackgroundColor(theme, MaterialTheme.colors.background)
    val tintColor = tintColor ?: wallpaperType?.defaultTintColor(theme)
    Column(Modifier
      .drawWithCache {
        if (wallpaperImage != null && wallpaperType != null && backgroundColor != null && tintColor != null) {
          chatViewBackground(wallpaperImage, wallpaperType, backgroundColor, tintColor)
        } else {
          onDrawBehind {
            drawRect(themeBackgroundColor)
          }
        }
      }
      .padding(DEFAULT_PADDING_HALF)
    ) {
      if (withMessages) {
        val chatItemTail = remember { appPreferences.chatItemTail.state }

        Column(verticalArrangement = Arrangement.spacedBy(4.dp), modifier = if (chatItemTail.value) Modifier else Modifier.padding(horizontal = msgTailWidthDp)) {
          val alice = remember { ChatItem.getSampleData(1, CIDirection.DirectRcv(), Clock.System.now(), generalGetString(MR.strings.wallpaper_preview_hello_bob)) }
          PreviewChatItemView(alice)
          PreviewChatItemView(
            ChatItem.getSampleData(2, CIDirection.DirectSnd(), Clock.System.now(), stringResource(MR.strings.wallpaper_preview_hello_alice),
              quotedItem = CIQuote(alice.chatDir, alice.id, sentAt = alice.meta.itemTs, formattedText = alice.formattedText, content = MsgContent.MCText(alice.content.text))
            )
          )
        }
      } else {
        Box(Modifier.fillMaxSize())
      }
    }
  }

  @Composable
  fun WallpaperPresetSelector(
    selectedWallpaper: WallpaperType?,
    baseTheme: DefaultTheme,
    activeBackgroundColor: Color? = null,
    activeTintColor: Color? = null,
    currentColors: (WallpaperType?) -> ThemeManager.ActiveTheme,
    onChooseType: (WallpaperType?) -> Unit,
  ) {
    val cornerRadius = 22

    @Composable
    fun Plus(tint: Color = MaterialTheme.colors.primary) {
      Icon(painterResource(MR.images.ic_add), null, Modifier.size(25.dp), tint = tint)
    }

    val backgrounds = PresetWallpaper.entries.toList()

    fun LazyGridScope.gridContent(width: Dp, height: Dp) {
      @Composable
      fun BackgroundItem(background: PresetWallpaper?) {
        val checked = (background == null && (selectedWallpaper == null || selectedWallpaper == WallpaperType.Empty)) || selectedWallpaper?.samePreset(background) == true
        Box(
          Modifier
            .size(width, height)
            .clip(RoundedCornerShape(percent = cornerRadius))
            .border(1.dp, if (checked) MaterialTheme.colors.primary.copy(0.8f) else MaterialTheme.colors.onBackground.copy(if (isInDarkTheme()) 0.2f else 0.1f), RoundedCornerShape(percent = cornerRadius))
            .clickable { onChooseType(background?.toType(baseTheme)) },
          contentAlignment = Alignment.Center
        ) {
          if (background != null) {
            val type = background.toType(baseTheme, if (checked) selectedWallpaper?.scale else null)
            SimpleXThemeOverride(remember(background, selectedWallpaper, CurrentColors.collectAsState().value) { currentColors(type) }) {
              ChatThemePreview(
                baseTheme,
                type.image,
                type,
                withMessages = false,
                backgroundColor = if (checked) activeBackgroundColor ?: MaterialTheme.wallpaper.background else MaterialTheme.wallpaper.background,
                tintColor = if (checked) activeTintColor ?: MaterialTheme.wallpaper.tint else MaterialTheme.wallpaper.tint
              )
            }
          }
        }
      }

      @Composable
      fun OwnBackgroundItem(type: WallpaperType?) {
        val overrides = remember(type, baseTheme, CurrentColors.collectAsState().value.wallpaper) {
          currentColors(WallpaperType.Image("", null, null))
        }
        val appWallpaper = overrides.wallpaper
        val backgroundColor = appWallpaper.background
        val tintColor = appWallpaper.tint
        val wallpaperImage = appWallpaper.type.image
        val checked = type is WallpaperType.Image && wallpaperImage != null
        val remoteHostConnected = chatModel.remoteHostId != null
        Box(
          Modifier
            .size(width, height)
            .clip(RoundedCornerShape(percent = cornerRadius))
            .border(1.dp, if (type is WallpaperType.Image) MaterialTheme.colors.primary.copy(0.8f) else MaterialTheme.colors.onBackground.copy(0.1f), RoundedCornerShape(percent = cornerRadius))
            .clickable { onChooseType(WallpaperType.Image("", null, null)) },
          contentAlignment = Alignment.Center
        ) {

          if (checked || wallpaperImage != null) {
            ChatThemePreview(
              baseTheme,
              wallpaperImage,
              if (checked) type else appWallpaper.type,
              backgroundColor = if (checked) activeBackgroundColor ?: backgroundColor else backgroundColor,
              tintColor = if (checked) activeTintColor ?: tintColor else tintColor,
              withMessages = false
            )
          } else if (remoteHostConnected) {
            Plus(MaterialTheme.colors.error)
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
        OwnBackgroundItem(selectedWallpaper)
      }
    }

    SimpleXThemeOverride(remember(selectedWallpaper, CurrentColors.collectAsState().value) { currentColors(selectedWallpaper) }) {
      ChatThemePreview(
        baseTheme,
        MaterialTheme.wallpaper.type.image,
        selectedWallpaper,
        backgroundColor = activeBackgroundColor ?: MaterialTheme.wallpaper.background,
        tintColor = activeTintColor ?: MaterialTheme.wallpaper.tint,
      )
    }

    if (appPlatform.isDesktop) {
      val itemWidth = (DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier - DEFAULT_PADDING * 2 - DEFAULT_PADDING_HALF * 3) / 4
      val itemHeight = (DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier - DEFAULT_PADDING * 2) / 4
      val rows = ceil((PresetWallpaper.entries.size + 2) / 4f).roundToInt()
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

  @Composable
  fun ThemesSection(systemDarkTheme: SharedPreference<String?>) {
    val currentTheme by CurrentColors.collectAsState()
    val baseTheme = currentTheme.base
    val wallpaperType = MaterialTheme.wallpaper.type
    val themeUserDestination: MutableState<Pair<Long, ThemeModeOverrides?>?> = rememberSaveable(stateSaver = serializableSaver()) {
      val currentUser = chatModel.currentUser.value
      mutableStateOf(
        if (currentUser?.uiThemes?.preferredMode(!currentTheme.colors.isLight) == null) null else currentUser.userId to currentUser.uiThemes
      )
    }
    val perUserTheme = remember(CurrentColors.collectAsState().value.base, chatModel.currentUser.value) {
      mutableStateOf(
        chatModel.currentUser.value?.uiThemes?.preferredMode(!CurrentColors.value.colors.isLight) ?: ThemeModeOverride()
      )
    }

    fun updateThemeUserDestination() {
      var (userId, themes) = themeUserDestination.value ?: return
      themes = if (perUserTheme.value.mode == DefaultThemeMode.LIGHT) {
        (themes ?: ThemeModeOverrides()).copy(light = perUserTheme.value)
      } else {
        (themes ?: ThemeModeOverrides()).copy(dark = perUserTheme.value)
      }
      themeUserDestination.value = userId to themes
    }

    val onTypeCopyFromSameTheme = { type: WallpaperType? ->
      if (themeUserDestination.value == null) {
        ThemeManager.saveAndApplyWallpaper(baseTheme, type)
      } else {
        val wallpaperFiles = setOf(perUserTheme.value.wallpaper?.imageFile)
        ThemeManager.copyFromSameThemeOverrides(type, null, perUserTheme)
        val wallpaperFilesToDelete = wallpaperFiles - perUserTheme.value.wallpaper?.imageFile
        wallpaperFilesToDelete.forEach(::removeWallpaperFile)
        updateThemeUserDestination()
      }
      saveThemeToDatabase(themeUserDestination.value)
      true
    }

    val onTypeChange = { type: WallpaperType? ->
      if (themeUserDestination.value == null) {
        ThemeManager.saveAndApplyWallpaper(baseTheme, type)
      } else {
        ThemeManager.applyWallpaper(type, perUserTheme)
        updateThemeUserDestination()
      }
      saveThemeToDatabase(themeUserDestination.value)
    }

    val onImport = { to: URI ->
      val filename = saveWallpaperFile(to)
      if (filename != null) {
        if (themeUserDestination.value == null) {
          removeWallpaperFile((currentTheme.wallpaper.type as? WallpaperType.Image)?.filename)
        } else {
          removeWallpaperFile((perUserTheme.value.type as? WallpaperType.Image)?.filename)
        }
        onTypeChange(WallpaperType.Image(filename, 1f, WallpaperScaleType.FILL))
      }
    }

    val currentColors = { type: WallpaperType? ->
      // If applying for :
      // - all themes: no overrides needed
      // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
      val perUserOverride = if (themeUserDestination.value == null) null else if (wallpaperType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
      ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.get())
    }

    val onChooseType: (WallpaperType?, FileChooserLauncher) -> Unit = { type: WallpaperType?, importWallpaperLauncher: FileChooserLauncher ->
      when {
        // don't have image in parent or already selected wallpaper with custom image
        type is WallpaperType.Image &&
            ((wallpaperType is WallpaperType.Image && themeUserDestination.value?.second != null && chatModel.remoteHostId() == null) ||
                currentColors(type).wallpaper.type.image == null ||
                (currentColors(type).wallpaper.type.image != null && CurrentColors.value.wallpaper.type is WallpaperType.Image && themeUserDestination.value == null)) ->
          withLongRunningApi { importWallpaperLauncher.launch("image/*") }
        type is WallpaperType.Image && themeUserDestination.value == null -> onTypeChange(currentColors(type).wallpaper.type)
        type is WallpaperType.Image && chatModel.remoteHostId() != null -> { /* do nothing when remote host connected */ }
        type is WallpaperType.Image -> onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)
        (themeUserDestination.value != null && themeUserDestination.value?.second?.preferredMode(!CurrentColors.value.colors.isLight)?.type != type) || CurrentColors.value.wallpaper.type != type -> onTypeCopyFromSameTheme(type)
        else -> onTypeChange(type)
      }
    }

    SectionView(stringResource(MR.strings.settings_section_title_themes)) {
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      ThemeDestinationPicker(themeUserDestination)
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))

      val importWallpaperLauncher = rememberFileChooserLauncher(true) { to: URI? ->
        if (to != null) onImport(to)
      }

      WallpaperPresetSelector(
        selectedWallpaper = wallpaperType,
        baseTheme = currentTheme.base,
        currentColors = { type ->
          currentColors(type)
        },
        onChooseType = { onChooseType(it, importWallpaperLauncher) },
      )
      val type = MaterialTheme.wallpaper.type
      if (type is WallpaperType.Image && (themeUserDestination.value == null || perUserTheme.value.wallpaper?.imageFile != null)) {
        SectionItemView(disabled = chatModel.remoteHostId != null && themeUserDestination.value != null, click = {
          if (themeUserDestination.value == null) {
            val defaultActiveTheme = ThemeManager.defaultActiveTheme(appPrefs.themeOverrides.get())
            ThemeManager.saveAndApplyWallpaper(baseTheme, null)
            ThemeManager.removeTheme(defaultActiveTheme?.themeId)
            removeWallpaperFile(type.filename)
          } else {
            removeUserThemeModeOverrides(themeUserDestination, perUserTheme)
          }
          saveThemeToDatabase(themeUserDestination.value)
        }) {
          Text(
            stringResource(MR.strings.theme_remove_image),
            color = if (chatModel.remoteHostId != null && themeUserDestination.value != null) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          )
        }
        SectionSpacer()
      }

      val state: State<DefaultThemeMode?> = remember(appPrefs.currentTheme.get()) {
        derivedStateOf {
          if (appPrefs.currentTheme.get() == DefaultTheme.SYSTEM_THEME_NAME) null else currentTheme.base.mode
        }
      }
      ColorModeSelector(state) {
        val newTheme = when (it) {
          null -> DefaultTheme.SYSTEM_THEME_NAME
          DefaultThemeMode.LIGHT -> DefaultTheme.LIGHT.themeName
          DefaultThemeMode.DARK -> appPrefs.systemDarkTheme.get()!!
        }
        ThemeManager.applyTheme(newTheme)
        saveThemeToDatabase(null)
      }

      // Doesn't work on desktop when specified like remember { systemDarkTheme.state }, this is workaround
      val darkModeState: State<String?> = remember(systemDarkTheme.get()) { derivedStateOf { systemDarkTheme.get() } }
      DarkModeThemeSelector(darkModeState) {
        ThemeManager.changeDarkTheme(it)
        if (appPrefs.currentTheme.get() == DefaultTheme.SYSTEM_THEME_NAME) {
          ThemeManager.applyTheme(appPrefs.currentTheme.get()!!)
        } else if (appPrefs.currentTheme.get() != DefaultTheme.LIGHT.themeName) {
          ThemeManager.applyTheme(appPrefs.systemDarkTheme.get()!!)
        }
        saveThemeToDatabase(null)
      }
    }
    SectionItemView(click = {
      val user = themeUserDestination.value
      if (user == null) {
        ModalManager.start.showModal {
          val importWallpaperLauncher = rememberFileChooserLauncher(true) { to: URI? ->
            if (to != null) onImport(to)
          }
          CustomizeThemeView { onChooseType(it, importWallpaperLauncher) }
        }
      } else {
        ModalManager.start.showModalCloseable { close ->
          UserWallpaperEditorModal(chatModel.remoteHostId(), user.first, close)
        }
      }
    }) {
      Text(stringResource(MR.strings.customize_theme_title))
    }
  }

  @Composable
  fun CustomizeThemeView(onChooseType: (WallpaperType?) -> Unit) {
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
    ) {
      val currentTheme by CurrentColors.collectAsState()

      AppBarTitle(stringResource(MR.strings.customize_theme_title))
      val wallpaperImage = MaterialTheme.wallpaper.type.image
      val wallpaperType = MaterialTheme.wallpaper.type
      val baseTheme = CurrentColors.collectAsState().value.base

      val editColor = { name: ThemeColor ->
        editColor(
          name,
          wallpaperType,
          wallpaperImage,
          onColorChange = { color ->
            ThemeManager.saveAndApplyThemeColor(baseTheme, name, color)
            saveThemeToDatabase(null)
          }
        )
      }

      WallpaperPresetSelector(
        selectedWallpaper = wallpaperType,
        baseTheme = currentTheme.base,
        currentColors = { type ->
          ThemeManager.currentColors(type, null, null, appPrefs.themeOverrides.get())
        },
        onChooseType = onChooseType
      )

      val type = MaterialTheme.wallpaper.type
      if (type is WallpaperType.Image) {
        SectionItemView(disabled = chatModel.remoteHostId != null, click = {
          val defaultActiveTheme = ThemeManager.defaultActiveTheme(appPrefs.themeOverrides.get())
          ThemeManager.saveAndApplyWallpaper(baseTheme, null)
          ThemeManager.removeTheme(defaultActiveTheme?.themeId)
          removeWallpaperFile(type.filename)
          saveThemeToDatabase(null)
        }) {
          Text(
            stringResource(MR.strings.theme_remove_image),
            color = if (chatModel.remoteHostId == null) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
          )
        }
        SectionSpacer()
      }

      SectionView(stringResource(MR.strings.settings_section_title_chat_colors).uppercase()) {
        WallpaperSetupView(
          wallpaperType,
          baseTheme,
          MaterialTheme.wallpaper,
          MaterialTheme.appColors.sentMessage,
          MaterialTheme.appColors.sentQuote,
          MaterialTheme.appColors.receivedMessage,
          MaterialTheme.appColors.receivedQuote,
          editColor = { name ->
            editColor(name)
          },
          onTypeChange = { type ->
            ThemeManager.saveAndApplyWallpaper(baseTheme, type)
            saveThemeToDatabase(null)
          },
        )
      }
      SectionDividerSpaced()

      CustomizeThemeColorsSection(currentTheme) { name ->
        editColor(name)
      }

      SectionDividerSpaced(maxBottomPadding = false)

      val currentOverrides = remember(currentTheme) { ThemeManager.defaultActiveTheme(appPrefs.themeOverrides.get()) }
      val canResetColors = currentTheme.base.hasChangedAnyColor(currentOverrides)
      if (canResetColors) {
        SectionItemView({
          ThemeManager.resetAllThemeColors()
          saveThemeToDatabase(null)
        }) {
          Text(generalGetString(MR.strings.reset_color), color = colors.primary)
        }
        SectionSpacer()
      }

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
          val overrides = ThemeManager.currentThemeOverridesForExport(null, null/*chatModel.currentUser.value?.uiThemes*/)
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
              ThemeManager.saveAndApplyThemeOverrides(theme)
              saveThemeToDatabase(null)
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
  fun ColorModeSwitcher() {
    val currentTheme by CurrentColors.collectAsState()
    val themeMode = if (remember { appPrefs.currentTheme.state }.value == DefaultTheme.SYSTEM_THEME_NAME) {
      if (systemInDarkThemeCurrently) DefaultThemeMode.DARK else DefaultThemeMode.LIGHT
    } else {
      currentTheme.base.mode
    }

    val onLongClick = {
      ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
      showToast(generalGetString(MR.strings.system_mode_toast))

      saveThemeToDatabase(null)
    }
    Box(
      modifier = Modifier
        .clip(CircleShape)
        .combinedClickable(
          onClick = {
            ThemeManager.applyTheme(if (themeMode == DefaultThemeMode.LIGHT) appPrefs.systemDarkTheme.get()!! else DefaultTheme.LIGHT.themeName)
            saveThemeToDatabase(null)
          },
          onLongClick = onLongClick
        )
        .onRightClick(onLongClick)
        .size(44.dp),
      contentAlignment = Alignment.Center
    ) {
      Icon(painterResource(if (themeMode == DefaultThemeMode.LIGHT) MR.images.ic_light_mode else MR.images.ic_bedtime_moon), stringResource(MR.strings.color_mode_light), tint = MaterialTheme.colors.secondary)
    }
  }

  private var updateBackendJob: Job = Job()
  private fun saveThemeToDatabase(themeUserDestination: Pair<Long, ThemeModeOverrides?>?) {
    val remoteHostId = chatModel.remoteHostId()
    val oldThemes = chatModel.currentUser.value?.uiThemes
    if (themeUserDestination != null) {
      // Update before save to make it work seamless
      chatModel.updateCurrentUserUiThemes(remoteHostId, themeUserDestination.second)
    }
    updateBackendJob.cancel()
    updateBackendJob = withBGApi {
      delay(300)
      if (themeUserDestination == null) {
        controller.apiSaveAppSettings(AppSettings.current.prepareForExport())
      } else if (!controller.apiSetUserUIThemes(remoteHostId, themeUserDestination.first, themeUserDestination.second)) {
        // If failed to apply for some reason return the old themes
        chatModel.updateCurrentUserUiThemes(remoteHostId, oldThemes)
      }
    }
  }

  fun editColor(name: ThemeColor, wallpaperType: WallpaperType, wallpaperImage: ImageBitmap?, onColorChange: (Color?) -> Unit) {
    ModalManager.start.showModal {
      val baseTheme = CurrentColors.collectAsState().value.base
      val wallpaperBackgroundColor = MaterialTheme.wallpaper.background ?: wallpaperType.defaultBackgroundColor(baseTheme, MaterialTheme.colors.background)
      val wallpaperTintColor = MaterialTheme.wallpaper.tint ?: wallpaperType.defaultTintColor(baseTheme)
      val initialColor: Color = when (name) {
        ThemeColor.WALLPAPER_BACKGROUND -> wallpaperBackgroundColor
        ThemeColor.WALLPAPER_TINT -> wallpaperTintColor
        ThemeColor.PRIMARY -> MaterialTheme.colors.primary
        ThemeColor.PRIMARY_VARIANT -> MaterialTheme.colors.primaryVariant
        ThemeColor.SECONDARY -> MaterialTheme.colors.secondary
        ThemeColor.SECONDARY_VARIANT -> MaterialTheme.colors.secondaryVariant
        ThemeColor.BACKGROUND -> MaterialTheme.colors.background
        ThemeColor.SURFACE -> MaterialTheme.colors.surface
        ThemeColor.TITLE -> MaterialTheme.appColors.title
        ThemeColor.PRIMARY_VARIANT2 -> MaterialTheme.appColors.primaryVariant2
        ThemeColor.SENT_MESSAGE -> MaterialTheme.appColors.sentMessage
        ThemeColor.SENT_QUOTE -> MaterialTheme.appColors.sentQuote
        ThemeColor.RECEIVED_MESSAGE -> MaterialTheme.appColors.receivedMessage
        ThemeColor.RECEIVED_QUOTE -> MaterialTheme.appColors.receivedQuote
      }
      ColorEditor(name, initialColor, baseTheme, MaterialTheme.wallpaper.type, wallpaperImage, currentColors = { CurrentColors.value },
        onColorChange = onColorChange
      )
    }
  }

  @Composable
  fun ModalData.UserWallpaperEditorModal(remoteHostId: Long?, userId: Long, close: () -> Unit) {
    val themes = remember(chatModel.currentUser.value) { mutableStateOf(chatModel.currentUser.value?.uiThemes ?: ThemeModeOverrides()) }
    val globalThemeUsed = remember { stateGetOrPut("globalThemeUsed") { false }  }
    val initialTheme = remember(CurrentColors.collectAsState().value.base) {
      val preferred = themes.value.preferredMode(!CurrentColors.value.colors.isLight)
      globalThemeUsed.value = preferred == null
      preferred ?: ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
    }
    UserWallpaperEditor(
      initialTheme,
      applyToMode = if (themes.value.light == themes.value.dark) null else initialTheme.mode,
      globalThemeUsed = globalThemeUsed,
      save = { applyToMode, newTheme ->
        save(applyToMode, newTheme, themes.value, userId, remoteHostId)
      })
    KeyChangeEffect(chatModel.currentUser.value?.userId, chatModel.remoteHostId) {
      close()
    }
  }

  suspend fun save(
    applyToMode: DefaultThemeMode?,
    newTheme: ThemeModeOverride?,
    themes: ThemeModeOverrides?,
    userId: Long,
    remoteHostId: Long?
  ) {
    val unchangedThemes: ThemeModeOverrides = themes ?: ThemeModeOverrides()
    val wallpaperFiles = setOf(unchangedThemes.light?.wallpaper?.imageFile, unchangedThemes.dark?.wallpaper?.imageFile)
    var changedThemes: ThemeModeOverrides? = unchangedThemes
    val changed = newTheme?.copy(wallpaper = newTheme.wallpaper?.withFilledWallpaperPath())
    changedThemes = when (applyToMode) {
      null -> changedThemes?.copy(light = changed?.copy(mode = DefaultThemeMode.LIGHT), dark = changed?.copy(mode = DefaultThemeMode.DARK))
      DefaultThemeMode.LIGHT -> changedThemes?.copy(light = changed?.copy(mode = applyToMode))
      DefaultThemeMode.DARK -> changedThemes?.copy(dark = changed?.copy(mode = applyToMode))
    }
    changedThemes = if (changedThemes?.light != null || changedThemes?.dark != null) {
      val light = changedThemes.light
      val dark = changedThemes.dark
      val currentMode = CurrentColors.value.base.mode
      // same image file for both modes, copy image to make them as different files
      if (light?.wallpaper?.imageFile != null && dark?.wallpaper?.imageFile != null && light.wallpaper.imageFile == dark.wallpaper.imageFile) {
        val imageFile = if (currentMode == DefaultThemeMode.LIGHT) {
          dark.wallpaper.imageFile
        } else {
          light.wallpaper.imageFile
        }
        val filePath = saveWallpaperFile(File(getWallpaperFilePath(imageFile)).toURI())
        changedThemes = if (currentMode == DefaultThemeMode.LIGHT) {
          changedThemes.copy(dark = dark.copy(wallpaper = dark.wallpaper.copy(imageFile = filePath)))
        } else {
          changedThemes.copy(light = light.copy(wallpaper = light.wallpaper.copy(imageFile = filePath)))
        }
      }
      changedThemes
    } else {
      null
    }

    val wallpaperFilesToDelete = wallpaperFiles - changedThemes?.light?.wallpaper?.imageFile - changedThemes?.dark?.wallpaper?.imageFile
    wallpaperFilesToDelete.forEach(::removeWallpaperFile)

    val oldThemes = chatModel.currentUser.value?.uiThemes
    // Update before save to make it work seamless
    chatModel.updateCurrentUserUiThemes(remoteHostId, changedThemes)
    updateBackendJob.cancel()
    updateBackendJob = withBGApi {
      delay(300)
      if (!controller.apiSetUserUIThemes(remoteHostId, userId, changedThemes)) {
        // If failed to apply for some reason return the old themes
        chatModel.updateCurrentUserUiThemes(remoteHostId, oldThemes)
      }
    }
  }

  @Composable
  fun ThemeDestinationPicker(themeUserDestination: MutableState<Pair<Long, ThemeModeOverrides?>?>) {
    val themeUserDest = remember(themeUserDestination.value?.first) { mutableStateOf(themeUserDestination.value?.first) }
    LaunchedEffect(themeUserDestination.value) {
      if (themeUserDestination.value == null) {
        // Easiest way to hide per-user customization.
        // Otherwise, it would be needed to make global variable and to use it everywhere for making a decision to include these overrides into active theme constructing or not
        chatModel.currentUser.value = chatModel.currentUser.value?.copy(uiThemes = null)
      } else {
        chatModel.updateCurrentUserUiThemes(chatModel.remoteHostId(), chatModel.users.firstOrNull { it.user.userId == chatModel.currentUser.value?.userId }?.user?.uiThemes)
      }
    }
    DisposableEffect(Unit) {
      onDispose {
        // Skip when Appearance screen is not hidden yet
        if (ModalManager.start.hasModalsOpen()) return@onDispose
        // Restore user overrides from stored list of users
        chatModel.updateCurrentUserUiThemes(chatModel.remoteHostId(), chatModel.users.firstOrNull { it.user.userId == chatModel.currentUser.value?.userId }?.user?.uiThemes)
        themeUserDestination.value = if (chatModel.currentUser.value?.uiThemes == null) null else chatModel.currentUser.value?.userId!! to chatModel.currentUser.value?.uiThemes
      }
    }

    val values by remember(chatModel.users.toList()) { mutableStateOf(
      listOf(null as Long? to generalGetString(MR.strings.theme_destination_app_theme))
          +
        chatModel.users.filter { it.user.activeUser }.map {
          it.user.userId to it.user.chatViewName
        },
      )
    }
    if (values.any { it.first == themeUserDestination.value?.first }) {
      ExposedDropDownSettingRow(
        generalGetString(MR.strings.chat_theme_apply_to_mode),
        values,
        themeUserDest,
        icon = null,
        enabled = remember { mutableStateOf(true) },
        onSelected = { userId ->
          themeUserDest.value = userId
          if (userId != null) {
            themeUserDestination.value = userId to chatModel.users.firstOrNull { it.user.userId == userId }?.user?.uiThemes
          } else {
            themeUserDestination.value = null
          }
          if (userId != null && userId != chatModel.currentUser.value?.userId) {
            withBGApi {
              controller.showProgressIfNeeded {
                chatModel.controller.changeActiveUser(chatModel.remoteHostId(), userId, null)
              }
            }
          }
        }
      )
    } else {
      themeUserDestination.value = null
    }
  }

  @Composable
  fun CustomizeThemeColorsSection(currentTheme: ThemeManager.ActiveTheme, editColor: (ThemeColor) -> Unit) {
    SectionView(stringResource(MR.strings.theme_colors_section_title)) {
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY) }) {
        val title = generalGetString(MR.strings.color_primary)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.primary)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY_VARIANT) }) {
        val title = generalGetString(MR.strings.color_primary_variant)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.primaryVariant)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY_VARIANT2) }) {
        val title = generalGetString(MR.strings.color_primary_variant2)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.appColors.primaryVariant2)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY) }) {
        val title = generalGetString(MR.strings.color_secondary)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.secondary)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SECONDARY_VARIANT) }) {
        val title = generalGetString(MR.strings.color_secondary_variant)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.secondaryVariant)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.BACKGROUND) }) {
        val title = generalGetString(MR.strings.color_background)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.background)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.SURFACE) }) {
        val title = generalGetString(MR.strings.color_surface)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.colors.surface)
      }
      SectionItemViewSpaceBetween({ editColor(ThemeColor.TITLE) }) {
        val title = generalGetString(MR.strings.color_title)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.appColors.title)
      }
    }
  }

  @Composable
  fun ColorEditor(
    name: ThemeColor,
    initialColor: Color,
    theme: DefaultTheme,
    wallpaperType: WallpaperType?,
    wallpaperImage: ImageBitmap?,
    previewBackgroundColor: Color? = MaterialTheme.wallpaper.background,
    previewTintColor: Color? = MaterialTheme.wallpaper.tint,
    currentColors: () -> ThemeManager.ActiveTheme,
    onColorChange: (Color?) -> Unit,
  ) {
    ColumnWithScrollBar(
      Modifier
        .fillMaxWidth()
    ) {
      AppBarTitle(name.text)

      val supportedLiveChange = name in listOf(ThemeColor.SECONDARY, ThemeColor.BACKGROUND, ThemeColor.SURFACE, ThemeColor.RECEIVED_MESSAGE, ThemeColor.SENT_MESSAGE, ThemeColor.SENT_QUOTE, ThemeColor.WALLPAPER_BACKGROUND, ThemeColor.WALLPAPER_TINT)
      if (supportedLiveChange) {
        SimpleXThemeOverride(currentColors()) {
          ChatThemePreview(theme, wallpaperImage, wallpaperType, previewBackgroundColor, previewTintColor)
        }
        SectionSpacer()
      }

      var currentColor by remember { mutableStateOf(initialColor) }
      val togglePicker = remember { mutableStateOf(false) }
      Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
        if (togglePicker.value) {
          ColorPicker(currentColor, showAlphaBar = wallpaperType is WallpaperType.Image || currentColor.alpha < 1f) {
            currentColor = it
            onColorChange(currentColor)
          }
        } else {
          ColorPicker(currentColor, showAlphaBar = wallpaperType is WallpaperType.Image || currentColor.alpha < 1f) {
            currentColor = it
            onColorChange(currentColor)
          }
        }
      }
      var allowReloadPicker by remember { mutableStateOf(false) }
      KeyChangeEffect(wallpaperType) {
        allowReloadPicker = true
      }
      KeyChangeEffect(initialColor) {
        if (initialColor != currentColor && allowReloadPicker) {
          currentColor = initialColor
          togglePicker.value = !togglePicker.value
        }
        allowReloadPicker = false
      }
      val clipboard = LocalClipboardManager.current
      val hexTrimmed = currentColor.toReadableHex().replaceFirst("#ff", "#")
      val savedColor by remember(wallpaperType) { mutableStateOf(initialColor) }

      Row(Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).height(DEFAULT_MIN_SECTION_ITEM_HEIGHT)) {
        Box(Modifier.weight(1f).fillMaxHeight().background(savedColor).clickable {
          currentColor = savedColor
          onColorChange(currentColor)
          togglePicker.value = !togglePicker.value
        })
        Box(Modifier.weight(1f).fillMaxHeight().background(currentColor).clickable {
          clipboard.shareText(hexTrimmed)
        })
      }
      if (appPrefs.developerTools.get()) {
        Row(Modifier.fillMaxWidth().padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING), horizontalArrangement = Arrangement.SpaceEvenly, verticalAlignment = Alignment.CenterVertically) {
          val textFieldState = remember { mutableStateOf(TextFieldValue(hexTrimmed)) }
          KeyChangeEffect(hexTrimmed) {
            textFieldState.value = textFieldState.value.copy(hexTrimmed)
          }
          DefaultBasicTextField(
            Modifier.fillMaxWidth(),
            textFieldState,
            leadingIcon = {
              IconButton(onClick = { clipboard.shareText(hexTrimmed) }) {
                Icon(painterResource(MR.images.ic_content_copy), generalGetString(MR.strings.copy_verb), Modifier.size(26.dp), tint = MaterialTheme.colors.primary)
              }
            },
            onValueChange = { value ->
              val color = value.text.trim('#', ' ')
              if (color.length == 6 || color.length == 8) {
                currentColor = if (color.length == 6) ("ff$color").colorFromReadableHex() else color.colorFromReadableHex()
                onColorChange(currentColor)
                textFieldState.value = value.copy(currentColor.toReadableHex().replaceFirst("#ff", "#"))
                togglePicker.value = !togglePicker.value
              } else {
                textFieldState.value = value
              }
            }
          )
        }
      }
      SectionItemView({
        allowReloadPicker = true
        onColorChange(null)
      }) {
        Text(generalGetString(MR.strings.reset_single_color), color = colors.primary)
      }
      SectionSpacer()
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
      "fa" to "فارسی",
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
    val values by remember(appPrefs.appLanguage.state.value) { mutableStateOf(supportedLanguages.map { it.key to it.value }) }
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
  private fun ColorModeSelector(state: State<DefaultThemeMode?>, onSelected: (DefaultThemeMode?) -> Unit) {
    val values by remember(appPrefs.appLanguage.state.value) {
      mutableStateOf(
        listOf(
          null to generalGetString(MR.strings.color_mode_system),
          DefaultThemeMode.LIGHT to generalGetString(MR.strings.color_mode_light),
          DefaultThemeMode.DARK to generalGetString(MR.strings.color_mode_dark)
        )
      )
    }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.color_mode),
      values,
      state,
      icon = null,
      enabled = remember { mutableStateOf(true) },
      onSelected = onSelected
    )
  }

  @Composable
  private fun DarkModeThemeSelector(state: State<String?>, onSelected: (String) -> Unit) {
    val values by remember {
      val darkThemes = ArrayList<Pair<String, String>>()
      darkThemes.add(DefaultTheme.DARK.themeName to generalGetString(MR.strings.theme_dark))
      darkThemes.add(DefaultTheme.SIMPLEX.themeName to generalGetString(MR.strings.theme_simplex))
      darkThemes.add(DefaultTheme.BLACK.themeName to generalGetString(MR.strings.theme_black))
      mutableStateOf(darkThemes.toList())
    }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.dark_mode_colors),
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
  wallpaperType: WallpaperType?,
  theme: DefaultTheme,
  initialWallpaper: AppWallpaper?,
  initialSentColor: Color,
  initialSentQuoteColor: Color,
  initialReceivedColor: Color,
  initialReceivedQuoteColor: Color,
  editColor: (ThemeColor) -> Unit,
  onTypeChange: (WallpaperType?) -> Unit,
) {
  if (wallpaperType is WallpaperType.Image) {
    val state = remember(wallpaperType.scaleType, initialWallpaper?.type) { mutableStateOf(wallpaperType.scaleType ?: (initialWallpaper?.type as? WallpaperType.Image)?.scaleType ?: WallpaperScaleType.FILL) }
    val values = remember {
      WallpaperScaleType.entries.map { it to generalGetString(it.text) }
    }
    ExposedDropDownSettingRow(
      stringResource(MR.strings.wallpaper_scale),
      values,
      state,
      onSelected = { scaleType ->
        onTypeChange(wallpaperType.copy(scaleType = scaleType))
      }
    )
  }

  if (wallpaperType is WallpaperType.Preset || (wallpaperType is WallpaperType.Image && wallpaperType.scaleType == WallpaperScaleType.REPEAT)) {
    val state = remember(wallpaperType, initialWallpaper?.type?.scale) { mutableStateOf(wallpaperType.scale ?: initialWallpaper?.type?.scale ?: 1f) }
    Row(Modifier.padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
      Text("${state.value}".substring(0, min("${state.value}".length, 4)), Modifier.width(50.dp))
      Slider(
        state.value,
        valueRange = 0.5f..2f,
        onValueChange = {
          if (wallpaperType is WallpaperType.Preset) {
            onTypeChange(wallpaperType.copy(scale = it))
          } else if (wallpaperType is WallpaperType.Image) {
            onTypeChange(wallpaperType.copy(scale = it))
          }
        }
      )
    }
  }

  if (wallpaperType is WallpaperType.Preset || wallpaperType is WallpaperType.Image) {
    val wallpaperBackgroundColor = initialWallpaper?.background ?: wallpaperType.defaultBackgroundColor(theme, MaterialTheme.colors.background)
    SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_BACKGROUND) }) {
      val title = generalGetString(MR.strings.color_wallpaper_background)
      Text(title)
      Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperBackgroundColor)
    }
    val wallpaperTintColor = initialWallpaper?.tint ?: wallpaperType.defaultTintColor(theme)
    SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_TINT) }) {
      val title = generalGetString(MR.strings.color_wallpaper_tint)
      Text(title)
      Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperTintColor)
    }
    SectionSpacer()
  }

  SectionItemViewSpaceBetween({ editColor(ThemeColor.SENT_MESSAGE) }) {
    val title = generalGetString(MR.strings.color_sent_message)
    Text(title)
    Icon(painterResource(MR.images.ic_circle_filled), title, tint = initialSentColor)
  }
  SectionItemViewSpaceBetween({ editColor(ThemeColor.SENT_QUOTE) }) {
    val title = generalGetString(MR.strings.color_sent_quote)
    Text(title)
    Icon(painterResource(MR.images.ic_circle_filled), title, tint = initialSentQuoteColor)
  }
  SectionItemViewSpaceBetween({ editColor(ThemeColor.RECEIVED_MESSAGE) }) {
    val title = generalGetString(MR.strings.color_received_message)
    Text(title)
    Icon(painterResource(MR.images.ic_circle_filled), title, tint = initialReceivedColor)
  }
  SectionItemViewSpaceBetween({ editColor(ThemeColor.RECEIVED_QUOTE) }) {
    val title = generalGetString(MR.strings.color_received_quote)
    Text(title)
    Icon(painterResource(MR.images.ic_circle_filled), title, tint = initialReceivedQuoteColor)
  }
}

@Composable
private fun ColorPicker(initialColor: Color, showAlphaBar: Boolean, onColorChanged: (Color) -> Unit) {
  ClassicColorPicker(modifier = Modifier
    .fillMaxWidth()
    .height(300.dp),
    color = HsvColor.from(color = initialColor),
    showAlphaBar = showAlphaBar,
    onColorChanged = { color: HsvColor ->
      onColorChanged(color.toColor())
    }
  )
}

private fun removeUserThemeModeOverrides(themeUserDestination: MutableState<Pair<Long, ThemeModeOverrides?>?>, perUserTheme: MutableState<ThemeModeOverride>) {
  val dest = themeUserDestination.value ?: return
  perUserTheme.value = ThemeModeOverride()
  themeUserDestination.value = dest.first to null
  val wallpaperFilesToDelete = listOf(
    (chatModel.currentUser.value?.uiThemes?.light?.type as? WallpaperType.Image)?.filename,
    (chatModel.currentUser.value?.uiThemes?.dark?.type as? WallpaperType.Image)?.filename
  )
  wallpaperFilesToDelete.forEach(::removeWallpaperFile)
}
