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
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalClipboardManager
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
import chat.simplex.res.MR
import com.godaddy.android.colorpicker.ClassicColorPicker
import com.godaddy.android.colorpicker.HsvColor
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.datetime.Clock
import kotlinx.serialization.encodeToString
import java.net.URI
import java.util.*
import kotlin.collections.ArrayList
import kotlin.math.*

@Composable
expect fun AppearanceView(m: ChatModel)

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
  fun ChatThemePreview(
    theme: DefaultTheme,
    backgroundImage: ImageBitmap?,
    backgroundImageType: BackgroundImageType?,
    backgroundColor: Color? = MaterialTheme.wallpaper.background,
    tintColor: Color? = MaterialTheme.wallpaper.tint,
    withMessages: Boolean = true
  ) {
    val themeBackgroundColor = MaterialTheme.colors.background
    val backgroundColor =  backgroundColor ?: backgroundImageType?.defaultBackgroundColor(theme, MaterialTheme.colors.background)
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
        val alice = remember { ChatItem.getSampleData(1, CIDirection.DirectRcv(), Clock.System.now(), generalGetString(MR.strings.background_image_preview_hello_bob)) }
        PreviewChatItemView(alice)
        PreviewChatItemView(
          ChatItem.getSampleData(2, CIDirection.DirectSnd(), Clock.System.now(), stringResource(MR.strings.background_image_preview_hello_alice),
          quotedItem = CIQuote(alice.chatDir, alice.id, sentAt = alice.meta.itemTs, formattedText = alice.formattedText, content = MsgContent.MCText(alice.content.text))
        )
        )
      } else {
        Box(Modifier.fillMaxSize())
      }
    }
  }

  @Composable
  fun WallpaperPresetSelector(
    selectedBackground: BackgroundImageType?,
    baseTheme: DefaultTheme,
    activeBackgroundColor: Color? = null,
    activeTintColor: Color? = null,
    currentColors: (BackgroundImageType?) -> ThemeManager.ActiveTheme,
    onChooseType: (BackgroundImageType?) -> Unit,
  ) {
    val cornerRadius = 22

    @Composable
    fun Plus() {
      Icon(painterResource(MR.images.ic_add), null, Modifier.size(25.dp), tint = MaterialTheme.colors.primary)
    }

    val backgrounds = PredefinedBackgroundImage.entries.toList()

    fun LazyGridScope.gridContent(width: Dp, height: Dp) {
      @Composable
      fun BackgroundItem(background: PredefinedBackgroundImage?) {
        val checked = (background == null && (selectedBackground == null || selectedBackground == BackgroundImageType.Empty)) || selectedBackground?.samePreset(background) == true
        Box(
          Modifier
            .size(width, height)
            .clip(RoundedCornerShape(percent = cornerRadius))
            .border(1.dp, if (checked) MaterialTheme.colors.primary.copy(0.8f) else MaterialTheme.colors.onBackground.copy(0.1f), RoundedCornerShape(percent = cornerRadius))
            .clickable { onChooseType(background?.toType()) },
          contentAlignment = Alignment.Center
        ) {
          if (background != null) {
            val type = background.toType(if (checked) selectedBackground?.scale else null)
            SimpleXThemeOverride(remember(background, selectedBackground, CurrentColors.collectAsState().value) { currentColors(type) }) {
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
      fun OwnBackgroundItem(type: BackgroundImageType?) {
        val overrides = remember(type, baseTheme) {
          currentColors(BackgroundImageType.Static("", null, null))
        }
        val appWallpaper = overrides.wallpaper
        val backgroundColor = appWallpaper.background
        val tintColor = appWallpaper.tint
        val backgroundImage = appWallpaper.type.image
        val checked = type is BackgroundImageType.Static && backgroundImage != null
        Box(
          Modifier
            .size(width, height)
            .clip(RoundedCornerShape(percent = cornerRadius))
            .border(1.dp, if (checked) MaterialTheme.colors.primary.copy(0.8f) else MaterialTheme.colors.onBackground.copy(0.1f), RoundedCornerShape(percent = cornerRadius))
            .clickable { onChooseType(BackgroundImageType.Static("", null, null)) },
          contentAlignment = Alignment.Center
        ) {

          if (checked || backgroundImage != null) {
            ChatThemePreview(
              baseTheme,
              backgroundImage,
              if (checked) type else appWallpaper.type,
              backgroundColor = if (checked) activeBackgroundColor ?: backgroundColor else backgroundColor,
              tintColor = if (checked) activeTintColor ?: tintColor else tintColor,
              withMessages = false
            )
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

    SimpleXThemeOverride(remember(selectedBackground, CurrentColors.collectAsState().value) { currentColors(selectedBackground) }) {
      ChatThemePreview(
        baseTheme,
        MaterialTheme.wallpaper.type.image,
        selectedBackground,
        backgroundColor = activeBackgroundColor ?: MaterialTheme.wallpaper.background,
        tintColor = activeTintColor ?: MaterialTheme.wallpaper.tint,
      )
    }

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

  private var updateBackendJob: Job = Job()
  private fun saveThemeToDatabase(themeUserDestination: Pair<Long, ThemeModeOverrides?>?) {
    val oldThemes = chatModel.currentUser.value?.uiThemes
    if (themeUserDestination != null) {
      // Update before save to make it work seamless
      chatModel.updateCurrentUserUiThemes(chatModel.remoteHostId(), themeUserDestination.second)
    }
    updateBackendJob.cancel()
    updateBackendJob = withBGApi {
      delay(300)
      if (themeUserDestination == null) {
        controller.apiSaveAppSettings(AppSettings.current.prepareForExport())
      } else if (!controller.apiSetUserUIThemes(chatModel.remoteHostId(), themeUserDestination.first, themeUserDestination.second)) {
        // If failed to apply for some reason return the old themes
        chatModel.updateCurrentUserUiThemes(chatModel.remoteHostId(), oldThemes)
      }
    }
  }

  @Composable
  fun ThemesSection(systemDarkTheme: SharedPreference<String?>) {
    val currentTheme by CurrentColors.collectAsState()
    val baseTheme = currentTheme.base
    val backgroundImageType = MaterialTheme.wallpaper.type
    val themeUserDestination: MutableState<Pair<Long, ThemeModeOverrides?>?> = rememberSaveable(stateSaver = serializableSaver()) {
      val currentUser = chatModel.currentUser.value
      mutableStateOf(
        if (currentUser?.uiThemes == null) null else currentUser.userId to currentUser.uiThemes
      )
    }
    val perUserTheme = remember { mutableStateOf(chatModel.currentUser.value?.uiThemes?.preferredMode(!CurrentColors.value.colors.isLight) ?: ThemeModeOverride()) }

    fun updateThemeUserDestination() {
      var (userId, themes) = themeUserDestination.value ?: return
      themes = if (perUserTheme.value.mode == DefaultThemeMode.LIGHT) {
        (themes ?: ThemeModeOverrides()).copy(light = perUserTheme.value)
      } else {
        (themes ?: ThemeModeOverrides()).copy(dark = perUserTheme.value)
      }
      themeUserDestination.value = userId to themes
    }

    val onTypeCopyFromSameTheme = { type: BackgroundImageType? ->
      if (themeUserDestination.value == null) {
        ThemeManager.saveAndApplyBackgroundImage(baseTheme, type)
      } else {
        val backgroundFiles = listOf(perUserTheme.value.wallpaper?.imageFile)
        ThemeManager.copyFromSameThemeOverrides(type, null, perUserTheme)
        val backgroundFilesToDelete = backgroundFiles - perUserTheme.value.wallpaper?.imageFile
        backgroundFilesToDelete.forEach(::removeBackgroundImage)
        updateThemeUserDestination()
      }
      saveThemeToDatabase(themeUserDestination.value)
      true
    }

    val onTypeChange = { type: BackgroundImageType? ->
      if (themeUserDestination.value == null) {
        ThemeManager.saveAndApplyBackgroundImage(baseTheme, type)
      } else {
        ThemeManager.applyBackgroundImage(type, perUserTheme)
        updateThemeUserDestination()
      }
      saveThemeToDatabase(themeUserDestination.value)
    }

    val importBackgroundImageLauncher = rememberFileChooserLauncher(true) { to: URI? ->
      if (to != null) {
        val filename = saveBackgroundImage(to)
        if (filename != null) {
          if (themeUserDestination.value == null) {
            removeBackgroundImage((currentTheme.wallpaper.type as? BackgroundImageType.Static)?.filename)
          } else {
            removeBackgroundImage((perUserTheme.value.type as? BackgroundImageType.Static)?.filename)
          }
          onTypeChange(BackgroundImageType.Static(filename, 1f, BackgroundImageScaleType.FILL))
        }
      }
    }

    val currentColors = { type: BackgroundImageType? ->
      // If applying for :
      // - all themes: no overrides needed
      // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
      val perUserOverride = if (themeUserDestination.value == null) null else if (backgroundImageType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
      ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.state.value)
    }

    val onChooseType: (BackgroundImageType?) -> Unit = { type: BackgroundImageType? ->
      when {
        // don't have image in parent or already selected wallpaper with custom image
        type is BackgroundImageType.Static && ((backgroundImageType is BackgroundImageType.Static && themeUserDestination.value?.second != null) || currentColors(type).wallpaper.type.image == null) -> withLongRunningApi { importBackgroundImageLauncher.launch("image/*") }
        type is BackgroundImageType.Static && themeUserDestination.value == null -> onTypeChange(currentColors(type).wallpaper.type)
        type is BackgroundImageType.Static -> onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)
        (themeUserDestination.value != null && themeUserDestination.value?.second?.preferredMode(!CurrentColors.value.colors.isLight)?.type != type) || currentTheme.wallpaper.type != type -> onTypeCopyFromSameTheme(type)
        else -> onTypeChange(type)
      }
    }

    SectionView(stringResource(MR.strings.settings_section_title_themes)) {
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      ThemeDestinationPicker(themeUserDestination)
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))

      WallpaperPresetSelector(
        selectedBackground = backgroundImageType,
        baseTheme = currentTheme.base,
        currentColors = { type ->
          currentColors(type)
        },
        onChooseType = onChooseType,
      )
      val type = MaterialTheme.wallpaper.type
      if (type is BackgroundImageType.Static && (themeUserDestination.value == null || perUserTheme.value.wallpaper?.imageFile != null)) {
        SectionItemView(click = {
          if (themeUserDestination.value == null) {
            val defaultActiveTheme = ThemeManager.defaultActiveTheme(appPrefs.themeOverrides.get())
            ThemeManager.saveAndApplyBackgroundImage(baseTheme, null)
            ThemeManager.removeTheme(defaultActiveTheme?.themeId)
            removeBackgroundImage(type.filename)
          } else {
            removeUserThemeModeOverrides(themeUserDestination, perUserTheme)
          }
          saveThemeToDatabase(themeUserDestination.value)
        }) {
          Text(stringResource(MR.strings.theme_remove_image))
        }
        SectionSpacer()
      }

      val state = remember { derivedStateOf { currentTheme.name } }
      ThemeSelector(state) {
        ThemeManager.applyTheme(it)
        saveThemeToDatabase(null)
      }

      DarkThemeSelector(remember { systemDarkTheme.state }) {
        ThemeManager.changeDarkTheme(it)
        saveThemeToDatabase(null)
      }
    }
    SectionItemView(click = {
      ModalManager.start.showModalCloseable { close ->
        CustomizeThemeView(perUserTheme, themeUserDestination, close = close, updateThemeUserDestination = { updateThemeUserDestination() }, onChooseType = onChooseType) }
      }
    ) {
      Text(stringResource(MR.strings.customize_theme_title))
    }
  }

  @Composable
  fun CustomizeThemeView(perUserTheme: MutableState<ThemeModeOverride>, themeUserDestination: MutableState<Pair<Long, ThemeModeOverrides?>?>, updateThemeUserDestination: () -> Unit, onChooseType: (BackgroundImageType?) -> Unit, close: () -> Unit) {
    ColumnWithScrollBar(
      Modifier.fillMaxWidth(),
    ) {
      val currentTheme by CurrentColors.collectAsState()

      AppBarTitle(stringResource(MR.strings.customize_theme_title))
      val backgroundImage = MaterialTheme.wallpaper.type.image
      val backgroundImageType = MaterialTheme.wallpaper.type
      val baseTheme = CurrentColors.value.base
      // REMOVE IF
      if (false) {
        ChatThemePreview(baseTheme, backgroundImage, backgroundImageType)
        SectionSpacer()
      }

      val editColor = { name: ThemeColor ->
        ModalManager.start.showModal {
          val wallpaperBackgroundColor = MaterialTheme.wallpaper.background ?: backgroundImageType.defaultBackgroundColor(currentTheme.base, MaterialTheme.colors.background)
          val wallpaperTintColor = MaterialTheme.wallpaper.tint ?: backgroundImageType.defaultTintColor(currentTheme.base)
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
          ColorEditor(name, initialColor, baseTheme, MaterialTheme.wallpaper.type, backgroundImage, currentColors = { CurrentColors.value },
            header = {
              // LALAL TO REMOVE
              WallpaperPresetSelector(
                selectedBackground = MaterialTheme.wallpaper.type,
                baseTheme = currentTheme.base,
                currentColors = { type ->
                  // If applying for :
                  // - all themes: no overrides needed
                  // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
                  val perUserOverride = if (themeUserDestination.value == null) null else if (backgroundImageType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
                  ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.state.value)
                },
                onChooseType = onChooseType,
              )
              // LALAL TO REMOVE
            },
            onColorReset = {
              if (themeUserDestination.value == null) {
                ThemeManager.saveAndApplyThemeColor(baseTheme, name, null)
              } else {
                ThemeManager.applyThemeColor(name, null, perUserTheme)
                updateThemeUserDestination()
              }
              saveThemeToDatabase(themeUserDestination.value)
            },
            onColorChange = { color ->
              if (themeUserDestination.value == null) {
                ThemeManager.saveAndApplyThemeColor(baseTheme, name, color)
              } else {
                ThemeManager.applyThemeColor(name, color, perUserTheme)
                updateThemeUserDestination()
              }
              saveThemeToDatabase(themeUserDestination.value)
            }
          )
        }
      }

      // LALAL TO REMOVE
      WallpaperPresetSelector(
        selectedBackground = backgroundImageType,
        baseTheme = currentTheme.base,
        currentColors = { type ->
          // If applying for :
          // - all themes: no overrides needed
          // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
          val perUserOverride = if (themeUserDestination.value == null) null else if (backgroundImageType.sameType(type)) chatModel.currentUser.value?.uiThemes else null
          ThemeManager.currentColors(type, null, perUserOverride, appPrefs.themeOverrides.state.value)
        },
        onChooseType = onChooseType
      )
      // LALAL TO REMOVE
      SectionView(stringResource(MR.strings.settings_section_title_chat_colors).uppercase()) {
        WallpaperSetupView(
          backgroundImageType,
          baseTheme,
          MaterialTheme.wallpaper,
          MaterialTheme.appColors.sentMessage,
          MaterialTheme.appColors.sentQuote,
          MaterialTheme.appColors.receivedMessage,
          MaterialTheme.appColors.receivedQuote,
          editColor = { name ->
            // If no wallpaper is set, nothing to apply new color to. So if user didn't select any wallpaper yet, do it automatically before choosing color
            if (themeUserDestination.value != null && themeUserDestination.value?.second?.preferredMode(!CurrentColors.value.colors.isLight)?.wallpaper == null) {
              onChooseType(currentTheme.wallpaper.type)
            }
            editColor(name)
          },
          onTypeChange = { type ->
            if (themeUserDestination.value == null) {
              ThemeManager.saveAndApplyBackgroundImage(baseTheme, type)
            } else {
              ThemeManager.applyBackgroundImage(type, perUserTheme)
              updateThemeUserDestination()
            }
            saveThemeToDatabase(themeUserDestination.value)
          },
        )
      }
      SectionDividerSpaced(maxTopPadding = true)

      CustomizeThemeColorsSection(currentTheme) { name ->
        editColor(name)
      }

      val canResetColors = if (themeUserDestination.value == null) {
        currentTheme.base.hasChangedAnyColor(currentTheme.colors, currentTheme.appColors, currentTheme.wallpaper)
      } else {
        perUserTheme.value.colors != ThemeColors() || perUserTheme.value.wallpaper?.background != null || perUserTheme.value.wallpaper?.tint != null
      }
      val canRemoveOverrides = if (themeUserDestination.value == null) {
        false
      } else {
        themeUserDestination.value?.second != null
      }
      SectionSpacer()
      if (canResetColors) {
        SectionItemView({
          if (themeUserDestination.value == null) {
            ThemeManager.resetAllThemeColors()
          } else {
            ThemeManager.resetAllThemeColors(perUserTheme)
            updateThemeUserDestination()
          }
          saveThemeToDatabase(themeUserDestination.value)
        }) {
          Text(generalGetString(MR.strings.reset_color), color = colors.primary)
        }
      }
      if (canRemoveOverrides) {
        SectionItemView({
          removeUserThemeModeOverrides(themeUserDestination, perUserTheme)
          saveThemeToDatabase(themeUserDestination.value)
          close()
        }) {
          Text(generalGetString(MR.strings.chat_theme_reset_to_global_theme), color = colors.primary)
        }
      }
      SectionSpacer()

      if (themeUserDestination.value == null) {
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
                saveThemeToDatabase(themeUserDestination.value)
              }
            }
          }
          // Can not limit to YAML mime type since it's unsupported by Android
          SectionItemView({ withLongRunningApi { importThemeLauncher.launch("*/*") } }) {
            Text(generalGetString(MR.strings.import_theme), color = colors.primary)
          }
        }
      }
      SectionBottomSpacer()
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
      listOf(null as Long? to generalGetString(MR.strings.theme_destination_all_profiles))
          +
        chatModel.users.filter { it.user.activeUser || it.user.viewPwdHash == null }.map {
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
      SectionItemViewSpaceBetween({ editColor(ThemeColor.PRIMARY_VARIANT2) }) {
        val title = generalGetString(MR.strings.color_primary_variant2)
        Text(title)
        Icon(painterResource(MR.images.ic_circle_filled), title, tint = currentTheme.appColors.primaryVariant2)
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
    currentColors: () -> ThemeManager.ActiveTheme,
    header: (@Composable () -> Unit)? = null,
    onColorReset: () -> Unit,
    onColorChange: (Color) -> Unit,
  ) {
    ColumnWithScrollBar(
      Modifier
        .fillMaxWidth()
    ) {
      AppBarTitle(name.text)

      val supportedLiveChange = name in listOf(ThemeColor.SECONDARY, ThemeColor.BACKGROUND, ThemeColor.SURFACE, ThemeColor.RECEIVED_MESSAGE, ThemeColor.SENT_MESSAGE, ThemeColor.WALLPAPER_BACKGROUND, ThemeColor.WALLPAPER_TINT)
      // LALAL REMOVE HEADER AND ELSE
      if (supportedLiveChange && header == null) {
        SimpleXThemeOverride(currentColors()) {
          ChatThemePreview(theme, backgroundImage, backgroundImageType, previewBackgroundColor, previewTintColor)
        }
        SectionSpacer()
      } else {
        header?.invoke()
      }

      var currentColor by remember { mutableStateOf(initialColor) }
      val togglePicker = remember { mutableStateOf(false) }
      Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
        if (togglePicker.value) {
          ColorPicker(currentColor, showAlphaBar = backgroundImageType is BackgroundImageType.Static || currentColor.alpha < 1f) {
            currentColor = it
            onColorChange(currentColor)
          }
        } else {
          ColorPicker(currentColor, showAlphaBar = backgroundImageType is BackgroundImageType.Static || currentColor.alpha < 1f) {
            currentColor = it
            onColorChange(currentColor)
          }
        }
      }
      var allowReloadPicker by remember { mutableStateOf(false) }
      KeyChangeEffect(backgroundImageType) {
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
      val savedColor by remember(backgroundImageType) { mutableStateOf(initialColor) }

      Row(Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).height(46.dp)) {
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
        onColorReset()
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
    val values by remember(ChatController.appPrefs.appLanguage.state.value) {
      mutableStateOf(ThemeManager.allThemes().map { it.second to it.third })
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
      darkThemes.add(DefaultTheme.BLACK.themeName to generalGetString(MR.strings.theme_black))
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
  initialWallpaper: AppWallpaper?,
  initialSentColor: Color,
  initialSentQuoteColor: Color,
  initialReceivedColor: Color,
  initialReceivedQuoteColor: Color,
  editColor: (ThemeColor) -> Unit,
  onTypeChange: (BackgroundImageType?) -> Unit,
) {
  if (backgroundImageType is BackgroundImageType.Static) {
    val state = remember(backgroundImageType.scaleType, initialWallpaper?.type) { mutableStateOf(backgroundImageType.scaleType ?: (initialWallpaper?.type as? BackgroundImageType.Static)?.scaleType ?: BackgroundImageScaleType.FILL) }
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
    val state = remember(backgroundImageType, initialWallpaper?.type?.scale) { mutableStateOf(backgroundImageType.scale ?: initialWallpaper?.type?.scale ?: 1f) }
    Row(Modifier.padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
      Text("${state.value}".substring(0, min("${state.value}".length, 4)), Modifier.width(50.dp))
      Slider(
        state.value,
        valueRange = 0.5f..2f,
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

  if (backgroundImageType is BackgroundImageType.Repeated || backgroundImageType is BackgroundImageType.Static) {
    val wallpaperBackgroundColor = initialWallpaper?.background ?: backgroundImageType.defaultBackgroundColor(theme, MaterialTheme.colors.background)
    SectionItemViewSpaceBetween({ editColor(ThemeColor.WALLPAPER_BACKGROUND) }) {
      val title = generalGetString(MR.strings.color_wallpaper_background)
      Text(title)
      Icon(painterResource(MR.images.ic_circle_filled), title, tint = wallpaperBackgroundColor)
    }
    val wallpaperTintColor = initialWallpaper?.tint ?: backgroundImageType.defaultTintColor(theme)
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
  val backgroundFilesToDelete = listOf(
    (chatModel.currentUser.value?.uiThemes?.light?.type as? BackgroundImageType.Static)?.filename,
    (chatModel.currentUser.value?.uiThemes?.dark?.type as? BackgroundImageType.Static)?.filename
  )
  backgroundFilesToDelete.forEach(::removeBackgroundImage)
}
