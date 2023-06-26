package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import android.app.Activity
import android.content.ComponentName
import android.content.pm.PackageManager
import android.content.pm.PackageManager.COMPONENT_ENABLED_STATE_DEFAULT
import android.content.pm.PackageManager.COMPONENT_ENABLED_STATE_ENABLED
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.graphics.drawable.toBitmap
import chat.simplex.common.R
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.helpers.APPLICATION_ID
import chat.simplex.common.helpers.saveAppLocale
import chat.simplex.common.views.usersettings.AppearanceScope.ColorEditor
import kotlinx.coroutines.delay

enum class AppIcon(val resId: Int) {
  DEFAULT(R.drawable.icon_round_common),
  DARK_BLUE(R.drawable.icon_dark_blue_round_common),
}

@Composable
actual fun AppearanceView(m: ChatModel, showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)) {
  val appIcon = remember { mutableStateOf(findEnabledIcon()) }

  fun setAppIcon(newIcon: AppIcon) {
    if (appIcon.value == newIcon) return
    val newComponent = ComponentName(APPLICATION_ID, "chat.simplex.app.MainActivity_${newIcon.name.lowercase()}")
    val oldComponent = ComponentName(APPLICATION_ID, "chat.simplex.app.MainActivity_${appIcon.value.name.lowercase()}")
    androidAppContext.packageManager.setComponentEnabledSetting(
      newComponent,
      COMPONENT_ENABLED_STATE_ENABLED, PackageManager.DONT_KILL_APP
    )

    androidAppContext.packageManager.setComponentEnabledSetting(
      oldComponent,
      PackageManager.COMPONENT_ENABLED_STATE_DISABLED, PackageManager.DONT_KILL_APP
    )

    appIcon.value = newIcon
  }

  AppearanceScope.AppearanceLayout(
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

@Composable
fun AppearanceScope.AppearanceLayout(
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
    AppBarTitle(stringResource(MR.strings.appearance_settings))
    SectionView(stringResource(MR.strings.settings_section_title_language), padding = PaddingValues()) {
      val context = LocalContext.current
      //      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
      //        SectionItemWithValue(
      //          generalGetString(MR.strings.settings_section_title_language).lowercase().replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.US) else it.toString() },
      //          remember { mutableStateOf("system") },
      //          listOf(ValueTitleDesc("system", generalGetString(MR.strings.change_verb), "")),
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

    SectionView(stringResource(MR.strings.settings_section_title_icon), padding = PaddingValues(horizontal = DEFAULT_PADDING_HALF)) {
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
    ThemesSection(systemDarkTheme, showSettingsModal, editColor)
    SectionBottomSpacer()
  }
}

private fun findEnabledIcon(): AppIcon = AppIcon.values().first { icon ->
  androidAppContext.packageManager.getComponentEnabledSetting(
    ComponentName(APPLICATION_ID, "chat.simplex.app.MainActivity_${icon.name.lowercase()}")
  ).let { it == COMPONENT_ENABLED_STATE_DEFAULT || it == COMPONENT_ENABLED_STATE_ENABLED }
}

@Preview
@Composable
fun PreviewAppearanceSettings() {
  SimpleXTheme {
    AppearanceScope.AppearanceLayout(
      icon = remember { mutableStateOf(AppIcon.DARK_BLUE) },
      languagePref = SharedPreference({ null }, {}),
      systemDarkTheme = SharedPreference({ null }, {}),
      changeIcon = {},
      showSettingsModal = { {} },
      editColor = { _, _ -> },
    )
  }
}
