package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.User
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.UserProfileRow
import chat.simplex.common.views.database.PassphraseField
import chat.simplex.common.views.helpers.*

@Composable
fun HiddenProfileView(
  m: ChatModel,
  user: User,
  close: () -> Unit,
) {
  HiddenProfileLayout(
    user,
    saveProfilePassword = { hidePassword ->
      withBGApi {
        try {
          val u = m.controller.apiHideUser(user.userId, hidePassword)
          m.updateUser(u)
          close()
        } catch (e: Exception) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.error_saving_user_password),
            text = e.stackTraceToString()
          )
        }
      }
    }
  )
}

@Composable
private fun HiddenProfileLayout(
  user: User,
  saveProfilePassword: (String) -> Unit
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.hide_profile))
    SectionView(padding = PaddingValues(start = 8.dp, end = DEFAULT_PADDING)) {
      UserProfileRow(user)
    }
    SectionSpacer()

    val hidePassword = rememberSaveable { mutableStateOf("") }
    val confirmHidePassword = rememberSaveable { mutableStateOf("") }
    val passwordValid by remember { derivedStateOf { hidePassword.value == hidePassword.value.trim() } }
    val confirmValid by remember { derivedStateOf { confirmHidePassword.value == "" || hidePassword.value == confirmHidePassword.value } }
    val saveDisabled by remember { derivedStateOf { hidePassword.value == "" || !passwordValid || confirmHidePassword.value == "" || !confirmValid } }
    SectionView(stringResource(MR.strings.hidden_profile_password).uppercase()) {
      SectionItemView {
        PassphraseField(hidePassword, generalGetString(MR.strings.password_to_show), isValid = { passwordValid }, showStrength = true)
      }
      SectionItemView {
        PassphraseField(confirmHidePassword, stringResource(MR.strings.confirm_password), isValid = { confirmValid }, dependsOn = hidePassword)
      }
      SectionItemViewSpaceBetween({ saveProfilePassword(hidePassword.value) }, disabled = saveDisabled, minHeight = TextFieldDefaults.MinHeight) {
        Text(generalGetString(MR.strings.save_profile_password), color = if (saveDisabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
      }
    }
    SectionTextFooter(stringResource(MR.strings.to_reveal_profile_enter_password))
    SectionBottomSpacer()
  }
}
