package chat.simplex.common.views.localauth

import chat.simplex.common.platform.BackHandler
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import com.icerockdev.library.MR
import chat.simplex.common.views.helpers.DatabaseUtils
import chat.simplex.common.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.common.views.helpers.generalGetString

@Composable
fun SetAppPasscodeView(
  passcodeKeychain: DatabaseUtils.KeyStoreItem = ksAppPassword,
  title: String = generalGetString(MR.strings.new_passcode),
  reason: String? = null,
  submit: () -> Unit,
  cancel: () -> Unit,
  close: () -> Unit
) {
  val passcode = rememberSaveable { mutableStateOf("") }
  var enteredPassword by rememberSaveable { mutableStateOf("") }
  var confirming by rememberSaveable { mutableStateOf(false) }

  @Composable
  fun SetPasswordView(title: String, submitLabel: String, submitEnabled: (((String) -> Boolean))? = null, submit: () -> Unit) {
    BackHandler {
      close()
      cancel()
    }
    PasscodeView(passcode, title = title, reason = reason, submitLabel = submitLabel, submitEnabled = submitEnabled, submit = submit) {
      close()
      cancel()
    }
  }

  if (confirming) {
    SetPasswordView(
      generalGetString(MR.strings.confirm_passcode),
      generalGetString(MR.strings.confirm_verb),
      submitEnabled = { pwd -> pwd == enteredPassword }
    ) {
      if (passcode.value == enteredPassword) {
        passcodeKeychain.set(passcode.value)
        enteredPassword = ""
        passcode.value = ""
        close()
        submit()
      }
    }
  } else {
    SetPasswordView(title, generalGetString(MR.strings.save_verb)) {
      enteredPassword = passcode.value
      passcode.value = ""
      confirming = true
    }
  }
}
