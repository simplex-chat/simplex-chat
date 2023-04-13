package chat.simplex.app.views.localauth

import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.helpers.*

@Composable
fun LocalAuthView(m: ChatModel, authRequest: LocalAuthRequest) {
  val passcode = rememberSaveable { mutableStateOf("") }
  PasscodeView(m, passcode, authRequest.title ?: stringResource(R.string.la_enter_app_password), authRequest.reason, stringResource(R.string.submit_passcode),
    submit = {
      val r: LAResult = if (passcode.value == authRequest.password) LAResult.Success else LAResult.Failed(generalGetString(R.string.incorrect_passcode))
      authRequest.completed(r)
    },
    cancel = {
      authRequest.completed(LAResult.Failed(generalGetString(R.string.authentication_cancelled)))
    })
}

