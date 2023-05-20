package chat.simplex.app.views.onboarding

import SectionBottomSpacer
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.UserContactLinkRec
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun CreateSimpleXAddress(m: ChatModel) {
  val context = LocalContext.current
  var progressIndicator by remember { mutableStateOf(false) }
  val userAddress = remember { m.userAddress }

  CreateSimpleXAddressLayout(
    userAddress.value,
    share = { address: String -> shareText(context, address) },
    sendEmail = { address ->
      sendEmail(
        context,
        generalGetString(R.string.email_invite_subject),
        generalGetString(R.string.email_invite_body).format(address.connReqContact)
      )
    },
    createAddress = {
      withApi {
        progressIndicator = true
        val connReqContact = m.controller.apiCreateUserAddress()
        if (connReqContact != null) {
          m.userAddress.value = UserContactLinkRec(connReqContact)
//          TODO uncomment in v5.2
//          try {
//            val u = m.controller.apiSetProfileAddress(true)
//            if (u != null) {
//              m.updateUser(u)
//            }
//          } catch (e: Exception) {
//            Log.e(TAG, "CreateSimpleXAddress apiSetProfileAddress: ${e.stackTraceToString()}")
//          }
          progressIndicator = false
        }
      }
    },
    nextStep = {
      m.controller.appPrefs.onboardingStage.set(OnboardingStage.Step4_SetNotificationsMode)
      m.onboardingStage.value = OnboardingStage.Step4_SetNotificationsMode
    },
  )

  if (progressIndicator) {
    ProgressIndicator()
  }
}

@Composable
private fun CreateSimpleXAddressLayout(
  userAddress: UserContactLinkRec?,
  share: (String) -> Unit,
  sendEmail: (UserContactLinkRec) -> Unit,
  createAddress: () -> Unit,
  nextStep: () -> Unit,
) {
  Column(
    Modifier.fillMaxSize().verticalScroll(rememberScrollState()).padding(top = DEFAULT_PADDING),
    horizontalAlignment = Alignment.CenterHorizontally,
  ) {
    AppBarTitle(stringResource(R.string.simplex_address))

    Spacer(Modifier.weight(1f))

    if (userAddress != null) {
      QRCode(userAddress.connReqContact, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).aspectRatio(1f))
      ShareAddressButton { share(userAddress.connReqContact) }
      Spacer(Modifier.weight(1f))
      ShareViaEmailButton { sendEmail(userAddress) }
      Spacer(Modifier.weight(1f))
      ContinueButton(nextStep)
    } else {
      CreateAddressButton(createAddress)
//      TODO remove color in v5.2
      TextBelowButton(stringResource(R.string.your_contacts_will_see_it), color = Color.Transparent)
      Spacer(Modifier.weight(1f))
      SkipButton(nextStep)
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun CreateAddressButton(onClick: () -> Unit) {
  TextButton(onClick) {
    Text(stringResource(R.string.create_simplex_address), style = MaterialTheme.typography.h2, color = MaterialTheme.colors.primary)
  }
}

@Composable
fun ShareAddressButton(onClick: () -> Unit) {
  SimpleButtonFrame(onClick) {
    Icon(
      painterResource(R.drawable.ic_share_filled), generalGetString(R.string.share_verb), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(end = 8.dp).size(18.dp)
    )
    Text(stringResource(R.string.share_verb), style = MaterialTheme.typography.caption, color = MaterialTheme.colors.primary)
  }
}

@Composable
fun ShareViaEmailButton(onClick: () -> Unit) {
  SimpleButtonFrame(onClick) {
    Icon(
      painterResource(R.drawable.ic_mail), generalGetString(R.string.share_verb), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(end = 8.dp).size(30.dp)
    )
    Text(stringResource(R.string.invite_friends), style = MaterialTheme.typography.h6, color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun ContinueButton(onClick: () -> Unit) {
  SimpleButtonIconEnded(stringResource(R.string.continue_to_next_step), painterResource(R.drawable.ic_chevron_right), click = onClick)
}

@Composable
private fun SkipButton(onClick: () -> Unit) {
  SimpleButtonIconEnded(stringResource(R.string.dont_create_address), painterResource(R.drawable.ic_chevron_right), click = onClick)
  TextBelowButton(stringResource(R.string.you_can_create_it_later))
}

@Composable
private fun TextBelowButton(text: String, color: Color = Color.Unspecified) {
  // TODO remove color in v5.2
  Text(
    text,
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING * 3),
    style = MaterialTheme.typography.subtitle1,
    textAlign = TextAlign.Center,
    color = color
  )
}

@Composable
private fun ProgressIndicator() {
  Box(
    Modifier.fillMaxSize(),
    contentAlignment = Alignment.Center
  ) {
    CircularProgressIndicator(
      Modifier
        .padding(horizontal = 2.dp)
        .size(30.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 3.dp
    )
  }
}
