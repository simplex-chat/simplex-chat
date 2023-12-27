package chat.simplex.common.views.onboarding

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.SimpleXLinkQRCode
import chat.simplex.common.views.newchat.simplexChatLink
import chat.simplex.res.MR

@Composable
fun CreateSimpleXAddress(m: ChatModel, rhId: Long?) {
  var progressIndicator by remember { mutableStateOf(false) }
  val userAddress = remember { m.userAddress }
  val clipboard = LocalClipboardManager.current
  val uriHandler = LocalUriHandler.current

  LaunchedEffect(Unit) {
    prepareChatBeforeAddressCreation(rhId)
  }

  CreateSimpleXAddressLayout(
    userAddress.value,
    share = { address: String -> clipboard.shareText(address) },
    sendEmail = { address ->
      uriHandler.sendEmail(
        generalGetString(MR.strings.email_invite_subject),
        generalGetString(MR.strings.email_invite_body).format(simplexChatLink(address.connReqContact))
      )
    },
    createAddress = {
      withApi {
        progressIndicator = true
        val connReqContact = m.controller.apiCreateUserAddress(rhId)
        if (connReqContact != null) {
          m.userAddress.value = UserContactLinkRec(connReqContact)
          progressIndicator = false
        }
      }
    },
    nextStep = {
      val next = if (appPlatform.isAndroid) {
        OnboardingStage.Step4_SetNotificationsMode
      } else {
        OnboardingStage.OnboardingComplete
      }
      m.controller.appPrefs.onboardingStage.set(next)
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
    AppBarTitle(stringResource(MR.strings.simplex_address))

    Spacer(Modifier.weight(1f))

    if (userAddress != null) {
      SimpleXLinkQRCode(userAddress.connReqContact, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).aspectRatio(1f))
      ShareAddressButton { share(simplexChatLink(userAddress.connReqContact)) }
      Spacer(Modifier.weight(1f))
      ShareViaEmailButton { sendEmail(userAddress) }
      Spacer(Modifier.weight(1f))
      ContinueButton(nextStep)
    } else {
      CreateAddressButton(createAddress)
      TextBelowButton(stringResource(MR.strings.you_can_make_address_visible_via_settings))
      Spacer(Modifier.weight(1f))
      SkipButton(nextStep)
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun CreateAddressButton(onClick: () -> Unit) {
  TextButton(onClick) {
    Text(stringResource(MR.strings.create_simplex_address), style = MaterialTheme.typography.h2, color = MaterialTheme.colors.primary)
  }
}

@Composable
fun ShareAddressButton(onClick: () -> Unit) {
  SimpleButtonFrame(onClick) {
    Icon(
      painterResource(MR.images.ic_share_filled), generalGetString(MR.strings.share_verb), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(end = 8.dp).size(18.dp)
    )
    Text(stringResource(MR.strings.share_verb), style = MaterialTheme.typography.caption, color = MaterialTheme.colors.primary)
  }
}

@Composable
fun ShareViaEmailButton(onClick: () -> Unit) {
  SimpleButtonFrame(onClick) {
    Icon(
      painterResource(MR.images.ic_mail), generalGetString(MR.strings.share_verb), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(end = 8.dp).size(30.dp)
    )
    Text(stringResource(MR.strings.invite_friends), style = MaterialTheme.typography.h6, color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun ContinueButton(onClick: () -> Unit) {
  SimpleButtonIconEnded(stringResource(MR.strings.continue_to_next_step), painterResource(MR.images.ic_chevron_right), click = onClick)
}

@Composable
private fun SkipButton(onClick: () -> Unit) {
  SimpleButtonIconEnded(stringResource(MR.strings.dont_create_address), painterResource(MR.images.ic_chevron_right), click = onClick)
  TextBelowButton(stringResource(MR.strings.you_can_create_it_later))
}

@Composable
private fun TextBelowButton(text: String) {
  Text(
    text,
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING * 3),
    style = MaterialTheme.typography.subtitle1,
    textAlign = TextAlign.Center,
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

private fun prepareChatBeforeAddressCreation(rhId: Long?) {
  if (chatModel.users.isNotEmpty()) return
  withApi {
    val user = chatModel.controller.apiGetActiveUser(rhId) ?: return@withApi
    chatModel.currentUser.value = user
    if (chatModel.users.isEmpty()) {
      if (appPlatform.isDesktop) {
        // Make possible to use chat after going to remote device linking and returning back to local profile creation
        chatModel.chatRunning.value = false
      }
      chatModel.controller.startChat(user)
    } else {
      val users = chatModel.controller.listUsers(rhId)
      chatModel.users.clear()
      chatModel.users.addAll(users)
      chatModel.controller.getUserChatData(rhId)
    }
  }
}
