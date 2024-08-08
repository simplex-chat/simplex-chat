package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.font.FontWeight
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
      withBGApi {
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
  ColumnWithScrollBar(
    Modifier
      .fillMaxSize(),
    horizontalAlignment = Alignment.CenterHorizontally,
  ) {
    CloseSheetBar(showClose = false, close = {})
    AppBarTitle(stringResource(MR.strings.simplex_address))

    Spacer(Modifier.weight(1f))

    if (userAddress != null) {
      SimpleXLinkQRCode(userAddress.connReqContact)
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      Row {
        ShareAddressButton { share(simplexChatLink(userAddress.connReqContact)) }
        Spacer(Modifier.width(DEFAULT_PADDING * 2))
        ShareViaEmailButton { sendEmail(userAddress) }
      }
      Spacer(Modifier.height(DEFAULT_PADDING))
      Spacer(Modifier.weight(1f))
      Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
        OnboardingActionButton(
          modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
          labelId = MR.strings.continue_to_next_step,
          onboarding = null,
          onclick = nextStep
        )
        // Reserve space
        TextButtonBelowOnboardingButton("", null)
      }
    } else {
      Button(createAddress, Modifier, shape = CircleShape, contentPadding = PaddingValues()) {
        Icon(painterResource(MR.images.ic_mail_filled), null, Modifier.size(100.dp).background(MaterialTheme.colors.primary, CircleShape).padding(25.dp), tint = Color.White)
      }
      Spacer(Modifier.height(DEFAULT_PADDING))
      Spacer(Modifier.weight(1f))
      Text(stringResource(MR.strings.create_simplex_address), style = MaterialTheme.typography.h3, fontWeight = FontWeight.Bold)
      TextBelowButton(stringResource(MR.strings.you_can_make_address_visible_via_settings))
      Spacer(Modifier.height(DEFAULT_PADDING))
      Spacer(Modifier.weight(1f))

      Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
        OnboardingActionButton(
          modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
          labelId = MR.strings.create_address_button,
          onboarding = null,
          onclick = createAddress
        )
        TextButtonBelowOnboardingButton(stringResource(MR.strings.dont_create_address), nextStep)
      }
    }
  }
}

@Composable
fun ShareAddressButton(onClick: () -> Unit) {
  Column(horizontalAlignment = Alignment.CenterHorizontally) {
    IconButton(onClick, Modifier.padding(bottom = DEFAULT_PADDING_HALF).border(1.dp, MaterialTheme.colors.secondary.copy(0.1f), CircleShape)) {
      Icon(
        painterResource(MR.images.ic_share_filled), generalGetString(MR.strings.share_verb), tint = MaterialTheme.colors.primary,
        modifier = Modifier.size(50.dp).padding(DEFAULT_PADDING_HALF)
      )
    }
    Text(stringResource(MR.strings.share_verb))
  }
}

@Composable
fun ShareViaEmailButton(onClick: () -> Unit) {
  Column(horizontalAlignment = Alignment.CenterHorizontally) {
    IconButton(onClick, Modifier.padding(bottom = DEFAULT_PADDING_HALF).border(1.dp, MaterialTheme.colors.secondary.copy(0.1f), CircleShape)) {
      Icon(
        painterResource(MR.images.ic_mail), generalGetString(MR.strings.share_verb), tint = MaterialTheme.colors.primary,
        modifier = Modifier.size(50.dp).padding(DEFAULT_PADDING_HALF)
      )
    }
    Text(stringResource(MR.strings.invite_friends_short))
  }
}

@Composable
private fun TextBelowButton(text: String) {
  Text(
    text,
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING * 3, vertical = DEFAULT_PADDING_HALF),
    style = MaterialTheme.typography.subtitle1,
    color = MaterialTheme.colors.secondary,
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
  // No visible users but may have hidden. In this case chat should be started anyway because it's stopped on this stage with hidden users
  if (chatModel.users.any { u -> !u.user.hidden }) return
  withBGApi {
    val user = chatModel.controller.apiGetActiveUser(rhId) ?: return@withBGApi
    chatModel.currentUser.value = user
    chatModel.controller.startChat(user)
  }
}
