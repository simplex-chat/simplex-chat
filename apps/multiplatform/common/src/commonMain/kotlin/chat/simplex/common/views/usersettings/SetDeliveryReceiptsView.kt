package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*

@Composable
fun SetDeliveryReceiptsView(m: ChatModel) {
  SetDeliveryReceiptsLayout(
    enableReceipts = {
      val currentUser = m.currentUser.value
      if (currentUser != null) {
        withBGApi {
          try {
            m.controller.apiSetAllContactReceipts(currentUser.remoteHostId, enable = true)
            m.currentUser.value = currentUser.copy(sendRcptsContacts = true)
            m.setDeliveryReceipts.value = false
            m.controller.appPrefs.privacyDeliveryReceiptsSet.set(true)
            try {
              val users = m.controller.listUsers(currentUser.remoteHostId)
              m.users.clear()
              m.users.addAll(users)
            } catch (e: Exception) {
              Log.e(TAG, "listUsers error: ${e.stackTraceToString()}")
            }
          } catch (e: Exception) {
            AlertManager.shared.showAlertDialog(
              title = generalGetString(MR.strings.error_enabling_delivery_receipts),
              text = e.stackTraceToString()
            )
            Log.e(TAG, "${generalGetString(MR.strings.error_enabling_delivery_receipts)}: ${e.stackTraceToString()}")
            m.setDeliveryReceipts.value = false
          }
        }
      }
    },
    skip = {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.delivery_receipts_are_disabled),
        text = generalGetString(MR.strings.you_can_enable_delivery_receipts_later_alert),
        confirmText = generalGetString(MR.strings.ok),
        dismissText = generalGetString(MR.strings.dont_show_again),
        onConfirm = {
          m.setDeliveryReceipts.value = false
        },
        onDismiss = {
          m.setDeliveryReceipts.value = false
          m.controller.appPrefs.privacyDeliveryReceiptsSet.set(true)
        }
      )
    },
    userCount = m.users.size
  )
}

@Composable
private fun SetDeliveryReceiptsLayout(
  enableReceipts: () -> Unit,
  skip: () -> Unit,
  userCount: Int,
) {
  val endPadding = if (appPlatform.isDesktop) 56.dp else 0.dp
  val (scrollBarAlpha, scrollModifier, scrollJob) = platform.desktopScrollBarComponents()
  val scrollState = rememberScrollState()
  Column(
    Modifier.fillMaxSize().verticalScroll(scrollState).then(if (appPlatform.isDesktop) scrollModifier else Modifier).padding(top = DEFAULT_PADDING, end = endPadding),
    horizontalAlignment = Alignment.CenterHorizontally,
  ) {
    AppBarTitle(stringResource(MR.strings.delivery_receipts_title))

    Spacer(Modifier.weight(1f))

    EnableReceiptsButton(enableReceipts)
    if (userCount > 1) {
      TextBelowButton(stringResource(MR.strings.sending_delivery_receipts_will_be_enabled_all_profiles))
    } else {
      TextBelowButton(stringResource(MR.strings.sending_delivery_receipts_will_be_enabled))
    }

    Spacer(Modifier.weight(1f))

    SkipButton(skip)

    SectionBottomSpacer()
  }
  if (appPlatform.isDesktop) {
    Box(Modifier.fillMaxSize().padding(end = endPadding)) {
      platform.desktopScrollBar(scrollState, Modifier.align(Alignment.CenterEnd).fillMaxHeight(), scrollBarAlpha, scrollJob, false)
    }
  }
}

@Composable
private fun EnableReceiptsButton(onClick: () -> Unit) {
  TextButton(onClick) {
    Text(stringResource(MR.strings.enable_receipts_all), style = MaterialTheme.typography.h2, color = MaterialTheme.colors.primary)
  }
}

@Composable
private fun SkipButton(onClick: () -> Unit) {
  SimpleButtonIconEnded(stringResource(MR.strings.dont_enable_receipts), painterResource(MR.images.ic_chevron_right), click = onClick)
  TextBelowButton(stringResource(MR.strings.you_can_enable_delivery_receipts_later))
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

