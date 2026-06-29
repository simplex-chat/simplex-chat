package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*

@Composable
fun ChannelWebPageView(
  rhId: Long?,
  groupInfo: GroupInfo,
  chatModel: ChatModel,
  close: () -> Unit
) {
  val isChannel = groupInfo.isChannel
  val access = groupInfo.groupProfile.publicGroup?.publicGroupAccess
  val webPage = rememberSaveable { mutableStateOf(access?.groupWebPage ?: "") }
  val allowEmbedding = rememberSaveable { mutableStateOf(access?.allowEmbedding ?: false) }
  val groupRelays = remember { mutableStateListOf<GroupRelay>() }

  val dataUnchanged = webPage.value.trim() == (access?.groupWebPage ?: "") &&
    allowEmbedding.value == (access?.allowEmbedding ?: false)

  val save: () -> Unit = {
    withBGApi {
      val trimmedPage = webPage.value.trim()
      val newAccess = PublicGroupAccess(
        groupWebPage = trimmedPage.ifEmpty { null },
        simplexName = access?.simplexName,
        domainWebPage = access?.domainWebPage ?: false,
        allowEmbedding = allowEmbedding.value
      )
      val gp = groupInfo.groupProfile.copy(
        publicGroup = groupInfo.groupProfile.publicGroup?.copy(publicGroupAccess = newAccess)
      )
      val gInfo = chatModel.controller.apiUpdateGroup(rhId, groupInfo.groupId, gp, isChannel)
      if (gInfo != null) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateGroup(rhId, gInfo)
        }
        close()
      }
    }
  }

  val closeWithAlert = {
    if (dataUnchanged) {
      close()
    } else {
      AlertManager.shared.showAlertDialogStacked(
        title = generalGetString(MR.strings.save_preferences_question),
        confirmText = generalGetString(if (isChannel) MR.strings.save_and_notify_channel_subscribers else MR.strings.save_and_notify_group_members),
        dismissText = generalGetString(MR.strings.exit_without_saving),
        onConfirm = save,
        onDismiss = close,
      )
    }
  }

  LaunchedEffect(Unit) {
    val relays = chatModel.controller.apiGetGroupRelays(rhId, groupInfo.groupId)
    groupRelays.clear()
    groupRelays.addAll(relays)
  }

  BackHandler(onBack = closeWithAlert)
  ModalView(close = closeWithAlert, cardScreen = true) {
    ChannelWebPageLayout(
      isChannel = isChannel,
      webPage = webPage,
      allowEmbedding = allowEmbedding,
      groupRelays = groupRelays,
      groupInfo = groupInfo,
      dataUnchanged = dataUnchanged,
      save = save
    )
  }
}

@Composable
private fun ChannelWebPageLayout(
  isChannel: Boolean,
  webPage: MutableState<String>,
  allowEmbedding: MutableState<Boolean>,
  groupRelays: List<GroupRelay>,
  groupInfo: GroupInfo,
  dataUnchanged: Boolean,
  save: () -> Unit
) {
  val clipboard = LocalClipboardManager.current
  ColumnWithScrollBar {
    AppBarTitle(stringResource(if (isChannel) MR.strings.channel_webpage else MR.strings.group_webpage))

    val embedCode = embedCode(groupRelays, groupInfo)
    if (embedCode != null) {
      SectionTextFooter(stringResource(MR.strings.webpage_info))
      SectionDividerSpaced()

      SectionView(stringResource(MR.strings.webpage_code)) {
        SectionItemView {
          Text(
            embedCode,
            style = MaterialTheme.typography.body2.copy(fontFamily = FontFamily.Monospace, fontSize = 12.sp),
            maxLines = 6,
            overflow = TextOverflow.Ellipsis
          )
        }
        SectionItemView({
          clipboard.setText(AnnotatedString(embedCode))
          showToast(generalGetString(MR.strings.copied))
        }) {
          Icon(painterResource(MR.images.ic_content_copy), null, tint = MaterialTheme.colors.primary)
          Spacer(Modifier.width(8.dp))
          Text(stringResource(MR.strings.copy_code), color = MaterialTheme.colors.primary)
        }
      }
      SectionTextFooter(stringResource(MR.strings.webpage_code_footer))
    } else {
      SectionTextFooter(stringResource(MR.strings.relays_no_web_support))
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.enter_webpage_url)) {
      PlainTextEditor(webPage, placeholder = stringResource(MR.strings.web_page_url_placeholder))
    }
    SectionTextFooter(stringResource(MR.strings.webpage_url_footer))
    SectionDividerSpaced()

    SectionView {
      PreferenceToggle(stringResource(MR.strings.allow_anyone_to_embed), checked = allowEmbedding.value) {
        allowEmbedding.value = it
      }
    }
    SectionTextFooter(stringResource(if (allowEmbedding.value) MR.strings.embed_any_webpage_can_show else MR.strings.embed_only_your_page))
    SectionDividerSpaced()

    SectionView {
      SectionItemView(save, disabled = dataUnchanged) {
        Text(
          stringResource(MR.strings.save_verb),
          color = if (dataUnchanged) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
        )
      }
    }

    SectionBottomSpacer()
  }
}

private fun embedCode(groupRelays: List<GroupRelay>, groupInfo: GroupInfo): String? {
  val pg = groupInfo.groupProfile.publicGroup ?: return null
  val relayDomains = groupRelays.mapNotNull { it.relayCap.webDomain }
  if (relayDomains.isEmpty()) return null
  val domains = relayDomains.joinToString(",")
  return """<div data-simplex-channel-preview
  data-channel-link="${pg.groupLink}"
  data-channel-id="${pg.publicGroupId}"
  data-relay-domains="$domains"
  data-app-download-buttons="on"
  data-color-scheme="light"
></div>
<script src="https://simplex.chat/js/channel-preview.js"></script>"""
}
