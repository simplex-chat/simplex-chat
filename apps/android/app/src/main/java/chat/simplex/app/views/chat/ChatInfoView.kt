package chat.simplex.app.views.chat

import InfoRow
import InfoRowEllipsis
import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import android.widget.Toast
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun ChatInfoView(chatModel: ChatModel, connStats: ConnectionStats?, close: () -> Unit) {
  BackHandler(onBack = close)
  val chat = chatModel.chats.firstOrNull { it.id == chatModel.chatId.value }
  val developerTools = chatModel.controller.appPrefs.developerTools.get()
  if (chat != null) {
    ChatInfoLayout(
      chat,
      connStats,
      developerTools,
      deleteContact = { deleteContactDialog(chat.chatInfo, chatModel, close) },
      clearChat = { clearChatDialog(chat.chatInfo, chatModel, close) },
      changeNtfsState = { enabled ->
        changeNtfsState(enabled, chat, chatModel)
      },
    )
  }
}

fun changeNtfsState(enabled: Boolean, chat: Chat, chatModel: ChatModel) {
  val newChatInfo = when(chat.chatInfo) {
    is ChatInfo.Direct -> with (chat.chatInfo) {
      ChatInfo.Direct(contact.copy(chatSettings = contact.chatSettings.copy(enableNtfs = enabled)))
    }
    is ChatInfo.Group -> with(chat.chatInfo) {
      ChatInfo.Group(groupInfo.copy(chatSettings = groupInfo.chatSettings.copy(enableNtfs = enabled)))
    }
    else -> null
  }
  withApi {
    val res = when (newChatInfo) {
      is ChatInfo.Direct -> with(newChatInfo) {
        chatModel.controller.apiSetSettings(chatType, apiId, contact.chatSettings)
      }
      is ChatInfo.Group -> with(newChatInfo) {
        chatModel.controller.apiSetSettings(chatType, apiId, groupInfo.chatSettings)
      }
      else -> false
    }
    if (res && newChatInfo != null) {
      chatModel.updateChatInfo(newChatInfo)
      if (!enabled) {
        chatModel.controller.ntfManager.cancelNotificationsForChat(chat.id)
      }
    }
  }
}

fun deleteContactDialog(chatInfo: ChatInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.delete_contact_question),
    text = generalGetString(R.string.delete_contact_all_messages_deleted_cannot_undo_warning),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = {
      withApi {
        val r = chatModel.controller.apiDeleteChat(chatInfo.chatType, chatInfo.apiId)
        if (r) {
          chatModel.removeChat(chatInfo.id)
          chatModel.chatId.value = null
          chatModel.controller.ntfManager.cancelNotificationsForChat(chatInfo.id)
          close?.invoke()
        }
      }
    }
  )
}

fun clearChatDialog(chatInfo: ChatInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.clear_chat_question),
    text = generalGetString(R.string.clear_chat_warning),
    confirmText = generalGetString(R.string.clear_verb),
    onConfirm = {
      withApi {
        val updatedChatInfo = chatModel.controller.apiClearChat(chatInfo.chatType, chatInfo.apiId)
        if (updatedChatInfo != null) {
          chatModel.clearChat(updatedChatInfo)
          chatModel.controller.ntfManager.cancelNotificationsForChat(chatInfo.id)
          close?.invoke()
        }
      }
    }
  )
}

@Composable
fun ChatInfoLayout(
  chat: Chat,
  connStats: ConnectionStats?,
  developerTools: Boolean,
  deleteContact: () -> Unit,
  clearChat: () -> Unit,
  changeNtfsState: (Boolean) -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start
  ) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.Center
    ) {
      ChatInfoHeader(chat.chatInfo)
    }
    SectionSpacer()

    if (connStats != null) {
      SectionView(title = stringResource(R.string.conn_stats_section_title_servers)) {
        SectionItemView {
          NetworkStatusRow(chat.serverInfo.networkStatus)
        }
        val rcvServers = connStats.rcvServers
        if (rcvServers != null && rcvServers.isNotEmpty()) {
          SectionDivider()
          SimplexServers(stringResource(R.string.receiving_via), rcvServers)
        }
        val sndServers = connStats.sndServers
        if (sndServers != null && sndServers.isNotEmpty()) {
          SectionDivider()
          SimplexServers(stringResource(R.string.sending_via), sndServers)
        }
      }
      SectionSpacer()
    }

    var ntfsEnabled by remember { mutableStateOf(chat.chatInfo.ntfsEnabled) }
    SectionView(title = stringResource(R.string.settings_section_title_settings)) {
      SectionItemView {
        NtfsSwitch(ntfsEnabled) {
          ntfsEnabled = !ntfsEnabled
          changeNtfsState(ntfsEnabled)
        }
      }
    }
    SectionSpacer()

    SectionView {
      SectionItemView {
        ClearChatButton(clearChat)
      }
      SectionDivider()
      SectionItemView {
        DeleteContactButton(deleteContact)
      }
    }
    SectionSpacer()

    if (developerTools) {
      SectionView(title = stringResource(R.string.section_title_for_console)) {
        InfoRow(stringResource(R.string.info_row_local_name), chat.chatInfo.localDisplayName)
        SectionDivider()
        InfoRow(stringResource(R.string.info_row_database_id), chat.chatInfo.apiId.toString())
      }
      SectionSpacer()
    }
  }
}

@Composable
fun ChatInfoHeader(cInfo: ChatInfo) {
  Column(
    Modifier.padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    ChatInfoImage(cInfo, size = 192.dp, iconColor = if (isInDarkTheme()) GroupDark else SettingsSecondaryLight)
    Text(
      cInfo.displayName, style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
      color = MaterialTheme.colors.onBackground,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
    if (cInfo.fullName != "" && cInfo.fullName != cInfo.displayName) {
      Text(
        cInfo.fullName, style = MaterialTheme.typography.h2,
        color = MaterialTheme.colors.onBackground,
        maxLines = 2,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun NetworkStatusRow(networkStatus: Chat.NetworkStatus) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.network_status),
          networkStatus.statusExplanation
        )
      },
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      Text(stringResource(R.string.network_status))
      Icon(
        Icons.Outlined.Info,
        stringResource(R.string.network_status),
        tint = MaterialTheme.colors.primary
      )
    }

    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      Text(
        networkStatus.statusString,
        color = HighOrLowlight
      )
      ServerImage(networkStatus)
    }
  }
}

@Composable
fun ServerImage(networkStatus: Chat.NetworkStatus) {
  Box(Modifier.size(18.dp)) {
    when (networkStatus) {
      is Chat.NetworkStatus.Connected ->
        Icon(Icons.Filled.Circle, stringResource(R.string.icon_descr_server_status_connected), tint = MaterialTheme.colors.primaryVariant)
      is Chat.NetworkStatus.Disconnected ->
        Icon(Icons.Filled.Pending, stringResource(R.string.icon_descr_server_status_disconnected), tint = HighOrLowlight)
      is Chat.NetworkStatus.Error ->
        Icon(Icons.Filled.Error, stringResource(R.string.icon_descr_server_status_error), tint = HighOrLowlight)
      else -> Icon(Icons.Outlined.Circle, stringResource(R.string.icon_descr_server_status_pending), tint = HighOrLowlight)
    }
  }
}

@Composable
fun SimplexServers(text: String, servers: List<String>) {
  val info = servers.joinToString(separator = ", ") { it.substringAfter("@") }
  val clipboardManager: ClipboardManager = LocalClipboardManager.current
  InfoRowEllipsis(text, info) {
    clipboardManager.setText(AnnotatedString(servers.joinToString(separator = ",")))
    Toast.makeText(SimplexApp.context, generalGetString(R.string.copied), Toast.LENGTH_SHORT).show()
  }
}

@Composable
fun NtfsSwitch(
  ntfsEnabled: Boolean,
  toggleNtfs: (Boolean) -> Unit
) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      Icon(
        Icons.Outlined.Notifications,
        stringResource(R.string.notifications),
        tint = HighOrLowlight
      )
      Text(stringResource(R.string.notifications))
    }
    Switch(
      checked = ntfsEnabled,
      onCheckedChange = toggleNtfs,
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      ),
    )
  }
}

@Composable
fun ClearChatButton(clearChat: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { clearChat() },
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Restore,
      stringResource(R.string.clear_chat_button),
      tint = WarningOrange
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.clear_chat_button), color = WarningOrange)
  }
}

@Composable
fun DeleteContactButton(deleteContact: () -> Unit) {
  Row(
    Modifier
      .fillMaxSize()
      .clickable { deleteContact() },
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      Icons.Outlined.Delete,
      stringResource(R.string.button_delete_contact),
      tint = Color.Red
    )
    Spacer(Modifier.size(8.dp))
    Text(stringResource(R.string.button_delete_contact), color = Color.Red)
  }
}

@Preview
@Composable
fun PreviewChatInfoLayout() {
  SimpleXTheme {
    ChatInfoLayout(
      chat = Chat(
        chatInfo = ChatInfo.Direct.sampleData,
        chatItems = arrayListOf(),
        serverInfo = Chat.ServerInfo(Chat.NetworkStatus.Error("agent BROKER TIMEOUT"))
      ),
      changeNtfsState = {},
      developerTools = false,
      connStats = null,
      deleteContact = {}, clearChat = {}
    )
  }
}
