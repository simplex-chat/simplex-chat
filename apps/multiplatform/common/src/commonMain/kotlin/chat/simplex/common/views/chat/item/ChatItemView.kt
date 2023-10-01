package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.chat.ComposeContextItem
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.res.MR
import kotlinx.datetime.Clock

// TODO refactor so that FramedItemView can show all CIContent items if they're deleted (see Swift code)

val chatEventStyle = SpanStyle(fontSize = 12.sp, fontWeight = FontWeight.Light, color = CurrentColors.value.colors.secondary)

fun chatEventText(ci: ChatItem): AnnotatedString =
  chatEventText(ci.content.text, ci.timestampText)

fun chatEventText(eventText: String, ts: String): AnnotatedString =
  buildAnnotatedString {
    withStyle(chatEventStyle) { append("$eventText  $ts") }
  }

@Composable
fun ChatItemView(
  cInfo: ChatInfo,
  cItem: ChatItem,
  composeState: MutableState<ComposeState>,
  imageProvider: (() -> ImageGalleryProvider)? = null,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long, Boolean) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  acceptCall: (Contact) -> Unit,
  scrollToItem: (Long) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  openDirectChat: (Long) -> Unit,
  updateContactStats: (Contact) -> Unit,
  updateMemberStats: (GroupInfo, GroupMember) -> Unit,
  syncContactConnection: (Contact) -> Unit,
  syncMemberConnection: (GroupInfo, GroupMember) -> Unit,
  findModelChat: (String) -> Chat?,
  findModelMember: (String) -> GroupMember?,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  getConnectedMemberNames: (() -> List<String>)? = null,
  developerTools: Boolean,
) {
  val uriHandler = LocalUriHandler.current
  val sent = cItem.chatDir.sent
  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart
  val showMenu = remember { mutableStateOf(false) }
  val revealed = remember { mutableStateOf(false) }
  val fullDeleteAllowed = remember(cInfo) { cInfo.featureEnabled(ChatFeature.FullDelete) }
  val onLinkLongClick = { _: String -> showMenu.value = true }
  val live = composeState.value.liveMessage != null

  Box(
    modifier = Modifier
      .padding(bottom = 4.dp)
      .fillMaxWidth(),
    contentAlignment = alignment,
  ) {
    val onClick = {
      when (cItem.meta.itemStatus) {
        is CIStatus.SndErrorAuth -> {
          showMsgDeliveryErrorAlert(generalGetString(MR.strings.message_delivery_error_desc))
        }
        is CIStatus.SndError -> {
          showMsgDeliveryErrorAlert(generalGetString(MR.strings.unknown_error) + ": ${cItem.meta.itemStatus.agentError}")
        }
        else -> {}
      }
    }

    @Composable
    fun ChatItemReactions() {
      Row(verticalAlignment = Alignment.CenterVertically) {
        cItem.reactions.forEach { r ->
          var modifier = Modifier.padding(horizontal = 5.dp, vertical = 2.dp).clip(RoundedCornerShape(8.dp))
          if (cInfo.featureEnabled(ChatFeature.Reactions) && (cItem.allowAddReaction || r.userReacted)) {
            modifier = modifier.clickable {
              setReaction(cInfo, cItem, !r.userReacted, r.reaction)
            }
          }
          Row(modifier.padding(2.dp)) {
            ReactionIcon(r.reaction.text, fontSize = 12.sp)
            if (r.totalReacted > 1) {
              Spacer(Modifier.width(4.dp))
              Text(
                "${r.totalReacted}",
                fontSize = 11.5.sp,
                fontWeight = if (r.userReacted) FontWeight.Bold else FontWeight.Normal,
                color = if (r.userReacted) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
                modifier = if (appPlatform.isAndroid) Modifier else Modifier.padding(top = 4.dp)
              )
            }
          }
        }
      }
    }

    Column(horizontalAlignment = if (cItem.chatDir.sent) Alignment.End else Alignment.Start) {
      Column(
        Modifier
          .clip(RoundedCornerShape(18.dp))
          .combinedClickable(onLongClick = { showMenu.value = true }, onClick = onClick)
          .onRightClick { showMenu.value = true },
      ) {
        @Composable
        fun framedItemView() {
          FramedItemView(cInfo, cItem, uriHandler, imageProvider, linkMode = linkMode, showMenu, receiveFile, onLinkLongClick, scrollToItem)
        }

        fun deleteMessageQuestionText(): String {
          return if (fullDeleteAllowed) {
            generalGetString(MR.strings.delete_message_cannot_be_undone_warning)
          } else {
            generalGetString(MR.strings.delete_message_mark_deleted_warning)
          }
        }

        fun moderateMessageQuestionText(): String {
          return if (fullDeleteAllowed) {
            generalGetString(MR.strings.moderate_message_will_be_deleted_warning)
          } else {
            generalGetString(MR.strings.moderate_message_will_be_marked_warning)
          }
        }

        @Composable
        fun MsgReactionsMenu() {
          val rs = MsgReaction.values.mapNotNull { r ->
            if (null == cItem.reactions.find { it.userReacted && it.reaction.text == r.text }) {
              r
            } else {
              null
            }
          }
          if (rs.isNotEmpty()) {
            Row(modifier = Modifier.padding(horizontal = DEFAULT_PADDING).horizontalScroll(rememberScrollState()), verticalAlignment = Alignment.CenterVertically) {
              rs.forEach() { r ->
                Box(
                  Modifier.size(36.dp).clickable {
                    setReaction(cInfo, cItem, true, r)
                    showMenu.value = false
                  },
                  contentAlignment = Alignment.Center
                ) {
                  ReactionIcon(r.text, 12.sp)
                }
              }
            }
          }
        }

        @Composable
        fun MsgContentItemDropdownMenu() {
          val saveFileLauncher = rememberSaveFileLauncher(ciFile = cItem.file)
          DefaultDropdownMenu(showMenu) {
            if (cInfo.featureEnabled(ChatFeature.Reactions) && cItem.allowAddReaction) {
              MsgReactionsMenu()
            }
            if (cItem.meta.itemDeleted == null && !live) {
              ItemAction(stringResource(MR.strings.reply_verb), painterResource(MR.images.ic_reply), onClick = {
                if (composeState.value.editing) {
                  composeState.value = ComposeState(contextItem = ComposeContextItem.QuotedItem(cItem), useLinkPreviews = useLinkPreviews)
                } else {
                  composeState.value = composeState.value.copy(contextItem = ComposeContextItem.QuotedItem(cItem))
                }
                showMenu.value = false
              })
            }
            val clipboard = LocalClipboardManager.current
            ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
              val fileSource = getLoadedFileSource(cItem.file)
              when {
                fileSource != null -> shareFile(cItem.text, fileSource)
                else -> clipboard.shareText(cItem.content.text)
              }
              showMenu.value = false
            })
            ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
              clipboard.setText(AnnotatedString(cItem.content.text))
              showMenu.value = false
            })
            if ((cItem.content.msgContent is MsgContent.MCImage || cItem.content.msgContent is MsgContent.MCVideo || cItem.content.msgContent is MsgContent.MCFile || cItem.content.msgContent is MsgContent.MCVoice) && getLoadedFilePath(cItem.file) != null) {
              SaveContentItemAction(cItem, saveFileLauncher, showMenu)
            }
            if (cItem.meta.editable && cItem.content.msgContent !is MsgContent.MCVoice && !live) {
              ItemAction(stringResource(MR.strings.edit_verb), painterResource(MR.images.ic_edit_filled), onClick = {
                composeState.value = ComposeState(editingItem = cItem, useLinkPreviews = useLinkPreviews)
                showMenu.value = false
              })
            }
            if (cItem.meta.itemDeleted != null && revealed.value) {
              ItemAction(
                stringResource(MR.strings.hide_verb),
                painterResource(MR.images.ic_visibility_off),
                onClick = {
                  revealed.value = false
                  showMenu.value = false
                }
              )
            }
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            if (cItem.meta.itemDeleted == null && cItem.file != null && cItem.file.cancelAction != null) {
              CancelFileItemAction(cItem.file.fileId, showMenu, cancelFile = cancelFile, cancelAction = cItem.file.cancelAction)
            }
            if (!(live && cItem.meta.isLive)) {
              DeleteItemAction(cItem, showMenu, questionText = deleteMessageQuestionText(), deleteMessage)
            }
            val groupInfo = cItem.memberToModerate(cInfo)?.first
            if (groupInfo != null) {
              ModerateItemAction(cItem, questionText = moderateMessageQuestionText(), showMenu, deleteMessage)
            }
          }
        }

        @Composable
        fun MarkedDeletedItemDropdownMenu() {
          DefaultDropdownMenu(showMenu) {
            if (!cItem.isDeletedContent) {
              ItemAction(
                stringResource(MR.strings.reveal_verb),
                painterResource(MR.images.ic_visibility),
                onClick = {
                  revealed.value = true
                  showMenu.value = false
                }
              )
            }
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            DeleteItemAction(cItem, showMenu, questionText = deleteMessageQuestionText(), deleteMessage)
          }
        }

        @Composable
        fun ContentItem() {
          val mc = cItem.content.msgContent
          if (cItem.meta.itemDeleted != null && !revealed.value) {
            MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL)
            MarkedDeletedItemDropdownMenu()
          } else {
            if (cItem.quotedItem == null && cItem.meta.itemDeleted == null && !cItem.meta.isLive) {
              if (mc is MsgContent.MCText && isShortEmoji(cItem.content.text)) {
                EmojiItemView(cItem, cInfo.timedMessagesTTL)
              } else if (mc is MsgContent.MCVoice && cItem.content.text.isEmpty()) {
                CIVoiceView(mc.duration, cItem.file, cItem.meta.itemEdited, cItem.chatDir.sent, hasText = false, cItem, cInfo.timedMessagesTTL, longClick = { onLinkLongClick("") }, receiveFile)
              } else {
                framedItemView()
              }
            } else {
              framedItemView()
            }
            MsgContentItemDropdownMenu()
          }
        }

        @Composable fun DeletedItem() {
          DeletedItemView(cItem, cInfo.timedMessagesTTL)
          DefaultDropdownMenu(showMenu) {
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            DeleteItemAction(cItem, showMenu, questionText = deleteMessageQuestionText(), deleteMessage)
          }
        }

        @Composable fun CallItem(status: CICallStatus, duration: Int) {
          CICallItemView(cInfo, cItem, status, duration, acceptCall)
        }

        fun eventItemViewText(): AnnotatedString {
          val memberDisplayName = cItem.memberDisplayName
          return if (memberDisplayName != null) {
            buildAnnotatedString {
              withStyle(chatEventStyle) { append(memberDisplayName) }
              append(" ")
            }.plus(chatEventText(cItem))
          } else {
            chatEventText(cItem)
          }
        }

        @Composable fun EventItemView() {
          CIEventView(eventItemViewText())
        }

        fun membersConnectedText(): String? {
          return if (getConnectedMemberNames != null) {
            val ns = getConnectedMemberNames()
            when {
              ns.size > 3 -> String.format(generalGetString(MR.strings.rcv_group_event_n_members_connected), ns[0], ns[1], ns.size - 2)
              ns.size == 3 -> String.format(generalGetString(MR.strings.rcv_group_event_3_members_connected), ns[0], ns[1], ns[2])
              ns.size == 2 -> String.format(generalGetString(MR.strings.rcv_group_event_2_members_connected), ns[0], ns[1])
              else -> null
            }
          } else {
            null
          }
        }

        fun membersConnectedItemText(): AnnotatedString {
          val t = membersConnectedText()
          return if (t != null) {
            chatEventText(t, cItem.timestampText)
          } else {
            eventItemViewText()
          }
        }

        @Composable
        fun ModeratedItem() {
          MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL)
          DefaultDropdownMenu(showMenu) {
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            DeleteItemAction(cItem, showMenu, questionText = generalGetString(MR.strings.delete_message_cannot_be_undone_warning), deleteMessage)
          }
        }

        when (val c = cItem.content) {
          is CIContent.SndMsgContent -> ContentItem()
          is CIContent.RcvMsgContent -> ContentItem()
          is CIContent.SndDeleted -> DeletedItem()
          is CIContent.RcvDeleted -> DeletedItem()
          is CIContent.SndCall -> CallItem(c.status, c.duration)
          is CIContent.RcvCall -> CallItem(c.status, c.duration)
          is CIContent.RcvIntegrityError -> if (developerTools) {
            IntegrityErrorItemView(c.msgError, cItem, cInfo.timedMessagesTTL)
          } else {
            Box(Modifier.size(0.dp)) {}
          }
          is CIContent.RcvDecryptionError -> CIRcvDecryptionError(c.msgDecryptError, c.msgCount, cInfo, cItem, updateContactStats = updateContactStats, updateMemberStats = updateMemberStats, syncContactConnection = syncContactConnection, syncMemberConnection = syncMemberConnection, findModelChat = findModelChat, findModelMember = findModelMember)
          is CIContent.RcvGroupInvitation -> CIGroupInvitationView(cItem, c.groupInvitation, c.memberRole, joinGroup = joinGroup, chatIncognito = cInfo.incognito)
          is CIContent.SndGroupInvitation -> CIGroupInvitationView(cItem, c.groupInvitation, c.memberRole, joinGroup = joinGroup, chatIncognito = cInfo.incognito)
          is CIContent.RcvGroupEventContent -> when (c.rcvGroupEvent) {
            is RcvGroupEvent.MemberConnected -> CIEventView(membersConnectedItemText())
            is RcvGroupEvent.MemberCreatedContact -> CIMemberCreatedContactView(cItem, openDirectChat)
            else -> EventItemView()
          }
          is CIContent.SndGroupEventContent -> EventItemView()
          is CIContent.RcvConnEventContent -> EventItemView()
          is CIContent.SndConnEventContent -> EventItemView()
          is CIContent.RcvChatFeature -> CIChatFeatureView(cItem, c.feature, c.enabled.iconColor)
          is CIContent.SndChatFeature -> CIChatFeatureView(cItem, c.feature, c.enabled.iconColor)
          is CIContent.RcvChatPreference -> {
            val ct = if (cInfo is ChatInfo.Direct) cInfo.contact else null
            CIFeaturePreferenceView(cItem, ct, c.feature, c.allowed, acceptFeature)
          }
          is CIContent.SndChatPreference -> CIChatFeatureView(cItem, c.feature, MaterialTheme.colors.secondary, icon = c.feature.icon,)
          is CIContent.RcvGroupFeature -> CIChatFeatureView(cItem, c.groupFeature, c.preference.enable.iconColor)
          is CIContent.SndGroupFeature -> CIChatFeatureView(cItem, c.groupFeature, c.preference.enable.iconColor)
          is CIContent.RcvChatFeatureRejected -> CIChatFeatureView(cItem, c.feature, Color.Red)
          is CIContent.RcvGroupFeatureRejected -> CIChatFeatureView(cItem, c.groupFeature, Color.Red)
          is CIContent.SndModerated -> ModeratedItem()
          is CIContent.RcvModerated -> ModeratedItem()
          is CIContent.InvalidJSON -> CIInvalidJSONView(c.json)
        }
      }

      if (cItem.content.msgContent != null && (cItem.meta.itemDeleted == null || revealed.value) && cItem.reactions.isNotEmpty()) {
        ChatItemReactions()
      }
    }
  }
}

@Composable
expect fun ReactionIcon(text: String, fontSize: TextUnit = TextUnit.Unspecified)

@Composable
expect fun SaveContentItemAction(cItem: ChatItem, saveFileLauncher: FileChooserLauncher, showMenu: MutableState<Boolean>)

@Composable
fun CancelFileItemAction(
  fileId: Long,
  showMenu: MutableState<Boolean>,
  cancelFile: (Long) -> Unit,
  cancelAction: CancelAction
) {
  ItemAction(
    stringResource(cancelAction.uiActionId),
    painterResource(MR.images.ic_close),
    onClick = {
      showMenu.value = false
      cancelFileAlertDialog(fileId, cancelFile = cancelFile, cancelAction = cancelAction)
    },
    color = Color.Red
  )
}

@Composable
fun ItemInfoAction(
  cInfo: ChatInfo,
  cItem: ChatItem,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  showMenu: MutableState<Boolean>
) {
  ItemAction(
    stringResource(MR.strings.info_menu),
    painterResource(MR.images.ic_info),
    onClick = {
      showItemDetails(cInfo, cItem)
      showMenu.value = false
    }
  )
}


@Composable
fun DeleteItemAction(
  cItem: ChatItem,
  showMenu: MutableState<Boolean>,
  questionText: String,
  deleteMessage: (Long, CIDeleteMode) -> Unit
) {
  ItemAction(
    stringResource(MR.strings.delete_verb),
    painterResource(MR.images.ic_delete),
    onClick = {
      showMenu.value = false
      deleteMessageAlertDialog(cItem, questionText, deleteMessage = deleteMessage)
    },
    color = Color.Red
  )
}

@Composable
fun ModerateItemAction(
  cItem: ChatItem,
  questionText: String,
  showMenu: MutableState<Boolean>,
  deleteMessage: (Long, CIDeleteMode) -> Unit
) {
  ItemAction(
    stringResource(MR.strings.moderate_verb),
    painterResource(MR.images.ic_flag),
    onClick = {
      showMenu.value = false
      moderateMessageAlertDialog(cItem, questionText, deleteMessage = deleteMessage)
    },
    color = Color.Red
  )
}

@Composable
fun ItemAction(text: String, icon: Painter, onClick: () -> Unit, color: Color = Color.Unspecified) {
  val finalColor = if (color == Color.Unspecified) {
    if (isInDarkTheme()) MenuTextColorDark else Color.Black
  } else color
  DropdownMenuItem(onClick, contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Text(
        text,
        modifier = Modifier
          .fillMaxWidth()
          .weight(1F)
          .padding(end = 15.dp),
        color = finalColor
      )
      Icon(icon, text, tint = finalColor)
    }
  }
}

@Composable
fun ItemAction(text: String, icon: ImageVector, onClick: () -> Unit, color: Color = Color.Unspecified) {
  val finalColor = if (color == Color.Unspecified) {
    if (isInDarkTheme()) MenuTextColorDark else Color.Black
  } else color
  DropdownMenuItem(onClick, contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Text(
        text,
        modifier = Modifier
          .fillMaxWidth()
          .weight(1F)
          .padding(end = 15.dp),
        color = finalColor
      )
      Icon(icon, text, tint = finalColor)
    }
  }
}

fun cancelFileAlertDialog(fileId: Long, cancelFile: (Long) -> Unit, cancelAction: CancelAction) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(cancelAction.alert.titleId),
    text = generalGetString(cancelAction.alert.messageId),
    confirmText = generalGetString(cancelAction.alert.confirmId),
    destructive = true,
    onConfirm = {
      cancelFile(fileId)
    }
  )
}

fun deleteMessageAlertDialog(chatItem: ChatItem, questionText: String, deleteMessage: (Long, CIDeleteMode) -> Unit) {
  AlertManager.shared.showAlertDialogButtons(
    title = generalGetString(MR.strings.delete_message__question),
    text = questionText,
    buttons = {
      Row(
        Modifier
          .fillMaxWidth()
          .padding(horizontal = 8.dp, vertical = 2.dp),
        horizontalArrangement = Arrangement.Center,
      ) {
        TextButton(onClick = {
          deleteMessage(chatItem.id, CIDeleteMode.cidmInternal)
          AlertManager.shared.hideAlert()
        }) { Text(stringResource(MR.strings.for_me_only), color = MaterialTheme.colors.error) }
        if (chatItem.meta.editable) {
          Spacer(Modifier.padding(horizontal = 4.dp))
          TextButton(onClick = {
            deleteMessage(chatItem.id, CIDeleteMode.cidmBroadcast)
            AlertManager.shared.hideAlert()
          }) { Text(stringResource(MR.strings.for_everybody), color = MaterialTheme.colors.error) }
        }
      }
    }
  )
}

fun moderateMessageAlertDialog(chatItem: ChatItem, questionText: String, deleteMessage: (Long, CIDeleteMode) -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_member_message__question),
    text = questionText,
    confirmText = generalGetString(MR.strings.delete_verb),
    destructive = true,
    onConfirm = {
      deleteMessage(chatItem.id, CIDeleteMode.cidmBroadcast)
    }
  )
}

private fun showMsgDeliveryErrorAlert(description: String) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.message_delivery_error_title),
    text = description,
  )
}

@Preview
@Composable
fun PreviewChatItemView() {
  SimpleXTheme {
    ChatItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      deleteMessage = { _, _ -> },
      receiveFile = { _, _ -> },
      cancelFile = {},
      joinGroup = {},
      acceptCall = { _ -> },
      scrollToItem = {},
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      updateContactStats = { },
      updateMemberStats = { _, _ -> },
      syncContactConnection = { },
      syncMemberConnection = { _, _ -> },
      findModelChat = { null },
      findModelMember = { null },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      developerTools = false,
    )
  }
}

@Preview
@Composable
fun PreviewChatItemViewDeletedContent() {
  SimpleXTheme {
    ChatItemView(
      ChatInfo.Direct.sampleData,
      ChatItem.getDeletedContentSampleData(),
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      deleteMessage = { _, _ -> },
      receiveFile = { _, _ -> },
      cancelFile = {},
      joinGroup = {},
      acceptCall = { _ -> },
      scrollToItem = {},
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      updateContactStats = { },
      updateMemberStats = { _, _ -> },
      syncContactConnection = { },
      syncMemberConnection = { _, _ -> },
      findModelChat = { null },
      findModelMember = { null },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      developerTools = false,
    )
  }
}
