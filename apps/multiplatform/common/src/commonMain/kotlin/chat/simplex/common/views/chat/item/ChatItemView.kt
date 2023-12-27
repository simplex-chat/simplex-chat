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
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*
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
  revealed: MutableState<Boolean>,
  range: IntRange?,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  receiveFile: (Long, Boolean) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long, () -> Unit) -> Unit,
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
  developerTools: Boolean,
) {
  val uriHandler = LocalUriHandler.current
  val sent = cItem.chatDir.sent
  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart
  val showMenu = remember { mutableStateOf(false) }
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
          return if (!sent || fullDeleteAllowed) {
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
          when {
            cItem.content.msgContent != null -> {
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
                val cachedRemoteReqs = remember { CIFile.cachedRemoteFileRequests }
                val copyAndShareAllowed = when {
                  cItem.content.text.isNotEmpty() -> true
                  cItem.file != null && chatModel.connectedToRemote() && cachedRemoteReqs[cItem.file.fileSource] != false && cItem.file.loaded -> true
                  getLoadedFilePath(cItem.file) != null -> true
                  else -> false
                }

                if (copyAndShareAllowed) {
                  ItemAction(stringResource(MR.strings.share_verb), painterResource(MR.images.ic_share), onClick = {
                    var fileSource = getLoadedFileSource(cItem.file)
                    val shareIfExists = {
                      when (val f = fileSource) {
                        null -> clipboard.shareText(cItem.content.text)
                        else -> shareFile(cItem.text, f)
                      }
                      showMenu.value = false
                    }
                    if (chatModel.connectedToRemote() && fileSource == null) {
                      withBGApi {
                        cItem.file?.loadRemoteFile(true)
                        fileSource = getLoadedFileSource(cItem.file)
                        shareIfExists()
                      }
                    } else shareIfExists()
                  })
                }
                if (copyAndShareAllowed) {
                  ItemAction(stringResource(MR.strings.copy_verb), painterResource(MR.images.ic_content_copy), onClick = {
                    copyItemToClipboard(cItem, clipboard)
                    showMenu.value = false
                  })
                }
                if ((cItem.content.msgContent is MsgContent.MCImage || cItem.content.msgContent is MsgContent.MCVideo || cItem.content.msgContent is MsgContent.MCFile || cItem.content.msgContent is MsgContent.MCVoice) && (getLoadedFilePath(cItem.file) != null || (chatModel.connectedToRemote() && cachedRemoteReqs[cItem.file?.fileSource] != false && cItem.file?.loaded == true))) {
                  SaveContentItemAction(cItem, saveFileLauncher, showMenu)
                }
                if (cItem.meta.editable && cItem.content.msgContent !is MsgContent.MCVoice && !live) {
                  ItemAction(stringResource(MR.strings.edit_verb), painterResource(MR.images.ic_edit_filled), onClick = {
                    composeState.value = ComposeState(editingItem = cItem, useLinkPreviews = useLinkPreviews)
                    showMenu.value = false
                  })
                }
                ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                if (revealed.value) {
                  HideItemAction(revealed, showMenu)
                }
                if (cItem.meta.itemDeleted == null && cItem.file != null && cItem.file.cancelAction != null) {
                  CancelFileItemAction(cItem.file.fileId, showMenu, cancelFile = cancelFile, cancelAction = cItem.file.cancelAction)
                }
                if (!(live && cItem.meta.isLive)) {
                  DeleteItemAction(cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                }
                val groupInfo = cItem.memberToModerate(cInfo)?.first
                if (groupInfo != null) {
                  ModerateItemAction(cItem, questionText = moderateMessageQuestionText(), showMenu, deleteMessage)
                }
              }
            }
            cItem.meta.itemDeleted != null -> {
              DefaultDropdownMenu(showMenu) {
                if (revealed.value) {
                  HideItemAction(revealed, showMenu)
                } else if (!cItem.isDeletedContent) {
                  RevealItemAction(revealed, showMenu)
                } else if (range != null) {
                  ExpandItemAction(revealed, showMenu)
                }
                ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                DeleteItemAction(cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
              }
            }
            cItem.isDeletedContent -> {
              DefaultDropdownMenu(showMenu) {
                ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                DeleteItemAction(cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
              }
            }
            cItem.mergeCategory != null && ((range?.count() ?: 0) > 1 || revealed.value) -> {
              DefaultDropdownMenu(showMenu) {
                if (revealed.value) {
                  ShrinkItemAction(revealed, showMenu)
                } else {
                  ExpandItemAction(revealed, showMenu)
                }
              }
            }
            else -> {
              showMenu.value = false
            }
          }
        }

        @Composable
        fun MarkedDeletedItemDropdownMenu() {
          DefaultDropdownMenu(showMenu) {
            if (!cItem.isDeletedContent) {
              RevealItemAction(revealed, showMenu)
            }
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            DeleteItemAction(cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
          }
        }

        @Composable
        fun ContentItem() {
          val mc = cItem.content.msgContent
          if (cItem.meta.itemDeleted != null && (!revealed.value || cItem.isDeletedContent)) {
            MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL, revealed)
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
            DeleteItemAction(cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
          }
        }

        @Composable fun CallItem(status: CICallStatus, duration: Int) {
          CICallItemView(cInfo, cItem, status, duration, acceptCall)
        }

        fun mergedGroupEventText(chatItem: ChatItem): String? {
          val (count, ns) = chatModel.getConnectedMemberNames(chatItem)
          val members = when {
            ns.size == 1 -> String.format(generalGetString(MR.strings.rcv_group_event_1_member_connected), ns[0])
            ns.size == 2 -> String.format(generalGetString(MR.strings.rcv_group_event_2_members_connected), ns[0], ns[1])
            ns.size == 3 -> String.format(generalGetString(MR.strings.rcv_group_event_3_members_connected), ns[0], ns[1], ns[2])
            ns.size > 3 -> String.format(generalGetString(MR.strings.rcv_group_event_n_members_connected), ns[0], ns[1], ns.size - 2)
            else -> ""
          }
          return if (count <= 1) {
            null
          } else if (ns.isEmpty()) {
            generalGetString(MR.strings.rcv_group_events_count).format(count)
          } else if (count > ns.size) {
            members + " " + generalGetString(MR.strings.rcv_group_and_other_events).format(count - ns.size)
          } else {
            members
          }
        }

        fun eventItemViewText(): AnnotatedString {
          val memberDisplayName = cItem.memberDisplayName
          val t = mergedGroupEventText(cItem)
          return if (!revealed.value && t != null) {
            chatEventText(t, cItem.timestampText)
          } else if (memberDisplayName != null) {
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

        @Composable
        fun ModeratedItem() {
          MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL, revealed)
          DefaultDropdownMenu(showMenu) {
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            DeleteItemAction(cItem, revealed, showMenu, questionText = generalGetString(MR.strings.delete_message_cannot_be_undone_warning), deleteMessage, deleteMessages)
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
          is CIContent.RcvDirectEventContent -> {
            EventItemView()
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvGroupEventContent -> {
            when (c.rcvGroupEvent) {
              is RcvGroupEvent.MemberCreatedContact -> CIMemberCreatedContactView(cItem, openDirectChat)
              else -> EventItemView()
            }
            MsgContentItemDropdownMenu()
          }
          is CIContent.SndGroupEventContent -> {
            EventItemView()
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvConnEventContent -> {
            EventItemView()
            MsgContentItemDropdownMenu()
          }
          is CIContent.SndConnEventContent -> {
            EventItemView()
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvChatFeature -> {
            CIChatFeatureView(cItem, c.feature, c.enabled.iconColor, revealed = revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
          is CIContent.SndChatFeature -> {
            CIChatFeatureView(cItem, c.feature, c.enabled.iconColor, revealed = revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvChatPreference -> {
            val ct = if (cInfo is ChatInfo.Direct) cInfo.contact else null
            CIFeaturePreferenceView(cItem, ct, c.feature, c.allowed, acceptFeature)
          }
          is CIContent.SndChatPreference -> {
            CIChatFeatureView(cItem, c.feature, MaterialTheme.colors.secondary, icon = c.feature.icon, revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvGroupFeature -> {
            CIChatFeatureView(cItem, c.groupFeature, c.preference.enable.iconColor, revealed = revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
          is CIContent.SndGroupFeature -> {
            CIChatFeatureView(cItem, c.groupFeature, c.preference.enable.iconColor, revealed = revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvChatFeatureRejected -> {
            CIChatFeatureView(cItem, c.feature, Color.Red, revealed = revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
          is CIContent.RcvGroupFeatureRejected -> {
            CIChatFeatureView(cItem, c.groupFeature, Color.Red, revealed = revealed, showMenu = showMenu)
            MsgContentItemDropdownMenu()
          }
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
  revealed: MutableState<Boolean>,
  showMenu: MutableState<Boolean>,
  questionText: String,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
) {
  ItemAction(
    stringResource(MR.strings.delete_verb),
    painterResource(MR.images.ic_delete),
    onClick = {
      showMenu.value = false
      if (!revealed.value && cItem.meta.itemDeleted != null) {
        val currIndex = chatModel.getChatItemIndexOrNull(cItem)
        val ciCategory = cItem.mergeCategory
        if (currIndex != null && ciCategory != null) {
          val (prevHidden, _) = chatModel.getPrevShownChatItem(currIndex, ciCategory)
          val range = chatViewItemsRange(currIndex, prevHidden)
          if (range != null) {
            val itemIds: ArrayList<Long> = arrayListOf()
            for (i in range) {
              itemIds.add(chatModel.chatItems.asReversed()[i].id)
            }
            deleteMessagesAlertDialog(itemIds, generalGetString(MR.strings.delete_message_mark_deleted_warning), deleteMessages = deleteMessages)
          } else {
            deleteMessageAlertDialog(cItem, questionText, deleteMessage = deleteMessage)
          }
        } else {
          deleteMessageAlertDialog(cItem, questionText, deleteMessage = deleteMessage)
        }
      } else {
        deleteMessageAlertDialog(cItem, questionText, deleteMessage = deleteMessage)
      }
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
private fun RevealItemAction(revealed: MutableState<Boolean>, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.reveal_verb),
    painterResource(MR.images.ic_visibility),
    onClick = {
      revealed.value = true
      showMenu.value = false
    }
  )
}

@Composable
private fun HideItemAction(revealed: MutableState<Boolean>, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.hide_verb),
    painterResource(MR.images.ic_visibility_off),
    onClick = {
      revealed.value = false
      showMenu.value = false
    }
  )
}

@Composable
private fun ExpandItemAction(revealed: MutableState<Boolean>, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.expand_verb),
    painterResource(MR.images.ic_expand_all),
    onClick = {
      revealed.value = true
      showMenu.value = false
    },
  )
}

@Composable
private fun ShrinkItemAction(revealed: MutableState<Boolean>, showMenu: MutableState<Boolean>) {
  ItemAction(
    stringResource(MR.strings.hide_verb),
    painterResource(MR.images.ic_collapse_all),
    onClick = {
      revealed.value = false
      showMenu.value = false
    },
  )
}

@Composable
fun ItemAction(text: String, icon: Painter, color: Color = Color.Unspecified, onClick: () -> Unit) {
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

fun deleteMessagesAlertDialog(itemIds: List<Long>, questionText: String, deleteMessages: (List<Long>) -> Unit) {
  AlertManager.shared.showAlertDialogButtons(
    title = generalGetString(MR.strings.delete_messages__question).format(itemIds.size),
    text = questionText,
    buttons = {
      Row(
        Modifier
          .fillMaxWidth()
          .padding(horizontal = 8.dp, vertical = 2.dp),
        horizontalArrangement = Arrangement.Center,
      ) {
        TextButton(onClick = {
          deleteMessages(itemIds)
          AlertManager.shared.hideAlert()
        }) { Text(stringResource(MR.strings.for_me_only), color = MaterialTheme.colors.error) }
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

expect fun copyItemToClipboard(cItem: ChatItem, clipboard: ClipboardManager)

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
      revealed = remember { mutableStateOf(false) },
      range = 0..1,
      deleteMessage = { _, _ -> },
      deleteMessages = { _ -> },
      receiveFile = { _, _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
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
      revealed = remember { mutableStateOf(false) },
      range = 0..1,
      deleteMessage = { _, _ -> },
      deleteMessages = { _ -> },
      receiveFile = { _, _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
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
