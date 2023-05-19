package chat.simplex.app.views.chat.item

import android.Manifest
import android.os.Build
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.scrollable
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
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.IncognitoView
import com.google.accompanist.permissions.rememberPermissionState
import kotlinx.datetime.Clock

// TODO refactor so that FramedItemView can show all CIContent items if they're deleted (see Swift code)

@Composable
fun ChatItemView(
  cInfo: ChatInfo,
  cItem: ChatItem,
  composeState: MutableState<ComposeState>,
  imageProvider: (() -> ImageGalleryProvider)? = null,
  showMember: Boolean = false,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long) -> Unit,
  acceptCall: (Contact) -> Unit,
  scrollToItem: (Long) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
) {
  val context = LocalContext.current
  val uriHandler = LocalUriHandler.current
  val sent = cItem.chatDir.sent
  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart
  val showMenu = remember { mutableStateOf(false) }
  val revealed = remember { mutableStateOf(false) }
  val fullDeleteAllowed = remember(cInfo) { cInfo.featureEnabled(ChatFeature.FullDelete) }
  val saveFileLauncher = rememberSaveFileLauncher(cxt = context, ciFile = cItem.file)
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
          showMsgDeliveryErrorAlert(generalGetString(R.string.message_delivery_error_desc))
        }
        is CIStatus.SndError -> {
          showMsgDeliveryErrorAlert(generalGetString(R.string.unknown_error) + ": ${cItem.meta.itemStatus.agentError}")
        }
        else -> {}
      }
    }

    @Composable
    fun ChatItemReactions() {
      Row {
        cItem.reactions.forEach { r ->
          var modifier = Modifier.padding(horizontal = 5.dp, vertical = 2.dp).clip(RoundedCornerShape(8.dp))
          if (cInfo.featureEnabled(ChatFeature.Reactions) && (cItem.allowAddReaction || r.userReacted)) {
            modifier = modifier.clickable {
              setReaction(cInfo, cItem, !r.userReacted, r.reaction)
            }
          }
          Row(modifier.padding(2.dp)) {
            Text(r.reaction.text, fontSize = 12.sp)
            if (r.totalReacted > 1) {
              Spacer(Modifier.width(4.dp))
              Text("${r.totalReacted}",
                fontSize = 11.5.sp,
                fontWeight = if (r.userReacted) FontWeight.Bold else FontWeight.Normal,
                color = if (r.userReacted) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
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
          .combinedClickable(onLongClick = { showMenu.value = true }, onClick = onClick),
      ) {
        @Composable
        fun framedItemView() {
          FramedItemView(cInfo, cItem, uriHandler, imageProvider, showMember = showMember, linkMode = linkMode, showMenu, receiveFile, onLinkLongClick, scrollToItem)
        }

        fun deleteMessageQuestionText(): String {
          return if (fullDeleteAllowed) {
            generalGetString(R.string.delete_message_cannot_be_undone_warning)
          } else {
            generalGetString(R.string.delete_message_mark_deleted_warning)
          }
        }

        fun moderateMessageQuestionText(): String {
          return if (fullDeleteAllowed) {
            generalGetString(R.string.moderate_message_will_be_deleted_warning)
          } else {
            generalGetString(R.string.moderate_message_will_be_marked_warning)
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
            Row(modifier = Modifier.padding(horizontal = DEFAULT_PADDING).horizontalScroll(rememberScrollState())) {
              rs.forEach() { r ->
                Box(
                  Modifier.size(36.dp).clickable {
                    setReaction(cInfo, cItem, true, r)
                    showMenu.value = false
                  },
                  contentAlignment = Alignment.Center
                ) {
                  Text(r.text)
                }
              }
            }
          }
        }

        @Composable
        fun MsgContentItemDropdownMenu() {
          DefaultDropdownMenu(showMenu) {
            if (cInfo.featureEnabled(ChatFeature.Reactions) && cItem.allowAddReaction) {
              MsgReactionsMenu()
            }
            if (cItem.meta.itemDeleted == null && !live) {
              ItemAction(stringResource(R.string.reply_verb), painterResource(R.drawable.ic_reply), onClick = {
                if (composeState.value.editing) {
                  composeState.value = ComposeState(contextItem = ComposeContextItem.QuotedItem(cItem), useLinkPreviews = useLinkPreviews)
                } else {
                  composeState.value = composeState.value.copy(contextItem = ComposeContextItem.QuotedItem(cItem))
                }
                showMenu.value = false
              })
            }
            ItemAction(stringResource(R.string.share_verb), painterResource(R.drawable.ic_share), onClick = {
              val filePath = getLoadedFilePath(SimplexApp.context, cItem.file)
              when {
                filePath != null -> shareFile(context, cItem.text, filePath)
                else -> shareText(context, cItem.content.text)
              }
              showMenu.value = false
            })
            ItemAction(stringResource(R.string.copy_verb), painterResource(R.drawable.ic_content_copy), onClick = {
              copyText(context, cItem.content.text)
              showMenu.value = false
            })
            if (cItem.content.msgContent is MsgContent.MCImage || cItem.content.msgContent is MsgContent.MCVideo || cItem.content.msgContent is MsgContent.MCFile || cItem.content.msgContent is MsgContent.MCVoice) {
              val filePath = getLoadedFilePath(context, cItem.file)
              if (filePath != null) {
                val writePermissionState = rememberPermissionState(permission = Manifest.permission.WRITE_EXTERNAL_STORAGE)
                ItemAction(stringResource(R.string.save_verb), painterResource(R.drawable.ic_download), onClick = {
                  when (cItem.content.msgContent) {
                    is MsgContent.MCImage -> {
                      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R || writePermissionState.hasPermission) {
                        saveImage(context, cItem.file)
                      } else {
                        writePermissionState.launchPermissionRequest()
                      }
                    }
                    is MsgContent.MCFile, is MsgContent.MCVoice, is MsgContent.MCVideo -> saveFileLauncher.launch(cItem.file?.fileName)
                    else -> {}
                  }
                  showMenu.value = false
                })
              }
            }
            if (cItem.meta.editable && cItem.content.msgContent !is MsgContent.MCVoice && !live) {
              ItemAction(stringResource(R.string.edit_verb), painterResource(R.drawable.ic_edit_filled), onClick = {
                composeState.value = ComposeState(editingItem = cItem, useLinkPreviews = useLinkPreviews)
                showMenu.value = false
              })
            }
            if (cItem.meta.itemDeleted != null && revealed.value) {
              ItemAction(
                stringResource(R.string.hide_verb),
                painterResource(R.drawable.ic_visibility_off),
                onClick = {
                  revealed.value = false
                  showMenu.value = false
                }
              )
            }
            if (cItem.meta.itemDeleted == null && cItem.file != null && cItem.file.cancelAction != null) {
              CancelFileItemAction(cItem.file.fileId, showMenu, cancelFile = cancelFile, cancelAction = cItem.file.cancelAction)
            }
            ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
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
                stringResource(R.string.reveal_verb),
                painterResource(R.drawable.ic_visibility),
                onClick = {
                  revealed.value = true
                  showMenu.value = false
                }
              )
              ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
            }
            DeleteItemAction(cItem, showMenu, questionText = deleteMessageQuestionText(), deleteMessage)
          }
        }

        @Composable
        fun ContentItem() {
          val mc = cItem.content.msgContent
          if (cItem.meta.itemDeleted != null && !revealed.value) {
            MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL, showMember = showMember)
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
          DeletedItemView(cItem, cInfo.timedMessagesTTL, showMember = showMember)
          DefaultDropdownMenu(showMenu) {
            DeleteItemAction(cItem, showMenu, questionText = deleteMessageQuestionText(), deleteMessage)
          }
        }

        @Composable fun CallItem(status: CICallStatus, duration: Int) {
          CICallItemView(cInfo, cItem, status, duration, acceptCall)
        }

        when (val c = cItem.content) {
          is CIContent.SndMsgContent -> ContentItem()
          is CIContent.RcvMsgContent -> ContentItem()
          is CIContent.SndDeleted -> DeletedItem()
          is CIContent.RcvDeleted -> DeletedItem()
          is CIContent.SndCall -> CallItem(c.status, c.duration)
          is CIContent.RcvCall -> CallItem(c.status, c.duration)
          is CIContent.RcvIntegrityError -> IntegrityErrorItemView(c.msgError, cItem, cInfo.timedMessagesTTL, showMember = showMember)
          is CIContent.RcvDecryptionError -> CIRcvDecryptionError(c.msgDecryptError, c.msgCount, cItem, cInfo.timedMessagesTTL, showMember = showMember)
          is CIContent.RcvGroupInvitation -> CIGroupInvitationView(cItem, c.groupInvitation, c.memberRole, joinGroup = joinGroup, chatIncognito = cInfo.incognito)
          is CIContent.SndGroupInvitation -> CIGroupInvitationView(cItem, c.groupInvitation, c.memberRole, joinGroup = joinGroup, chatIncognito = cInfo.incognito)
          is CIContent.RcvGroupEventContent -> CIEventView(cItem)
          is CIContent.SndGroupEventContent -> CIEventView(cItem)
          is CIContent.RcvConnEventContent -> CIEventView(cItem)
          is CIContent.SndConnEventContent -> CIEventView(cItem)
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
          is CIContent.SndModerated -> MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL, showMember = showMember)
          is CIContent.RcvModerated -> MarkedDeletedItemView(cItem, cInfo.timedMessagesTTL, showMember = showMember)
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
fun CancelFileItemAction(
  fileId: Long,
  showMenu: MutableState<Boolean>,
  cancelFile: (Long) -> Unit,
  cancelAction: CancelAction
) {
  ItemAction(
    stringResource(cancelAction.uiActionId),
    painterResource(R.drawable.ic_close),
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
    stringResource(R.string.info_menu),
    painterResource(R.drawable.ic_info),
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
    stringResource(R.string.delete_verb),
    painterResource(R.drawable.ic_delete),
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
    stringResource(R.string.moderate_verb),
    painterResource(R.drawable.ic_flag),
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
    title = generalGetString(R.string.delete_message__question),
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
        }) { Text(stringResource(R.string.for_me_only), color = MaterialTheme.colors.error) }
        if (chatItem.meta.editable) {
          Spacer(Modifier.padding(horizontal = 4.dp))
          TextButton(onClick = {
            deleteMessage(chatItem.id, CIDeleteMode.cidmBroadcast)
            AlertManager.shared.hideAlert()
          }) { Text(stringResource(R.string.for_everybody), color = MaterialTheme.colors.error) }
        }
      }
    }
  )
}

fun moderateMessageAlertDialog(chatItem: ChatItem, questionText: String, deleteMessage: (Long, CIDeleteMode) -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.delete_member_message__question),
    text = questionText,
    confirmText = generalGetString(R.string.delete_verb),
    destructive = true,
    onConfirm = {
      deleteMessage(chatItem.id, CIDeleteMode.cidmBroadcast)
    }
  )
}

private fun showMsgDeliveryErrorAlert(description: String) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.message_delivery_error_title),
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
      receiveFile = {},
      cancelFile = {},
      joinGroup = {},
      acceptCall = { _ -> },
      scrollToItem = {},
      acceptFeature = { _, _, _ -> },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
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
      receiveFile = {},
      cancelFile = {},
      joinGroup = {},
      acceptCall = { _ -> },
      scrollToItem = {},
      acceptFeature = { _, _, _ -> },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
    )
  }
}
