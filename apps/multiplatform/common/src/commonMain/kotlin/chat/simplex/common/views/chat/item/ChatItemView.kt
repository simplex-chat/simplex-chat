package chat.simplex.common.views.chat.item

import SectionItemView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.currentUser
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chatlist.openChat
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import kotlinx.datetime.Clock
import kotlin.math.*

// TODO refactor so that FramedItemView can show all CIContent items if they're deleted (see Swift code)

private val msgRectMaxRadius = 18.dp
private val msgBubbleMaxRadius = msgRectMaxRadius * 1.2f
val msgTailWidthDp = 9.dp
private val msgTailMinHeightDp = msgTailWidthDp * 1.254f // ~56deg
private val msgTailMaxHeightDp = msgTailWidthDp * 1.732f // 60deg

val chatEventStyle = SpanStyle(fontSize = 12.sp, fontWeight = FontWeight.Light, color = CurrentColors.value.colors.secondary)

fun chatEventText(ci: ChatItem): AnnotatedString =
  chatEventText(ci.content.text, ci.timestampText)

fun chatEventText(eventText: String, ts: String): AnnotatedString =
  buildAnnotatedString {
    withStyle(chatEventStyle) { append("$eventText  $ts") }
  }

data class ChatItemReactionMenuItem (
  val name: String,
  val image: String?,
  val onClick: (() -> Unit)?
)

@Composable
fun ChatItemView(
  chatsCtx: ChatModel.ChatsContext,
  rhId: Long?,
  cInfo: ChatInfo,
  cItem: ChatItem,
  composeState: MutableState<ComposeState>,
  imageProvider: (() -> ImageGalleryProvider)? = null,
  useLinkPreviews: Boolean,
  linkMode: SimplexLinkMode,
  revealed: State<Boolean>,
  highlighted: State<Boolean>,
  hoveredItemId: MutableState<Long?>,
  range: State<IntRange?>,
  selectedChatItems: MutableState<Set<Long>?>,
  searchIsNotBlank: State<Boolean>,
  fillMaxWidth: Boolean = true,
  selectChatItem: () -> Unit,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  archiveReports: (List<Long>, Boolean) -> Unit,
  receiveFile: (Long) -> Unit,
  cancelFile: (Long) -> Unit,
  joinGroup: (Long, () -> Unit) -> Unit,
  acceptCall: (Contact) -> Unit,
  scrollToItem: (Long) -> Unit,
  scrollToQuotedItemFromItem: (Long) -> Unit,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit,
  openDirectChat: (Long) -> Unit,
  forwardItem: (ChatInfo, ChatItem) -> Unit,
  updateContactStats: (Contact) -> Unit,
  updateMemberStats: (GroupInfo, GroupMember) -> Unit,
  syncContactConnection: (Contact) -> Unit,
  syncMemberConnection: (GroupInfo, GroupMember) -> Unit,
  findModelChat: (String) -> Chat?,
  findModelMember: (String) -> GroupMember?,
  setReaction: (ChatInfo, ChatItem, Boolean, MsgReaction) -> Unit,
  showItemDetails: (ChatInfo, ChatItem) -> Unit,
  reveal: (Boolean) -> Unit,
  showMemberInfo: (GroupInfo, GroupMember) -> Unit,
  showChatInfo: () -> Unit,
  developerTools: Boolean,
  showViaProxy: Boolean,
  showTimestamp: Boolean,
  itemSeparation: ItemSeparation,
  preview: Boolean = false,
) {
  val uriHandler = LocalUriHandler.current
  val sent = cItem.chatDir.sent
  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart
  val showMenu = remember { mutableStateOf(false) }
  val fullDeleteAllowed = remember(cInfo) { cInfo.featureEnabled(ChatFeature.FullDelete) }
  val onLinkLongClick = { _: String -> showMenu.value = true }
  val live = remember { derivedStateOf { composeState.value.liveMessage != null } }.value

  Box(
    modifier = (if (fillMaxWidth) Modifier.fillMaxWidth() else Modifier),
    contentAlignment = alignment,
  ) {
    val info = cItem.meta.itemStatus.statusInto
    val onClick = if (info != null) {
      {
        AlertManager.shared.showAlertMsg(
          title = info.first,
          text = info.second,
        )
      }
    } else { {} }

    @Composable
    fun ChatItemReactions() {
      Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.chatItemOffset(cItem, itemSeparation.largeGap, inverted = true, revealed = true)) {
        cItem.reactions.forEach { r ->
          val showReactionMenu = remember { mutableStateOf(false) }
          val reactionMenuItems = remember { mutableStateOf(emptyList<ChatItemReactionMenuItem>()) }
          val interactionSource = remember { MutableInteractionSource() }
          val enterInteraction = remember { HoverInteraction.Enter() }
          KeyChangeEffect(highlighted.value) {
            if (highlighted.value) {
              interactionSource.emit(enterInteraction)
            } else {
              interactionSource.emit(HoverInteraction.Exit(enterInteraction))
            }
          }

          var modifier = Modifier.padding(horizontal = 5.dp, vertical = 2.dp).clip(RoundedCornerShape(8.dp))
          if (cInfo.featureEnabled(ChatFeature.Reactions)) {
            fun showReactionsMenu() {
              when (cInfo) {
                is ChatInfo.Group -> {
                  withBGApi {
                    try {
                      val members = controller.apiGetReactionMembers(rhId, cInfo.groupInfo.groupId, cItem.id, r.reaction)
                      if (members != null) {
                        showReactionMenu.value = true
                        reactionMenuItems.value = members.map {
                          val enabled = cInfo.groupInfo.membership.groupMemberId != it.groupMember.groupMemberId
                          val click = if (enabled) ({ showMemberInfo(cInfo.groupInfo, it.groupMember) }) else null
                          ChatItemReactionMenuItem(it.groupMember.displayName, it.groupMember.image, click)
                        }
                      }
                    } catch (e: Exception) {
                      Log.d(TAG, "chatItemView ChatItemReactions onLongClick: unexpected exception: ${e.stackTraceToString()}")
                    }
                  }
                }
                is ChatInfo.Direct -> {
                  showReactionMenu.value = true
                  val reactions = mutableListOf<ChatItemReactionMenuItem>()

                  if (!r.userReacted || r.totalReacted > 1) {
                    val contact = cInfo.contact
                    reactions.add(ChatItemReactionMenuItem(contact.displayName, contact.image, showChatInfo))
                  }

                  if (r.userReacted) {
                    reactions.add(ChatItemReactionMenuItem(generalGetString(MR.strings.sender_you_pronoun), currentUser.value?.image, null))
                  }
                  reactionMenuItems.value = reactions
                }
                else -> {}
              }
            }
            modifier = modifier
              .combinedClickable(
                onClick = {
                  if (cItem.allowAddReaction || r.userReacted) {
                    setReaction(cInfo, cItem, !r.userReacted, r.reaction)
                  }
                },
                onLongClick = {
                  showReactionsMenu()
                },
                interactionSource = interactionSource,
                indication = LocalIndication.current
              )
              .onRightClick { showReactionsMenu() }
          }
          Row(modifier.padding(2.dp), verticalAlignment = Alignment.CenterVertically) {
            ReactionIcon(r.reaction.text, fontSize = 12.sp)
            DefaultDropdownMenu(showMenu = showReactionMenu) {
              reactionMenuItems.value.forEach { m ->
                ItemAction(
                  text = m.name,
                  composable = { ProfileImage(44.dp, m.image) },
                  onClick = {
                    val click = m.onClick
                    if (click != null) {
                      click()
                      showReactionMenu.value = false
                    }
                  },
                  lineLimit = 1,
                  color = if (m.onClick == null) MaterialTheme.colors.secondary else MenuTextColor
                )
              }
            }
            if (r.totalReacted > 1) {
              Spacer(Modifier.width(4.dp))
              Text(
                "${r.totalReacted}",
                fontSize = 11.5.sp,
                fontWeight = if (r.userReacted) FontWeight.Bold else FontWeight.Normal,
                color = if (r.userReacted) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
              )
            }
          }
        }
      }
    }

    @Composable
    fun GoToItemInnerButton(alignStart: Boolean, icon: ImageResource, iconSize: Dp = 22.dp, parentActivated: State<Boolean>, onClick: () -> Unit) {
      val buttonInteractionSource = remember { MutableInteractionSource() }
      val buttonHovered = buttonInteractionSource.collectIsHoveredAsState()
      val buttonPressed = buttonInteractionSource.collectIsPressedAsState()
      val buttonActivated = remember { derivedStateOf { buttonHovered.value || buttonPressed.value } }

      val fullyVisible = parentActivated.value || buttonActivated.value || hoveredItemId.value == cItem.id
      val mixAlpha = 0.6f
      val mixedBackgroundColor = if (fullyVisible) {
        if (MaterialTheme.colors.isLight) {
          MaterialTheme.colors.secondary.mixWith(Color.White, mixAlpha)
        } else {
          MaterialTheme.colors.secondary.mixWith(Color.Black, mixAlpha)
        }
      } else {
        Color.Unspecified
      }
      val iconTint = if (fullyVisible) {
        Color.White
      } else {
        if (MaterialTheme.colors.isLight) {
          MaterialTheme.colors.secondary.mixWith(Color.White, mixAlpha)
        } else {
          MaterialTheme.colors.secondary.mixWith(Color.Black, mixAlpha)
        }
      }

      IconButton(
        onClick,
        Modifier
          .padding(start = if (alignStart) 0.dp else DEFAULT_PADDING_HALF + 3.dp, end = if (alignStart) DEFAULT_PADDING_HALF + 3.dp else 0.dp)
          .then(if (fullyVisible) Modifier.background(mixedBackgroundColor, CircleShape) else Modifier)
          .size(22.dp),
        interactionSource = buttonInteractionSource
      ) {
        Icon(painterResource(icon), null, Modifier.size(iconSize), tint = iconTint)
      }
    }

    @Composable
    fun GoToItemButton(alignStart: Boolean, parentActivated: State<Boolean>) {
      val chatTypeApiIdMsgId = cItem.meta.itemForwarded?.chatTypeApiIdMsgId
      if (searchIsNotBlank.value) {
        GoToItemInnerButton(alignStart, MR.images.ic_search, 17.dp, parentActivated) {
          withBGApi {
            openChat(secondaryChatsCtx = null, rhId, cInfo.chatType, cInfo.apiId, cItem.id)
            closeReportsIfNeeded()
          }
        }
      } else if (chatTypeApiIdMsgId != null) {
        GoToItemInnerButton(alignStart, MR.images.ic_arrow_forward, 22.dp, parentActivated) {
          val (chatType, apiId, msgId) = chatTypeApiIdMsgId
          withBGApi {
            openChat(secondaryChatsCtx = null, rhId, chatType, apiId, msgId)
            closeReportsIfNeeded()
          }
        }
      }
    }

    Column(horizontalAlignment = if (cItem.chatDir.sent) Alignment.End else Alignment.Start) {
      Row(verticalAlignment = Alignment.CenterVertically) {
        val bubbleInteractionSource = remember { MutableInteractionSource() }
        val bubbleHovered = bubbleInteractionSource.collectIsHoveredAsState()
        if (cItem.chatDir.sent) {
          GoToItemButton(true, bubbleHovered)
        }
        Column(Modifier.weight(1f, fill = false)) {
          val enterInteraction = remember { HoverInteraction.Enter() }
          LaunchedEffect(highlighted.value, hoveredItemId.value) {
            if (highlighted.value || hoveredItemId.value == cItem.id) {
              bubbleInteractionSource.emit(enterInteraction)
            } else {
              bubbleInteractionSource.emit(HoverInteraction.Exit(enterInteraction))
            }
          }
          Column(
            Modifier
              .clipChatItem(cItem, itemSeparation.largeGap, revealed.value)
              .hoverable(bubbleInteractionSource)
              .combinedClickable(
                onLongClick = { showMenu.value = true },
                onClick = {
                  if (appPlatform.isAndroid && (searchIsNotBlank.value || cItem.meta.itemForwarded?.chatTypeApiIdMsgId != null)) {
                    hoveredItemId.value = if (hoveredItemId.value == cItem.id) null else cItem.id
                  }
                  onClick()
                }, interactionSource = bubbleInteractionSource, indication = LocalIndication.current)
              .onRightClick { showMenu.value = true },
          ) {
            @Composable
            fun framedItemView() {
              FramedItemView(cInfo, cItem, uriHandler, imageProvider, linkMode = linkMode, showViaProxy = showViaProxy, showMenu, showTimestamp = showTimestamp, tailVisible = itemSeparation.largeGap, receiveFile, onLinkLongClick, scrollToItem, scrollToQuotedItemFromItem)
            }

            fun deleteMessageQuestionText(): String {
              return if (!sent || fullDeleteAllowed || cInfo is ChatInfo.Local) {
                generalGetString(MR.strings.delete_message_cannot_be_undone_warning)
              } else {
                generalGetString(MR.strings.delete_message_mark_deleted_warning)
              }
            }

            @Composable
            fun MsgReactionsMenu() {
              val rs = MsgReaction.supported.mapNotNull { r ->
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
                      Modifier.size(36.dp).clip(CircleShape).clickable {
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
            fun DeleteItemMenu() {
              DefaultDropdownMenu(showMenu) {
                DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                if (cItem.canBeDeletedForSelf) {
                  Divider()
                  SelectItemAction(showMenu, selectChatItem)
                }
              }
            }

            @Composable
            fun MsgContentItemDropdownMenu() {
              val saveFileLauncher = rememberSaveFileLauncher(ciFile = cItem.file)
              when {
                // cItem.id check is a special case for live message chat item which has negative ID while not sent yet
                cItem.isReport && cItem.meta.itemDeleted == null && cInfo is ChatInfo.Group -> {
                  DefaultDropdownMenu(showMenu) {
                    if (cItem.chatDir !is CIDirection.GroupSnd && cInfo.groupInfo.membership.memberRole >= GroupMemberRole.Moderator) {
                      ArchiveReportItemAction(cItem.id, cInfo.groupInfo.membership.memberActive, showMenu, archiveReports)
                    }
                    DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages, buttonText = stringResource(MR.strings.delete_report))
                    Divider()
                    SelectItemAction(showMenu, selectChatItem)
                  }
                }
                cItem.content.msgContent != null && cItem.id >= 0 && !cItem.isReport -> {
                  DefaultDropdownMenu(showMenu) {
                    if (cInfo.featureEnabled(ChatFeature.Reactions) && cItem.allowAddReaction) {
                      MsgReactionsMenu()
                    }
                    if (cItem.meta.itemDeleted == null && !live && !cItem.localNote) {
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
                      cItem.file?.forwardingAllowed() == true -> true
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
                          withLongRunningApi(slow = 600_000) {
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
                    if (cItem.file != null && (getLoadedFilePath(cItem.file) != null || (chatModel.connectedToRemote() && cachedRemoteReqs[cItem.file.fileSource] != false && cItem.file.loaded))) {
                      SaveContentItemAction(cItem, saveFileLauncher, showMenu)
                    } else if (cItem.file != null && cItem.file.fileStatus is CIFileStatus.RcvInvitation && fileSizeValid(cItem.file)) {
                      ItemAction(stringResource(MR.strings.download_file), painterResource(MR.images.ic_arrow_downward), onClick = {
                        withBGApi {
                          Log.d(TAG, "ChatItemView downloadFileAction")
                          val user = chatModel.currentUser.value
                          if (user != null) {
                            controller.receiveFile(rhId, user, cItem.file.fileId)
                          }
                        }
                        showMenu.value = false
                      })
                    }
                    if (cItem.meta.editable && cItem.content.msgContent !is MsgContent.MCVoice && !live) {
                      ItemAction(stringResource(MR.strings.edit_verb), painterResource(MR.images.ic_edit_filled), onClick = {
                        composeState.value = ComposeState(editingItem = cItem, useLinkPreviews = useLinkPreviews)
                        showMenu.value = false
                      })
                    }
                    if (cItem.meta.itemDeleted == null &&
                      (cItem.file == null || cItem.file.forwardingAllowed()) &&
                      !cItem.isLiveDummy && !live
                    ) {
                      ItemAction(stringResource(MR.strings.forward_chat_item), painterResource(MR.images.ic_forward), onClick = {
                        forwardItem(cInfo, cItem)
                        showMenu.value = false
                      })
                    }
                    ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                    if (revealed.value) {
                      HideItemAction(revealed, showMenu, reveal)
                    }
                    if (cItem.meta.itemDeleted == null && cItem.file != null && cItem.file.cancelAction != null && !cItem.localNote) {
                      CancelFileItemAction(cItem.file.fileId, showMenu, cancelFile = cancelFile, cancelAction = cItem.file.cancelAction)
                    }
                    if (!(live && cItem.meta.isLive) && !preview) {
                      DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                    }
                    if (cItem.chatDir !is CIDirection.GroupSnd) {
                      val groupInfo = cItem.memberToModerate(cInfo)?.first
                      if (groupInfo != null) {
                        ModerateItemAction(cItem, questionText = moderateMessageQuestionText(cInfo.featureEnabled(ChatFeature.FullDelete), 1), showMenu, deleteMessage)
                      } else if (cItem.meta.itemDeleted == null && cInfo is ChatInfo.Group && cInfo.groupInfo.groupFeatureEnabled(GroupFeature.Reports) && cInfo.groupInfo.membership.memberRole == GroupMemberRole.Member && !live) {
                        ReportItemAction(cItem, composeState, showMenu)
                      }
                    }
                    if (cItem.canBeDeletedForSelf) {
                      Divider()
                      SelectItemAction(showMenu, selectChatItem)
                    }
                  }
                }
                cItem.meta.itemDeleted != null -> {
                  DefaultDropdownMenu(showMenu) {
                    if (revealed.value) {
                      HideItemAction(revealed, showMenu, reveal)
                    } else if (!cItem.isDeletedContent) {
                      RevealItemAction(revealed, showMenu, reveal)
                    } else if (range.value != null) {
                      ExpandItemAction(revealed, showMenu, reveal)
                    }
                    ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                    DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                    if (cItem.canBeDeletedForSelf) {
                      Divider()
                      SelectItemAction(showMenu, selectChatItem)
                    }
                  }
                }
                cItem.isDeletedContent -> {
                  DefaultDropdownMenu(showMenu) {
                    ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                    DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                    if (cItem.canBeDeletedForSelf) {
                      Divider()
                      SelectItemAction(showMenu, selectChatItem)
                    }
                  }
                }
                cItem.mergeCategory != null && ((range.value?.count() ?: 0) > 1 || revealed.value) -> {
                  DefaultDropdownMenu(showMenu) {
                    if (revealed.value) {
                      ShrinkItemAction(revealed, showMenu, reveal)
                    } else {
                      ExpandItemAction(revealed, showMenu, reveal)
                    }
                    DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                    if (cItem.canBeDeletedForSelf) {
                      Divider()
                      SelectItemAction(showMenu, selectChatItem)
                    }
                  }
                }
                else -> {
                  DefaultDropdownMenu(showMenu) {
                    DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                    if (selectedChatItems.value == null) {
                      Divider()
                      SelectItemAction(showMenu, selectChatItem)
                    }
                  }
                }
              }
            }

            @Composable
            fun MarkedDeletedItemDropdownMenu() {
              DefaultDropdownMenu(showMenu) {
                if (!cItem.isDeletedContent) {
                  RevealItemAction(revealed, showMenu, reveal)
                }
                ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                if (cItem.canBeDeletedForSelf) {
                  Divider()
                  SelectItemAction(showMenu, selectChatItem)
                }
              }
            }

            @Composable
            fun ContentItem() {
              val mc = cItem.content.msgContent
              if (cItem.meta.itemDeleted != null && (!revealed.value || cItem.isDeletedContent)) {
                MarkedDeletedItemView(chatsCtx, cItem, cInfo, cInfo.timedMessagesTTL, revealed, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                MarkedDeletedItemDropdownMenu()
              } else {
                if (cItem.quotedItem == null && cItem.meta.itemForwarded == null && cItem.meta.itemDeleted == null && !cItem.meta.isLive) {
                  if (mc is MsgContent.MCText && isShortEmoji(cItem.content.text)) {
                    EmojiItemView(cItem, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
                  } else if (mc is MsgContent.MCVoice && cItem.content.text.isEmpty()) {
                    CIVoiceView(mc.duration, cItem.file, cItem.meta.itemEdited, cItem.chatDir.sent, hasText = false, cItem, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp, longClick = { onLinkLongClick("") }, receiveFile = receiveFile)
                  } else {
                    framedItemView()
                  }
                } else {
                  framedItemView()
                }
                MsgContentItemDropdownMenu()
              }
            }

            @Composable fun LegacyDeletedItem() {
              DeletedItemView(cItem, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
              DefaultDropdownMenu(showMenu) {
                ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = deleteMessageQuestionText(), deleteMessage, deleteMessages)
                if (cItem.canBeDeletedForSelf) {
                  Divider()
                  SelectItemAction(showMenu, selectChatItem)
                }
              }
            }

            @Composable fun CallItem(status: CICallStatus, duration: Int) {
              CICallItemView(cInfo, cItem, status, duration, showTimestamp = showTimestamp, acceptCall, cInfo.timedMessagesTTL)
              DeleteItemMenu()
            }

            fun mergedGroupEventText(chatItem: ChatItem, reversedChatItems: List<ChatItem>): String? {
              val (count, ns) = chatModel.getConnectedMemberNames(chatItem, reversedChatItems)
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

            fun eventItemViewText(reversedChatItems: List<ChatItem>): AnnotatedString {
              val memberDisplayName = cItem.memberDisplayName
              val t = mergedGroupEventText(cItem, reversedChatItems)
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
              val reversedChatItems = chatsCtx.chatItems.value.asReversed()
              CIEventView(eventItemViewText(reversedChatItems))
            }

            @Composable
            fun DeletedItem() {
              MarkedDeletedItemView(chatsCtx, cItem, cInfo, cInfo.timedMessagesTTL, revealed, showViaProxy = showViaProxy, showTimestamp = showTimestamp)
              DefaultDropdownMenu(showMenu) {
                if (revealed.value) {
                  HideItemAction(revealed, showMenu, reveal)
                } else if (!cItem.isDeletedContent) {
                  RevealItemAction(revealed, showMenu, reveal)
                } else if (range.value != null) {
                  ExpandItemAction(revealed, showMenu, reveal)
                }
                ItemInfoAction(cInfo, cItem, showItemDetails, showMenu)
                DeleteItemAction(chatsCtx, cItem, revealed, showMenu, questionText = generalGetString(MR.strings.delete_message_cannot_be_undone_warning), deleteMessage, deleteMessages)
                if (cItem.canBeDeletedForSelf) {
                  Divider()
                  SelectItemAction(showMenu, selectChatItem)
                }
              }
            }

            @Composable
            fun E2EEInfoNoPQText() {
              Text(
                buildAnnotatedString {
                  withStyle(chatEventStyle) { append(annotatedStringResource(MR.strings.e2ee_info_no_pq)) }
                },
                Modifier.padding(horizontal = 6.dp, vertical = 6.dp)
              )
            }

            @Composable
            fun DirectE2EEInfoText(e2EEInfo: E2EEInfo) {
              if (e2EEInfo.pqEnabled) {
                Text(
                  buildAnnotatedString {
                    withStyle(chatEventStyle) { append(annotatedStringResource(MR.strings.e2ee_info_pq)) }
                  },
                  Modifier.padding(horizontal = 6.dp, vertical = 6.dp)
                )
              } else {
                E2EEInfoNoPQText()
              }
            }

            when (val c = cItem.content) {
              is CIContent.SndMsgContent -> ContentItem()
              is CIContent.RcvMsgContent -> ContentItem()
              is CIContent.SndDeleted -> LegacyDeletedItem()
              is CIContent.RcvDeleted -> LegacyDeletedItem()
              is CIContent.SndCall -> CallItem(c.status, c.duration)
              is CIContent.RcvCall -> CallItem(c.status, c.duration)
              is CIContent.RcvIntegrityError -> if (developerTools) {
                IntegrityErrorItemView(c.msgError, cItem, showTimestamp, cInfo.timedMessagesTTL)
                DeleteItemMenu()
              } else {
                Box(Modifier.size(0.dp)) {}
              }
              is CIContent.RcvDecryptionError -> {
                CIRcvDecryptionError(c.msgDecryptError, c.msgCount, cInfo, cItem, updateContactStats = updateContactStats, updateMemberStats = updateMemberStats, syncContactConnection = syncContactConnection, syncMemberConnection = syncMemberConnection, findModelChat = findModelChat, findModelMember = findModelMember)
                DeleteItemMenu()
              }
              is CIContent.RcvGroupInvitation -> {
                CIGroupInvitationView(cItem, c.groupInvitation, c.memberRole, joinGroup = joinGroup, chatIncognito = cInfo.incognito, showTimestamp = showTimestamp, timedMessagesTTL = cInfo.timedMessagesTTL)
                DeleteItemMenu()
              }
              is CIContent.SndGroupInvitation -> {
                CIGroupInvitationView(cItem, c.groupInvitation, c.memberRole, joinGroup = joinGroup, chatIncognito = cInfo.incognito, showTimestamp = showTimestamp, timedMessagesTTL = cInfo.timedMessagesTTL)
                DeleteItemMenu()
              }
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
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.feature, c.enabled.iconColor, revealed = revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.SndChatFeature -> {
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.feature, c.enabled.iconColor, revealed = revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.RcvChatPreference -> {
                val ct = if (cInfo is ChatInfo.Direct) cInfo.contact else null
                CIFeaturePreferenceView(cItem, ct, c.feature, c.allowed, acceptFeature)
                DeleteItemMenu()
              }
              is CIContent.SndChatPreference -> {
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.feature, MaterialTheme.colors.secondary, icon = c.feature.icon, revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.RcvGroupFeature -> {
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.groupFeature, c.preference.enabled(c.memberRole_, (cInfo as? ChatInfo.Group)?.groupInfo?.membership).iconColor, revealed = revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.SndGroupFeature -> {
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.groupFeature, c.preference.enabled(c.memberRole_, (cInfo as? ChatInfo.Group)?.groupInfo?.membership).iconColor, revealed = revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.RcvChatFeatureRejected -> {
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.feature, Color.Red, revealed = revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.RcvGroupFeatureRejected -> {
                CIChatFeatureView(chatsCtx, cInfo, cItem, c.groupFeature, Color.Red, revealed = revealed, showMenu = showMenu)
                MsgContentItemDropdownMenu()
              }
              is CIContent.SndModerated -> DeletedItem()
              is CIContent.RcvModerated -> DeletedItem()
              is CIContent.RcvBlocked -> DeletedItem()
              is CIContent.SndDirectE2EEInfo -> DirectE2EEInfoText(c.e2eeInfo)
              is CIContent.RcvDirectE2EEInfo -> DirectE2EEInfoText(c.e2eeInfo)
              is CIContent.SndGroupE2EEInfo -> E2EEInfoNoPQText()
              is CIContent.RcvGroupE2EEInfo -> E2EEInfoNoPQText()
              is CIContent.InvalidJSON -> {
                CIInvalidJSONView(c.json)
                DeleteItemMenu()
              }
            }
          }
        }
        if (!cItem.chatDir.sent) {
          GoToItemButton(false, bubbleHovered)
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
  chatsCtx: ChatModel.ChatsContext,
  cItem: ChatItem,
  revealed: State<Boolean>,
  showMenu: MutableState<Boolean>,
  questionText: String,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  deleteMessages: (List<Long>) -> Unit,
  buttonText: String = stringResource(MR.strings.delete_verb),
) {
  ItemAction(
    buttonText,
    painterResource(MR.images.ic_delete),
    onClick = {
      showMenu.value = false
      if (!revealed.value) {
        val reversedChatItems = chatsCtx.chatItems.value.asReversed()
        val currIndex = chatModel.getChatItemIndexOrNull(cItem, reversedChatItems)
        val ciCategory = cItem.mergeCategory
        if (currIndex != null && ciCategory != null) {
          val (prevHidden, _) = chatModel.getPrevShownChatItem(currIndex, ciCategory, reversedChatItems)
          val range = chatViewItemsRange(currIndex, prevHidden)
          if (range != null) {
            val itemIds: ArrayList<Long> = arrayListOf()
            for (i in range) {
              itemIds.add(reversedChatItems[i].id)
            }
            deleteMessagesAlertDialog(
              itemIds,
              generalGetString(MR.strings.delete_messages_cannot_be_undone_warning),
              forAll = false,
              deleteMessages = { ids, _ -> deleteMessages(ids) }
            )
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
fun SelectItemAction(
  showMenu: MutableState<Boolean>,
  selectItem: () -> Unit,
) {
  ItemAction(
    stringResource(MR.strings.select_verb),
    painterResource(MR.images.ic_check_circle),
    onClick = {
      showMenu.value = false
      selectItem()
    }
  )
}

@Composable
private fun RevealItemAction(revealed: State<Boolean>, showMenu: MutableState<Boolean>, reveal: (Boolean) -> Unit) {
  ItemAction(
    stringResource(MR.strings.reveal_verb),
    painterResource(MR.images.ic_visibility),
    onClick = {
      reveal(true)
      showMenu.value = false
    }
  )
}

@Composable
private fun HideItemAction(revealed: State<Boolean>, showMenu: MutableState<Boolean>, reveal: (Boolean) -> Unit) {
  ItemAction(
    stringResource(MR.strings.hide_verb),
    painterResource(MR.images.ic_visibility_off),
    onClick = {
      reveal(false)
      showMenu.value = false
    }
  )
}

@Composable
private fun ExpandItemAction(revealed: State<Boolean>, showMenu: MutableState<Boolean>, reveal: (Boolean) -> Unit) {
  ItemAction(
    stringResource(MR.strings.expand_verb),
    painterResource(MR.images.ic_expand_all),
    onClick = {
      reveal(true)
      showMenu.value = false
    },
  )
}

@Composable
private fun ShrinkItemAction(revealed: State<Boolean>, showMenu: MutableState<Boolean>, reveal: (Boolean) -> Unit) {
  ItemAction(
    stringResource(MR.strings.hide_verb),
    painterResource(MR.images.ic_collapse_all),
    onClick = {
      reveal(false)
      showMenu.value = false
    },
  )
}

@Composable
private fun ReportItemAction(
  cItem: ChatItem,
  composeState: MutableState<ComposeState>,
  showMenu: MutableState<Boolean>,
) {
  ItemAction(
    stringResource(MR.strings.report_verb),
    painterResource(MR.images.ic_flag),
    onClick = {
      AlertManager.shared.showAlertDialogButtons(
        title = generalGetString(MR.strings.report_reason_alert_title),
        buttons = {
          ReportReason.supportedReasons.forEach { reason ->
            SectionItemView({
              if (composeState.value.editing) {
                composeState.value = ComposeState(
                  contextItem = ComposeContextItem.ReportedItem(cItem, reason),
                  useLinkPreviews = false,
                  preview = ComposePreview.NoPreview,
                )
              } else {
                composeState.value = composeState.value.copy(
                  contextItem = ComposeContextItem.ReportedItem(cItem, reason),
                  useLinkPreviews = false,
                  preview = ComposePreview.NoPreview,
                )
              }
              AlertManager.shared.hideAlert()
            }) {
              Text(reason.text, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
            }
          }
          SectionItemView({
            AlertManager.shared.hideAlert()
          }) {
            Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
        }
      )
      showMenu.value = false
    },
    color = Color.Red
  )
}

@Composable
private fun ArchiveReportItemAction(id: Long, allowForAll: Boolean, showMenu: MutableState<Boolean>, archiveReports: (List<Long>, Boolean) -> Unit) {
  ItemAction(
    stringResource(MR.strings.archive_report),
    painterResource(MR.images.ic_inventory_2),
    onClick = {
      showArchiveReportsAlert(listOf(id), allowForAll, archiveReports)
      showMenu.value = false
    }
  )
}

fun showArchiveReportsAlert(ids: List<Long>, allowForAll: Boolean, archiveReports: (List<Long>, Boolean) -> Unit) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = if (ids.size == 1) {
      generalGetString(MR.strings.report_archive_alert_title)
    } else {
      generalGetString(MR.strings.report_archive_alert_title_nth).format(ids.size)
    },
    text = null,
    buttons = {
      // Archive for me
      SectionItemView({
        AlertManager.shared.hideAlert()
        archiveReports(ids, false)
      }) {
        Text(
          generalGetString(MR.strings.report_archive_for_me),
          Modifier.fillMaxWidth(),
          textAlign = TextAlign.Center,
          color = MaterialTheme.colors.error
        )
      }
      if (allowForAll) {
        // Archive for all moderators
        SectionItemView({
          AlertManager.shared.hideAlert()
          archiveReports(ids, true)
        }) {
          Text(
            stringResource(MR.strings.report_archive_for_all_moderators),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.error
          )
        }
      }
    }
  )
}

@Composable
fun ItemAction(text: String, icon: Painter, color: Color = Color.Unspecified, onClick: () -> Unit) {
  val finalColor = if (color == Color.Unspecified) {
    MenuTextColor
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
fun ItemAction(text: String, icon: ImageBitmap, textColor: Color = Color.Unspecified, iconColor: Color = Color.Unspecified, onClick: () -> Unit) {
  val finalColor = if (textColor == Color.Unspecified) {
    MenuTextColor
  } else textColor
  DropdownMenuItem(onClick, contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Text(
        text,
        modifier = Modifier
          .fillMaxWidth()
          .weight(1F)
          .padding(end = 15.dp),
        color = finalColor,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
      if (iconColor == Color.Unspecified) {
        Image(icon, text, Modifier.size(22.dp))
      } else {
        Icon(icon, text, Modifier.size(22.dp), tint = iconColor)
      }
    }
  }
}

@Composable
fun ItemAction(
  text: String,
  composable: @Composable () -> Unit,
  color: Color = Color.Unspecified,
  onClick: () -> Unit,
  lineLimit: Int = Int.MAX_VALUE
) {
  val finalColor = if (color == Color.Unspecified) {
    MenuTextColor
  } else color
  DropdownMenuItem(onClick, contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Text(
        text,
        modifier = Modifier
          .fillMaxWidth()
          .weight(1F)
          .padding(end = 15.dp),
        color = finalColor,
        maxLines = lineLimit,
        overflow = TextOverflow.Ellipsis
      )
      composable()
    }
  }
}

@Composable
fun ItemAction(text: String, icon: ImageVector, onClick: () -> Unit, color: Color = Color.Unspecified) {
  val finalColor = if (color == Color.Unspecified) {
    MenuTextColor
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
fun ItemAction(text: String, color: Color = Color.Unspecified, onClick: () -> Unit) {
  val finalColor = if (color == Color.Unspecified) {
    MenuTextColor
  } else color
  DropdownMenuItem(onClick, contentPadding = PaddingValues(horizontal = DEFAULT_PADDING * 1.5f)) {
    Text(
      text,
      modifier = Modifier
        .fillMaxWidth()
        .weight(1F)
        .padding(end = 15.dp),
      color = finalColor
    )
  }
}

@Composable
fun Modifier.chatItemOffset(cItem: ChatItem, tailVisible: Boolean, inverted: Boolean = false, revealed: Boolean): Modifier {
  val chatItemTail = remember { appPreferences.chatItemTail.state }
  val style = shapeStyle(cItem, chatItemTail.value, tailVisible, revealed)

  val offset = if (style is ShapeStyle.Bubble) {
    if (style.tailVisible) {
      if (cItem.chatDir.sent) msgTailWidthDp else -msgTailWidthDp
    } else {
      0.dp
    }
  } else 0.dp

  return this.offset(x = if (inverted) (-1f * offset) else offset)
}

@Composable
fun Modifier.clipChatItem(chatItem: ChatItem? = null, tailVisible: Boolean = false, revealed: Boolean = false): Modifier {
  val chatItemRoundness = remember { appPreferences.chatItemRoundness.state }
  val chatItemTail = remember { appPreferences.chatItemTail.state }
  val style = shapeStyle(chatItem, chatItemTail.value, tailVisible, revealed)
  val cornerRoundness = chatItemRoundness.value.coerceIn(0f, 1f)

  val shape = when (style) {
    is ShapeStyle.Bubble -> chatItemShape(cornerRoundness, LocalDensity.current, style.tailVisible, chatItem?.chatDir?.sent == true)
    is ShapeStyle.RoundRect -> RoundedCornerShape(style.radius * cornerRoundness)
  }

  return this.clip(shape)
}

private fun chatItemShape(roundness: Float, density: Density, tailVisible: Boolean, sent: Boolean = false): GenericShape = GenericShape { size, _ ->
  val (msgTailWidth, msgBubbleMaxRadius) = with(density) { Pair(msgTailWidthDp.toPx(), msgBubbleMaxRadius.toPx()) }
  val width = size.width
  val height = size.height
  val rxMax = min(msgBubbleMaxRadius, width / 2)
  val ryMax = min(msgBubbleMaxRadius, height / 2)
  val rx = roundness * rxMax
  val ry = roundness * ryMax
  val tailHeight = with(density) {
    min(
      msgTailMinHeightDp.toPx() + roundness * (msgTailMaxHeightDp.toPx() - msgTailMinHeightDp.toPx()),
      height / 2
    )
  }
  moveTo(rx, 0f)
  lineTo(width - rx, 0f) // Top Line
  if (roundness > 0) {
    quadraticBezierTo(width, 0f, width, ry) // Top-right corner
  }
  if (height > 2 * ry) {
    lineTo(width, height - ry) // Right side
  }
  if (roundness > 0) {
    quadraticBezierTo(width, height, width - rx, height) // Bottom-right corner
  }
  if (tailVisible) {
    lineTo(0f, height) // Bottom line
    if (roundness > 0) {
      val d = tailHeight - msgTailWidth * msgTailWidth / tailHeight
      val controlPoint = Offset(msgTailWidth, height - tailHeight + d * sqrt(roundness))
      quadraticBezierTo(controlPoint.x, controlPoint.y, msgTailWidth, height - tailHeight)
    } else {
      lineTo(msgTailWidth, height - tailHeight)
    }

    if (height > ry + tailHeight) {
      lineTo(msgTailWidth, ry)
    }
  } else {
    lineTo(rx, height) // Bottom line
    if (roundness > 0) {
      quadraticBezierTo(0f, height, 0f, height - ry) // Bottom-left corner
    }
    if (height > 2 * ry) {
      lineTo(0f, ry) // Left side
    }
  }
  if (roundness > 0) {
    val bubbleInitialX = if (tailVisible) msgTailWidth else 0f
    quadraticBezierTo(bubbleInitialX, 0f, bubbleInitialX + rx, 0f) // Top-left corner
  }

  if (sent) {
    val matrix = Matrix()
    matrix.scale(-1f, 1f)
    this.transform(matrix)
    this.translate(Offset(size.width, 0f))
  }
}

sealed class ShapeStyle {
  data class Bubble(val tailVisible: Boolean, val startPadding: Boolean) : ShapeStyle()
  data class RoundRect(val radius: Dp) : ShapeStyle()
}

fun shapeStyle(chatItem: ChatItem? = null, tailEnabled: Boolean, tailVisible: Boolean, revealed: Boolean): ShapeStyle {
  if (chatItem == null) {
    return ShapeStyle.RoundRect(msgRectMaxRadius)
  }

  when (chatItem.content) {
    is CIContent.SndMsgContent,
    is CIContent.RcvMsgContent,
    is CIContent.RcvDecryptionError,
    is CIContent.SndDeleted,
    is CIContent.RcvDeleted,
    is CIContent.RcvIntegrityError,
    is CIContent.SndModerated,
    is CIContent.RcvModerated,
    is CIContent.RcvBlocked,
    is CIContent.InvalidJSON -> {
      if (chatItem.meta.itemDeleted != null && (!revealed || chatItem.isDeletedContent)) {
        return ShapeStyle.RoundRect(msgRectMaxRadius)
      }

      val tail = when (val content = chatItem.content.msgContent) {
        is MsgContent.MCImage,
        is MsgContent.MCVideo,
        is MsgContent.MCVoice -> {
          if (content.text.isEmpty()) {
            false
          } else {
            tailVisible
          }
        }
        is MsgContent.MCText -> {
          if (isShortEmoji(content.text)) {
            false
          } else {
            tailVisible
          }
        }
        else -> tailVisible
      }
      return if (tailEnabled) {
        ShapeStyle.Bubble(tail, !chatItem.chatDir.sent)
      } else {
        ShapeStyle.RoundRect(msgRectMaxRadius)
      }
    }

    is CIContent.RcvGroupInvitation,
    is CIContent.SndGroupInvitation -> return ShapeStyle.RoundRect(msgRectMaxRadius)
    else -> return ShapeStyle.RoundRect(8.dp)
  }
}

private fun closeReportsIfNeeded() {
  if (appPlatform.isAndroid && ModalManager.end.isLastModalOpen(ModalViewId.SECONDARY_CHAT)) {
    ModalManager.end.closeModals()
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
        if (chatItem.meta.deletable && !chatItem.localNote && !chatItem.isReport) {
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

fun deleteMessagesAlertDialog(itemIds: List<Long>, questionText: String, forAll: Boolean, deleteMessages: (List<Long>, Boolean) -> Unit) {
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
          deleteMessages(itemIds, false)
          AlertManager.shared.hideAlert()
        }) { Text(stringResource(MR.strings.for_me_only), color = MaterialTheme.colors.error) }

        if (forAll) {
          TextButton(onClick = {
            deleteMessages(itemIds, true)
            AlertManager.shared.hideAlert()
          }) { Text(stringResource(MR.strings.for_everybody), color = MaterialTheme.colors.error) }
        }
      }
    }
  )
}

fun moderateMessageQuestionText(fullDeleteAllowed: Boolean, count: Int): String {
  return if (fullDeleteAllowed) {
    generalGetString(if (count == 1) MR.strings.moderate_message_will_be_deleted_warning else MR.strings.moderate_messages_will_be_deleted_warning)
  } else {
    generalGetString(if (count == 1) MR.strings.moderate_message_will_be_marked_warning else MR.strings.moderate_messages_will_be_marked_warning)
  }
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

fun moderateMessagesAlertDialog(itemIds: List<Long>, questionText: String, deleteMessages: (List<Long>) -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = if (itemIds.size == 1) generalGetString(MR.strings.delete_member_message__question) else generalGetString(MR.strings.delete_members_messages__question).format(itemIds.size),
    text = questionText,
    confirmText = generalGetString(MR.strings.delete_verb),
    destructive = true,
    onConfirm = { deleteMessages(itemIds) }
  )
}

expect fun copyItemToClipboard(cItem: ChatItem, clipboard: ClipboardManager)

@Preview
@Composable
fun PreviewChatItemView(
  chatItem: ChatItem = ChatItem.getSampleData(1, CIDirection.DirectSnd(), Clock.System.now(), "hello")
) {
  ChatItemView(
    chatsCtx = ChatModel.ChatsContext(contentTag = null),
    rhId = null,
    ChatInfo.Direct.sampleData,
    chatItem,
    useLinkPreviews = true,
    linkMode = SimplexLinkMode.DESCRIPTION,
    composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
    revealed = remember { mutableStateOf(false) },
    highlighted = remember { mutableStateOf(false) },
    hoveredItemId = remember { mutableStateOf(null) },
    range = remember { mutableStateOf(0..1) },
    selectedChatItems = remember { mutableStateOf(setOf()) },
    searchIsNotBlank = remember { mutableStateOf(false) },
    selectChatItem = {},
    deleteMessage = { _, _ -> },
    deleteMessages = { _ -> },
    archiveReports = { _, _ -> },
    receiveFile = { _ -> },
    cancelFile = {},
    joinGroup = { _, _ -> },
    acceptCall = { _ -> },
    scrollToItem = {},
    scrollToQuotedItemFromItem = {},
    acceptFeature = { _, _, _ -> },
    openDirectChat = { _ -> },
    forwardItem = { _, _ -> },
    updateContactStats = { },
    updateMemberStats = { _, _ -> },
    syncContactConnection = { },
    syncMemberConnection = { _, _ -> },
    findModelChat = { null },
    findModelMember = { null },
    setReaction = { _, _, _, _ -> },
    showItemDetails = { _, _ -> },
    reveal = {},
    showMemberInfo = { _, _ ->},
    showChatInfo = {},
    developerTools = false,
    showViaProxy = false,
    showTimestamp = true,
    preview = true,
    itemSeparation = ItemSeparation(timestamp = true, largeGap = true, null)
  )
}

@Preview
@Composable
fun PreviewChatItemViewDeletedContent() {
  SimpleXTheme {
    ChatItemView(
      chatsCtx = ChatModel.ChatsContext(contentTag = null),
      rhId = null,
      ChatInfo.Direct.sampleData,
      ChatItem.getDeletedContentSampleData(),
      useLinkPreviews = true,
      linkMode = SimplexLinkMode.DESCRIPTION,
      composeState = remember { mutableStateOf(ComposeState(useLinkPreviews = true)) },
      revealed = remember { mutableStateOf(false) },
      highlighted = remember { mutableStateOf(false) },
      hoveredItemId = remember { mutableStateOf(null) },
      range = remember { mutableStateOf(0..1) },
      selectedChatItems = remember { mutableStateOf(setOf()) },
      searchIsNotBlank = remember { mutableStateOf(false) },
      selectChatItem = {},
      deleteMessage = { _, _ -> },
      deleteMessages = { _ -> },
      archiveReports = { _, _ -> },
      receiveFile = { _ -> },
      cancelFile = {},
      joinGroup = { _, _ -> },
      acceptCall = { _ -> },
      scrollToItem = {},
      scrollToQuotedItemFromItem = {},
      acceptFeature = { _, _, _ -> },
      openDirectChat = { _ -> },
      forwardItem = { _, _ -> },
      updateContactStats = { },
      updateMemberStats = { _, _ -> },
      syncContactConnection = { },
      syncMemberConnection = { _, _ -> },
      findModelChat = { null },
      findModelMember = { null },
      setReaction = { _, _, _, _ -> },
      showItemDetails = { _, _ -> },
      reveal = {},
      showMemberInfo = { _, _ ->},
      showChatInfo = {},
      developerTools = false,
      showViaProxy = false,
      preview = true,
      showTimestamp = true,
      itemSeparation = ItemSeparation(timestamp = true, largeGap = true, null)
    )
  }
}
