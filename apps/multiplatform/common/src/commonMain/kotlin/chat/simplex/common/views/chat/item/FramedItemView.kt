package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.lerp
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.*

@Composable
fun FramedItemView(
  chatsCtx: ChatModel.ChatsContext,
  chat: Chat,
  ci: ChatItem,
  uriHandler: UriHandler = LocalUriHandler.current,
  imageProvider: (() -> ImageGalleryProvider)? = null,
  linkMode: SimplexLinkMode,
  showViaProxy: Boolean = false,
  showMenu: MutableState<Boolean>,
  showTimestamp: Boolean = true,
  tailVisible: Boolean = false,
  receiveFile: (Long) -> Unit,
  onLinkLongClick: (String) -> Unit = {},
  scrollToItem: (Long) -> Unit = {},
  scrollToItemId: MutableState<Long?> = remember { mutableStateOf(null) },
  scrollToQuotedItemFromItem: (Long) -> Unit = {},
) {
  val mc = ci.content.msgContent
  val cInfo = chat.chatInfo
  val terminal = LocalTerminalLayout.current
  val sent = ci.chatDir.sent

  // Увеличенный на 10% размер шрифта (16.sp * 1.1 = 17.6.sp) с учётом системного множителя
  val msgFontSize = 17.6.sp * fontSizeMultiplier

  // M3 Expressive цвет пузыря сообщения
  val isLight = MaterialTheme.colors.isLight
  val bubbleBgColor = if (terminal) {
    Color.Transparent
  } else if (sent) {
    lerp(MaterialTheme.appColors.sentMessage, MaterialTheme.colors.primary, if (isLight) 0.08f else 0.14f)
  } else {
    lerp(MaterialTheme.appColors.receivedMessage, MaterialTheme.colors.primary, if (isLight) 0.04f else 0.07f)
  }

  @Composable
  fun BubbleSurface(transparent: Boolean = false, content: @Composable () -> Unit) {
    if (terminal || transparent) {
      content()
    } else {
      Surface(
        color = bubbleBgColor,
        contentColor = MaterialTheme.colors.onSurface,
        elevation = 0.dp
      ) {
        content()
      }
    }
  }

  when (mc) {
    is MsgContent.MCFile -> {
      BubbleSurface {
        CIFileView(
          ci.file,
          ci.meta,
          cInfo.timedMessagesTTL,
          showViaProxy = showViaProxy,
          showTimestamp = showTimestamp,
          showMenu = showMenu,
          receiveFile = receiveFile
        )
      }
    }
    is MsgContent.MCImage -> {
      val hasCaption = mc.text.isNotEmpty()
      BubbleSurface(transparent = !hasCaption) {
        Column(modifier = Modifier.width(IntrinsicSize.Min)) {
          CIImageView(
            mc.image,
            ci.file,
            imageProvider ?: { providerForGallery(emptyList(), ci.id) {} },
            showMenu,
            smallView = false,
            receiveFile = receiveFile
          )
        }
        is MsgContent.MCFile, is MsgContent.MCVoice -> {
          Box(Modifier.fillMaxWidth().weight(1f)) {
            ciQuotedMsgView(qi)
          }
          Icon(
            if (qi.content is MsgContent.MCFile) painterResource(MR.images.ic_draft_filled) else painterResource(MR.images.ic_mic_filled),
            if (qi.content is MsgContent.MCFile) stringResource(MR.strings.icon_descr_file) else stringResource(MR.strings.voice_message),
            Modifier
              .padding(top = if (terminal) 0.dp else 6.dp, end = 4.dp)
              .size(22.dp),
            tint = if (isInDarkTheme()) FileDark else FileLight
          )
        }
        is MsgContent.MCChat -> {
          val prefix = buildAnnotatedString {
            append(qi.content.chatLink.displayName + if (qi.content.text != qi.content.chatLink.connLinkStr) " - " else "")
          }
          Box(Modifier.fillMaxWidth().weight(1f)) {
            ciQuotedMsgView(qi, stripLink = qi.content.chatLink.connLinkStr, prefix = prefix)
          }
          Icon(
            painterResource(qi.content.chatLink.smallIconRes),
            null,
            Modifier.padding(top = if (terminal) 0.dp else 6.dp, end = 4.dp).size(22.dp),
            tint = if (isInDarkTheme()) FileDark else FileLight
          )
        }
        else -> ciQuotedMsgView(qi)
      }
    }
  }

  @Composable
  fun ciFileView(ci: ChatItem, text: String) {
    CIFileView(ci.file, ci.meta, chatTTL, showViaProxy, showTimestamp, showMenu, false, cardTop, receiveFile)
    if (text != "" || ci.meta.isLive) {
      CIMarkdownText(chatsCtx, ci, chat, chatTTL, linkMode = linkMode, uriHandler, showViaProxy = showViaProxy,  showTimestamp = showTimestamp)
    }
  }

  val captionlessMedia = (ci.content.msgContent is MsgContent.MCImage || ci.content.msgContent is MsgContent.MCVideo) &&
      !ci.meta.isLive && ci.content.text.isEmpty()
  val transparentBackground = captionlessMedia && ci.quotedItem == null && ci.meta.itemForwarded == null

  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  val metaReserved = when (val mc = ci.content.msgContent) {
    is MsgContent.MCVoice -> mc.text != ""
    is MsgContent.MCChat -> mc.text != mc.chatLink.connLinkStr
    else -> !captionlessMedia
  }
  Box(Modifier
    .then(
      when {
        !terminal -> Modifier.clipChatItem(ci, tailVisible, revealed = true)
        else -> Modifier
      }
    )
    .background(
      when {
        transparentBackground || terminal -> Color.Transparent
        sent -> sentColor
        else -> receivedColor
      }
    )) {
    var metaColor = MaterialTheme.colors.secondary
    ItemWithMeta(
      meta = { CIMetaView(ci, chatTTL, metaColor, showViaProxy = showViaProxy, showTimestamp = showTimestamp) },
      overlayMeta = if (captionlessMedia) { { Box(Modifier.padding(end = CHAT_ITEM_TEXT_PADDING)) { CIMetaView(ci, chatTTL, Color.White, showViaProxy = showViaProxy, showTimestamp = showTimestamp, lineHeightColor = messageTextColor()) } } } else null,
      metaReserved = metaReserved,
      gapBelowCard = terminalReactionsGap(ci).toDp(),
      endButton = endButton
    ) { inlineMeta ->
      Box(contentAlignment = Alignment.BottomEnd) {
        val tailRendered = chatItemTailRendered(ci, tailVisible, revealed = true)
        Column(
          Modifier
            .then(if (terminal && captionlessMedia) Modifier else Modifier.width(IntrinsicSize.Max))
            .padding(start = if (!sent && tailRendered) msgTailWidthDp else 0.dp, end = if (sent && tailRendered) msgTailWidthDp else 0.dp)
        ) {
          PriorityLayout(Modifier, CHAT_IMAGE_LAYOUT_ID, limitToPriorityWidth = !terminal || captionlessMedia) {
            @Composable
            fun Header() {
              if (ci.isReport) {
                if (ci.meta.itemDeleted == null) {
                  FramedItemHeader(
                    stringResource(if (ci.chatDir.sent) MR.strings.report_item_visibility_submitter else MR.strings.report_item_visibility_moderators),
                    true,
                    painterResource(MR.images.ic_flag),
                    iconColor = Color.Red

                  )
                }
                if (inlineMeta != null) {
                  Spacer(Modifier.height(4.dp))
                  Box(Modifier.align(Alignment.End)) {
                    inlineMeta()
                  }
                }
              }
            }
          }
        }
      }
    }
    is MsgContent.MCVideo -> {
      val hasCaption = mc.text.isNotEmpty()
      BubbleSurface(transparent = !hasCaption) {
        Column(modifier = Modifier.width(IntrinsicSize.Min)) {
          CIVideoView(
            mc.image,
            mc.duration,
            ci.file,
            imageProvider ?: { providerForGallery(emptyList(), ci.id) {} },
            showMenu,
            smallView = false,
            receiveFile = receiveFile
          )
          if (hasCaption) {
            ItemWithMeta(meta = { CIMetaView(ci, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp) }) { inlineMeta ->
              Column(
                modifier = Modifier
                  .fillMaxWidth()
                  .padding(horizontal = 14.dp, vertical = 10.dp),
                horizontalAlignment = Alignment.Start
              ) {
                SelectionContainer {
                  Text(
                    text = mc.text,
                    fontSize = msgFontSize,
                    textAlign = TextAlign.Start,
                    style = chatTextStyle(msgFontSize, MaterialTheme.colors.onSurface),
                    color = MaterialTheme.colors.onSurface
                  )
                }
                if (inlineMeta != null) {
                  Spacer(Modifier.height(4.dp))
                  Box(Modifier.align(Alignment.End)) {
                    inlineMeta()
                  }
                }
              }
            }
          }
        }
      }
    }
    is MsgContent.MCVoice -> {
      BubbleSurface {
        ItemWithMeta(meta = { CIMetaView(ci, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp) }) { inlineMeta ->
          Column(
            modifier = Modifier.padding(horizontal = 14.dp, vertical = 10.dp),
            horizontalAlignment = Alignment.Start
          ) {
            CIVoiceView(
              mc.duration,
              ci.file,
              ci.meta.itemEdited,
              ci.chatDir.sent,
              hasText = mc.text.isNotEmpty(),
              ci,
              cInfo.timedMessagesTTL,
              showViaProxy = showViaProxy,
              showTimestamp = showTimestamp,
              longClick = { onLinkLongClick("") },
              receiveFile = receiveFile
            )
            if (mc.text.isNotEmpty()) {
              Spacer(Modifier.height(6.dp))
              SelectionContainer {
                Text(
                  text = mc.text,
                  fontSize = msgFontSize,
                  textAlign = TextAlign.Start,
                  style = chatTextStyle(msgFontSize, MaterialTheme.colors.onSurface),
                  color = MaterialTheme.colors.onSurface
                )
              }
            }
            if (inlineMeta != null) {
              Spacer(Modifier.height(4.dp))
              Box(Modifier.align(Alignment.End)) {
                inlineMeta()
              }
            }
          }
        }
      }
    }
    is MsgContent.MCLink -> {
      BubbleSurface {
        ItemWithMeta(meta = { CIMetaView(ci, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp) }) { inlineMeta ->
          Column(
            modifier = Modifier.padding(bottom = 10.dp),
            horizontalAlignment = Alignment.Start
          ) {
            ChatItemLinkView(mc.preview, showMenu, onLongClick = { onLinkLongClick("") })
            if (mc.text.isNotEmpty()) {
              Spacer(Modifier.height(8.dp))
              Box(Modifier.padding(horizontal = 14.dp)) {
                SelectionContainer {
                  Text(
                    text = mc.text,
                    fontSize = msgFontSize,
                    textAlign = TextAlign.Start,
                    style = chatTextStyle(msgFontSize, MaterialTheme.colors.onSurface),
                    color = MaterialTheme.colors.onSurface
                  )
                }
              }
            }
            if (inlineMeta != null) {
              Spacer(Modifier.height(4.dp))
              Box(
                modifier = Modifier
                  .align(Alignment.End)
                  .padding(horizontal = 14.dp)
              ) {
                inlineMeta()
              }
            }
          }
        }
      }
    }
    is MsgContent.MCText -> {
      if (mc.text.isNotEmpty()) {
        if (terminal) {
          Column(Modifier.padding(horizontal = 8.dp)) {
            SelectionContainer {
              Text(
                text = mc.text,
                fontSize = msgFontSize,
                textAlign = TextAlign.Start,
                style = chatTextStyle(msgFontSize, MaterialTheme.colors.onSurface),
                color = MaterialTheme.colors.onSurface
              )
            }
            if (ci.file != null && ci.file.fileStatus !is CIFileStatus.Invalid) {
              Spacer(Modifier.height(8.dp))
              CIFileView(ci.file, ci.meta, cInfo.timedMessagesTTL, showViaProxy, showTimestamp, showMenu, smallView = true, receiveFile = receiveFile)
            }
          }
        } else {
          val hasFile = ci.file != null && ci.file.fileStatus !is CIFileStatus.Invalid

          BubbleSurface {
            ItemWithMeta(meta = { CIMetaView(ci, cInfo.timedMessagesTTL, showViaProxy = showViaProxy, showTimestamp = showTimestamp) }) { inlineMeta ->
              Column(
                modifier = Modifier.padding(horizontal = 14.dp, vertical = 10.dp),
                horizontalAlignment = Alignment.Start
              ) {
                if (hasFile) {
                  CIFileView(ci.file, ci.meta, cInfo.timedMessagesTTL, showViaProxy, showTimestamp, showMenu, smallView = true, receiveFile = receiveFile)
                  Spacer(Modifier.height(8.dp))
                }
                SelectionContainer {
                  Text(
                    text = mc.text,
                    fontSize = msgFontSize,
                    textAlign = TextAlign.Start,
                    style = chatTextStyle(msgFontSize, MaterialTheme.colors.onSurface),
                    color = MaterialTheme.colors.onSurface
                  )
                }
                if (inlineMeta != null) {
                  Spacer(Modifier.height(4.dp))
                  Box(Modifier.align(Alignment.End)) {
                    inlineMeta()
                  }
                }
              }
            }
          }
        }
      } else if (ci.file != null && ci.file.fileStatus !is CIFileStatus.Invalid) {
        BubbleSurface {
          CIFileView(ci.file, ci.meta, cInfo.timedMessagesTTL, showViaProxy, showTimestamp, showMenu, smallView = true, receiveFile = receiveFile)
        }
      }
    }
    is MsgContent.MCReport -> {
      val padding = if (terminal) Modifier.padding(horizontal = 8.dp) else Modifier.padding(horizontal = 14.dp, vertical = 10.dp)
      BubbleSurface {
        Column(
          modifier = padding,
          horizontalAlignment = Alignment.Start
        ) {
          Text(
            text = "Report: ${mc.reason.text}",
            fontSize = msgFontSize,
            textAlign = TextAlign.Start,
            style = chatTextStyle(msgFontSize, MaterialTheme.colors.error),
            color = MaterialTheme.colors.error
          )
          Spacer(Modifier.height(4.dp))
          SelectionContainer {
            Text(
              text = mc.text,
              fontSize = msgFontSize,
              textAlign = TextAlign.Start,
              style = chatTextStyle(msgFontSize, MaterialTheme.colors.onSurface),
              color = MaterialTheme.colors.onSurface
            )
          }
        }
      }
    }
    else -> {
      val padding = if (terminal) Modifier.padding(horizontal = 8.dp) else Modifier.padding(horizontal = 14.dp, vertical = 10.dp)
      BubbleSurface {
        Column(
          modifier = padding,
          horizontalAlignment = Alignment.Start
        ) {
          Text(
            text = ci.content.text.ifEmpty { "Unsupported message" },
            fontSize = msgFontSize,
            textAlign = TextAlign.Start,
            style = chatTextStyle(msgFontSize, MaterialTheme.colors.secondary),
            color = MaterialTheme.colors.secondary
          )
        }
      }
    }
  }
}

@Preview
@Composable
fun PreviewFramedItemView() {
  SimpleXTheme {
    val showMenu = remember { mutableStateOf(false) }
    FramedItemView(
      chatsCtx = ChatModel.ChatsContext(secondaryContextFilter = null),
      chat = Chat.sampleData,
      ci = ChatItem.getSampleData(1, CIDirection.DirectRcv(), previewDate, "Hello world"),
      linkMode = SimplexLinkMode.DESCRIPTION,
      showMenu = showMenu,
      receiveFile = {}
    )
  }
}
