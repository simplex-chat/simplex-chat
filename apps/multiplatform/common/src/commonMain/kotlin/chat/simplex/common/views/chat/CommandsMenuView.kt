package chat.simplex.common.views.chat

import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.layout
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chat.item.sendCommandMsg
import chat.simplex.common.views.helpers.commandMenuAnimSpec
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.launch

private val COMMAND_MENU_ROW_SIZE = 48.dp
private val MAX_COMMAND_MENU_HEIGHT = COMMAND_MENU_ROW_SIZE * 6 - 8.dp

@Composable
fun CommandsMenuView(
  chatsCtx: ChatModel.ChatsContext,
  chat: Chat,
  composeState: MutableState<ComposeState>,
  showCommandsMenu: MutableState<Boolean>
) {
  val maxHeightInPx = with(LocalDensity.current) { windowHeight().toPx() }
  val offsetY = remember { Animatable(maxHeightInPx) }
  val scope = rememberCoroutineScope()

  val currentCommands = remember { mutableStateOf<List<ChatBotCommand>>(emptyList()) }
  val menuTreeBackPath = remember { mutableStateOf<List<Pair<String, List<ChatBotCommand>>>>(emptyList()) }

  fun filterShownCommands(commands: List<ChatBotCommand>, msg: CharSequence): List<ChatBotCommand> {
    val cmds = mutableListOf<ChatBotCommand>()
    for (cmd in commands) {
      when (cmd) {
        is ChatBotCommand.Command ->
          if (cmd.keyword.startsWith(msg)) {
            cmds.add(cmd)
          }
        is ChatBotCommand.Menu ->
          cmds.addAll(filterShownCommands(cmd.commands, msg))
      }
    }
    return cmds
  }

  suspend fun closeCommandsMenu() {
    showCommandsMenu.value = false
    currentCommands.value = emptyList()
    menuTreeBackPath.value = emptyList()
    if (offsetY.value != 0f) {
      return
    }
    offsetY.animateTo(
      targetValue = maxHeightInPx,
      animationSpec = commandMenuAnimSpec()
    )
  }

  fun messageChanged(message: String) {
    val msg = message.trim()
    menuTreeBackPath.value = emptyList()
    if (msg == "/") {
      currentCommands.value = chat.chatInfo.menuCommands
    } else if (msg.startsWith("/")) {
      currentCommands.value = filterShownCommands(chat.chatInfo.menuCommands, msg.drop(1))
    } else {
      scope.launch { closeCommandsMenu() }
    }
  }

  LaunchedEffect(currentCommands.value.isNotEmpty()) {
    if (currentCommands.value.isNotEmpty()) {
      offsetY.animateTo(
        targetValue = 0f,
        animationSpec = commandMenuAnimSpec()
      )
    }
  }

  LaunchedEffect(composeState.value.message) {
    messageChanged(composeState.value.message.text)
  }

  LaunchedEffect(showCommandsMenu.value) {
    if (showCommandsMenu.value) {
      currentCommands.value = chat.chatInfo.menuCommands
      menuTreeBackPath.value = emptyList()
    } else {
      closeCommandsMenu()
    }
  }

  @Composable
  fun MenuLabelRow(prev: Pair<String, List<ChatBotCommand>>) {
    Box(
      modifier = Modifier
        .fillMaxWidth()
        .height(COMMAND_MENU_ROW_SIZE)
        .clickable {
          if (menuTreeBackPath.value.isNotEmpty()) {
            currentCommands.value = menuTreeBackPath.value.last().second
            menuTreeBackPath.value = menuTreeBackPath.value.dropLast(1)
          }
        },
      contentAlignment = Alignment.Center
    ) {
      Row(Modifier.padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
        Icon(
          painterResource(MR.images.ic_arrow_back_ios_new),
          contentDescription = null,
          tint = MaterialTheme.colors.secondary
        )
        Spacer(Modifier.width(DEFAULT_PADDING_HALF))
        Text(
          text = prev.first,
          style = MaterialTheme.typography.body2,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Medium,
          maxLines = 1,
          modifier = Modifier.weight(1f),
          overflow = TextOverflow.Ellipsis
        )
      }
    }
  }

  @Composable
  fun CommandRow(cmd: ChatBotCommand) {
    when (cmd) {
      is ChatBotCommand.Command -> {
        Box(
          modifier = Modifier
            .fillMaxWidth()
            .height(COMMAND_MENU_ROW_SIZE)
            .clickable {
              if (cmd.params != null) {
                val msg = "/${cmd.keyword} ${cmd.params}"
                composeState.value = ComposeState(message = ComposeMessage(msg, TextRange(msg.length)), useLinkPreviews = true)
              } else {
                composeState.value = ComposeState(message = ComposeMessage(), useLinkPreviews = true)
                sendCommandMsg(chatsCtx, chat,"/${cmd.keyword}")
              }
              scope.launch { closeCommandsMenu() }
            },
          contentAlignment = Alignment.Center
        ) {
          Row(Modifier.padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
            Text(
              text = cmd.label,
              style = MaterialTheme.typography.body1,
              maxLines = 1,
              modifier = Modifier.weight(1f),
              textAlign = TextAlign.Start,
              overflow = TextOverflow.Ellipsis
            )
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(
              text = "/${cmd.keyword}",
              style = MaterialTheme.typography.body2,
              maxLines = 1,
              color = MaterialTheme.colors.secondary
            )
          }
        }
      }
      is ChatBotCommand.Menu ->
        Box(
          modifier = Modifier
            .fillMaxWidth()
            .height(COMMAND_MENU_ROW_SIZE)
            .clickable {
              menuTreeBackPath.value += Pair(cmd.label, currentCommands.value)
              currentCommands.value = cmd.commands
            },
          contentAlignment = Alignment.Center
        ) {
          Row(Modifier.padding(horizontal = DEFAULT_PADDING), verticalAlignment = Alignment.CenterVertically) {
            Text(
              text = cmd.label,
              style = MaterialTheme.typography.body1,
              fontWeight = FontWeight.Medium,
              maxLines = 1,
              modifier = Modifier.weight(1f),
              overflow = TextOverflow.Ellipsis
            )
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Icon(
              painterResource(MR.images.ic_chevron_right),
              contentDescription = null,
              tint = MaterialTheme.colors.secondary
            )
          }
        }
    }
  }

  Box(
    modifier = Modifier
      .fillMaxSize()
      .offset { IntOffset(0, offsetY.value.toInt()) }
      .clickable(indication = null, interactionSource = remember { MutableInteractionSource() }) {
        scope.launch { closeCommandsMenu() }
      },
    contentAlignment = Alignment.BottomStart
  ) {
    LazyColumnWithScrollBarNoAppBar(
      Modifier
        .heightIn(max = MAX_COMMAND_MENU_HEIGHT)
        .background(MaterialTheme.colors.surface),
      maxHeight = remember { mutableStateOf(MAX_COMMAND_MENU_HEIGHT) },
      containerAlignment = Alignment.BottomEnd
    ) {
      itemsIndexed(currentCommands.value, key = { i, cmd -> "$i ${cmd.hashCode()}" }) { i, command ->
        if (i == 0) {
          val prev = menuTreeBackPath.value.lastOrNull()
          if (prev != null) {
            Divider()
            MenuLabelRow(prev)
          }
        }
        Divider()
        Box(Modifier.fillMaxWidth()) { CommandRow(command) }
      }
    }
  }
}
