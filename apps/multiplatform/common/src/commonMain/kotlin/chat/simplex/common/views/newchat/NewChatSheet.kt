package chat.simplex.common.views.newchat

import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.RemoteHostInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.contacts.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

@Composable
fun ModalData.NewChatView(rh: RemoteHostInfo?) {
  Column(
    Modifier.fillMaxSize(),
  ) {
    Box(contentAlignment = Alignment.Center) {
      val bottomPadding = DEFAULT_PADDING
      AppBarTitle(
        stringResource(MR.strings.new_chat),
        hostDevice(rh?.remoteHostId),
        bottomPadding = bottomPadding
      )
    }

    ContactsView(
      contactActions = {
        NewChatOptions(
          addContact = {
            ModalManager.center.closeModals()
            ModalManager.center.showModalCloseable { close -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = close) }
          },
          scanPaste = {
            ModalManager.center.closeModals()
            ModalManager.center.showModalCloseable { close -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = true, close = close) }
          },
          createGroup = {
            ModalManager.center.closeModals()
            ModalManager.center.showCustomModal { close -> AddGroupView(chatModel, chatModel.currentRemoteHost.value, close) }
          }
        )
      }
    )
  }
}

@Composable
fun NewChatButton(icon: Painter, text: String, click: () -> Unit, textColor: Color = Color.Unspecified, iconColor: Color = MaterialTheme.colors.secondary, disabled: Boolean = false, extraPadding: Boolean = false) {
  SectionItemView(click, disabled = disabled) {
    Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else iconColor)
    TextIconSpaced(extraPadding)
    Text(text, color = if (disabled) MaterialTheme.colors.secondary else textColor)
  }
}

@Composable
fun NewChatOptions(addContact: () -> Unit, scanPaste: () -> Unit, createGroup: () -> Unit) {
  val actions = remember { listOf(addContact, scanPaste, createGroup) }

  LazyColumn {
    items(actions.size) { index ->
      NewChatButton(icon = painterResource(icons[index]), text = stringResource(titles[index]), click = actions[index], extraPadding = true)
    }
  }
}

@Composable
fun NewChatSheet(chatModel: ChatModel, newChatSheetState: StateFlow<AnimatedViewState>, stopped: Boolean, closeNewChatSheet: (animated: Boolean) -> Unit) {
  // TODO close new chat if remote host changes in model
  NewChatSheetLayout(
    newChatSheetState,
    stopped,
    addContact = {
      closeNewChatSheet(false)
      ModalManager.center.closeModals()
      ModalManager.center.showModalCloseable { close -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = close) }
    },
    scanPaste = {
      closeNewChatSheet(false)
      ModalManager.center.closeModals()
      ModalManager.center.showModalCloseable { close -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = true, close = close) }
    },
    createGroup = {
      ModalManager.center.closeModals()
      ModalManager.center.showCustomModal { close -> AddGroupView(chatModel, chatModel.currentRemoteHost.value, close) }
    },
    closeNewChatSheet,
  )
}

private val titles = listOf(
  MR.strings.add_contact_tab,
  MR.strings.scan_paste_link,
  MR.strings.create_group_button
)
private val icons = listOf(MR.images.ic_add_link, MR.images.ic_qr_code, MR.images.ic_group)

@Composable
private fun NewChatSheetLayout(
  newChatSheetState: StateFlow<AnimatedViewState>,
  stopped: Boolean,
  addContact: () -> Unit,
  scanPaste: () -> Unit,
  createGroup: () -> Unit,
  closeNewChatSheet: (animated: Boolean) -> Unit,
) {
  var newChat by remember { mutableStateOf(newChatSheetState.value) }
  val resultingColor = if (isInDarkTheme()) Color.Black.copy(0.64f) else DrawerDefaults.scrimColor
  val animatedColor = remember {
    Animatable(
      if (newChat.isVisible()) Color.Transparent else resultingColor,
      Color.VectorConverter(resultingColor.colorSpace)
    )
  }
  val animatedFloat = remember { Animatable(if (newChat.isVisible()) 0f else 1f) }
  LaunchedEffect(Unit) {
    launch {
      newChatSheetState.collect {
        newChat = it
        launch {
          animatedColor.animateTo(if (newChat.isVisible()) resultingColor else Color.Transparent, newChatSheetAnimSpec())
        }
        launch {
          animatedFloat.animateTo(if (newChat.isVisible()) 1f else 0f, newChatSheetAnimSpec())
          if (newChat.isHiding()) closeNewChatSheet(false)
        }
      }
    }
  }
  val endPadding = if (appPlatform.isDesktop) 56.dp else 0.dp
  val maxWidth = with(LocalDensity.current) { windowWidth() * density }
  Column(
    Modifier
      .fillMaxSize()
      .padding(end = endPadding)
      .offset { IntOffset(if (newChat.isGone()) -maxWidth.value.roundToInt() else 0, 0) }
      .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null) { closeNewChatSheet(true) }
      .drawBehind { drawRect(animatedColor.value) },
    verticalArrangement = Arrangement.Bottom,
    horizontalAlignment = Alignment.End
  ) {
    val actions = remember { listOf(addContact, scanPaste, createGroup) }
    val backgroundColor = if (isInDarkTheme())
      blendARGB(MaterialTheme.colors.primary, Color.Black, 0.7F)
    else
      MaterialTheme.colors.background
    LazyColumn(Modifier
      .graphicsLayer {
        alpha = animatedFloat.value
        translationY = (1 - animatedFloat.value) * 20.dp.toPx()
      }) {
      items(actions.size) { index ->
        Row {
          Spacer(Modifier.weight(1f))
          Box(contentAlignment = Alignment.CenterEnd) {
            Button(
              actions[index],
              shape = RoundedCornerShape(21.dp * fontSizeSqrtMultiplier),
              colors = ButtonDefaults.textButtonColors(backgroundColor = backgroundColor),
              elevation = null,
              contentPadding = PaddingValues(horizontal = DEFAULT_PADDING_HALF, vertical = DEFAULT_PADDING_HALF),
              modifier = Modifier.height(42.dp * fontSizeSqrtMultiplier)
            ) {
              Text(
                stringResource(titles[index]),
                Modifier.padding(start = DEFAULT_PADDING_HALF),
                color = if (isInDarkTheme()) MaterialTheme.colors.primary else MaterialTheme.colors.primary,
                fontWeight = FontWeight.Medium,
              )
              Icon(
                painterResource(icons[index]),
                stringResource(titles[index]),
                Modifier.size(42.dp * fontSizeSqrtMultiplier),
                tint = if (isInDarkTheme()) MaterialTheme.colors.primary else MaterialTheme.colors.primary
              )
            }
          }
          Spacer(Modifier.width(DEFAULT_PADDING))
        }
        Spacer(Modifier.height(DEFAULT_PADDING))
      }
    }
    FloatingActionButton(
      onClick = { if (!stopped) closeNewChatSheet(true) },
      Modifier.padding(end = DEFAULT_PADDING, bottom = DEFAULT_PADDING).size(AppBarHeight * fontSizeSqrtMultiplier),
      elevation = FloatingActionButtonDefaults.elevation(
        defaultElevation = 0.dp,
        pressedElevation = 0.dp,
        hoveredElevation = 0.dp,
        focusedElevation = 0.dp,
      ),
      backgroundColor = if (!stopped) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      contentColor = Color.White
    ) {
      Icon(
        painterResource(MR.images.ic_edit_filled), stringResource(MR.strings.add_contact_or_create_group),
        Modifier.graphicsLayer { alpha = 1 - animatedFloat.value }.size(24.dp * fontSizeSqrtMultiplier)
      )
      Icon(
        painterResource(MR.images.ic_close), stringResource(MR.strings.add_contact_or_create_group),
        Modifier.graphicsLayer { alpha = animatedFloat.value }.size(24.dp * fontSizeSqrtMultiplier)
      )
    }
  }
}

@Composable
fun ActionButton(
  text: String?,
  comment: String?,
  icon: Painter,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(shape = RoundedCornerShape(18.dp), color = Color.Transparent, contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Composable
fun ActionButton(
  modifier: Modifier,
  text: String?,
  comment: String?,
  icon: Painter,
  tint: Color = MaterialTheme.colors.primary,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(modifier, shape = RoundedCornerShape(18.dp), contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .fillMaxWidth()
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else tint
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Preview
@Composable
private fun PreviewNewChatSheet() {
  SimpleXTheme {
    NewChatSheetLayout(
      MutableStateFlow(AnimatedViewState.VISIBLE),
      stopped = false,
      addContact = {},
      scanPaste = {},
      createGroup = {},
      closeNewChatSheet = {},
    )
  }
}
