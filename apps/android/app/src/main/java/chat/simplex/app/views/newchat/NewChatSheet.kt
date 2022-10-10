package chat.simplex.app.views.newchat

import androidx.activity.compose.BackHandler
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import androidx.core.graphics.ColorUtils
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

@Composable
fun NewChatSheet(chatModel: ChatModel, newChatSheetState: StateFlow<NewChatSheetState>, stopped: Boolean, closeNewChatSheet: (animated: Boolean) -> Unit) {
  if (newChatSheetState.collectAsState().value.isVisible()) BackHandler { closeNewChatSheet(true) }
  NewChatSheetLayout(
    newChatSheetState,
    stopped,
    addContact = {
      closeNewChatSheet(false)
      ModalManager.shared.showModal { CreateLinkView(chatModel, CreateLinkTab.ONE_TIME) }
    },
    connectViaLink = {
      closeNewChatSheet(false)
      ModalManager.shared.showModalCloseable { close -> ConnectViaLinkView(chatModel, close) }
    },
    createGroup = {
      closeNewChatSheet(false)
      ModalManager.shared.showCustomModal { close -> AddGroupView(chatModel, close) }
    },
    closeNewChatSheet,
  )
}

private val titles = listOf(R.string.share_one_time_link, R.string.connect_via_link_or_qr, R.string.create_group)
private val icons = listOf(Icons.Outlined.AddLink, Icons.Outlined.QrCode, Icons.Outlined.Group)

@Composable
private fun NewChatSheetLayout(
  newChatSheetState: StateFlow<NewChatSheetState>,
  stopped: Boolean,
  addContact: () -> Unit,
  connectViaLink: () -> Unit,
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
  val maxWidth = with(LocalDensity.current) { LocalConfiguration.current.screenWidthDp * density }
  Column(
    Modifier
      .fillMaxSize()
      .offset { IntOffset(if (newChat.isGone()) -maxWidth.roundToInt() else 0, 0) }
      .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null) { closeNewChatSheet(true) }
      .drawBehind { drawRect(animatedColor.value) },
    verticalArrangement = Arrangement.Bottom,
    horizontalAlignment = Alignment.End
  ) {
    val actions = remember { listOf(addContact, connectViaLink, createGroup) }
    val backgroundColor = if (isInDarkTheme())
      Color(ColorUtils.blendARGB(MaterialTheme.colors.primary.toArgb(), Color.Black.toArgb(), 0.7F))
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
              shape = RoundedCornerShape(21.dp),
              colors = ButtonDefaults.textButtonColors(backgroundColor = backgroundColor),
              elevation = null,
              contentPadding = PaddingValues(horizontal = DEFAULT_PADDING_HALF, vertical = DEFAULT_PADDING_HALF),
              modifier = Modifier.height(42.dp)
            ) {
              Text(
                stringResource(titles[index]),
                Modifier.padding(start = DEFAULT_PADDING_HALF),
                color = if (isInDarkTheme()) MaterialTheme.colors.primary else MaterialTheme.colors.primary,
                fontWeight = FontWeight.Medium,
              )
              Icon(
                icons[index],
                stringResource(titles[index]),
                Modifier.size(42.dp),
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
      Modifier.padding(end = 16.dp, bottom = 16.dp),
      elevation = FloatingActionButtonDefaults.elevation(
        defaultElevation = 0.dp,
        pressedElevation = 0.dp,
        hoveredElevation = 0.dp,
        focusedElevation = 0.dp,
      ),
      backgroundColor = if (!stopped) MaterialTheme.colors.primary else HighOrLowlight,
      contentColor = Color.White
    ) {
      Icon(
        Icons.Default.Edit, stringResource(R.string.add_contact_or_create_group),
        Modifier.graphicsLayer { alpha = 1 - animatedFloat.value }
      )
      Icon(
        Icons.Default.Close, stringResource(R.string.add_contact_or_create_group),
        Modifier.graphicsLayer { alpha = animatedFloat.value }
      )
    }
  }
}

@Composable
fun ActionButton(
  text: String?,
  comment: String?,
  icon: ImageVector,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(shape = RoundedCornerShape(18.dp)) {
    Column(
      Modifier
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
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
      MutableStateFlow(NewChatSheetState.VISIBLE),
      stopped = false,
      addContact = {},
      connectViaLink = {},
      createGroup = {},
      closeNewChatSheet = {},
    )
  }
}
