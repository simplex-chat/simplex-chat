package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@OptIn(ExperimentalLayoutApi::class)
@Composable
actual fun TagsRow(content: @Composable() (() -> Unit)) {
  FlowRow(modifier = Modifier.padding(horizontal = 14.dp)) { content() }
}

@Composable
actual fun ActiveCallInteractiveArea(call: Call) {
  val showMenu = remember { mutableStateOf(false) }
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  if (oneHandUI.value) {
    ActiveCallInteractiveAreaOneHand(call, showMenu)
  } else {
    CompositionLocalProvider(
      LocalIndication provides NoIndication
    ) {
      ActiveCallInteractiveAreaNonOneHand(call, showMenu)
    }
  }
}

@Composable
private fun ActiveCallInteractiveAreaOneHand(call: Call, showMenu: MutableState<Boolean>) {
  Box(
    Modifier
      .minimumInteractiveComponentSize()
      .combinedClickable(onClick = {
        val chat = chatModel.getChat(call.contact.id)
        if (chat != null) {
          withBGApi {
            openChat(secondaryChatsCtx = null, chat.remoteHostId, chat.chatInfo)
          }
        }
      },
        onLongClick = { showMenu.value = true },
        role = Role.Button,
        interactionSource = remember { MutableInteractionSource() },
        indication = remember { ripple(bounded = false, radius = 24.dp) }
      )
      .onRightClick { showMenu.value = true },
    contentAlignment = Alignment.Center
  ) {
    ProfileImage(
      image = call.contact.profile.image,
      size = 37.dp * fontSizeSqrtMultiplier,
      color = MaterialTheme.colors.secondaryVariant.mixWith(MaterialTheme.colors.onBackground, 0.97f)
    )
    Box(
      Modifier.offset(x = 1.dp, y = (-1).dp).background(SimplexGreen, CircleShape).padding(3.dp)
        .align(Alignment.TopEnd)
    ) {
      if (call.hasVideo) {
        Icon(
          painterResource(MR.images.ic_videocam_filled),
          stringResource(MR.strings.icon_descr_video_call),
          Modifier.size(12.dp),
          tint = Color.White
        )
      } else {
        Icon(
          painterResource(MR.images.ic_call_filled),
          stringResource(MR.strings.icon_descr_audio_call),
          Modifier.size(12.dp),
          tint = Color.White
        )
      }
    }
    DefaultDropdownMenu(showMenu) {
      ItemAction(
        stringResource(MR.strings.icon_descr_hang_up),
        painterResource(MR.images.ic_call_end_filled),
        color = MaterialTheme.colors.error,
        onClick = {
          withBGApi { chatModel.callManager.endCall(call) }
          showMenu.value = false
        })
    }
  }
}

@Composable
private fun ActiveCallInteractiveAreaNonOneHand(call: Call, showMenu: MutableState<Boolean>) {
  Box(
    Modifier
      .fillMaxSize(),
    contentAlignment = Alignment.BottomEnd
  ) {
    Box(
      Modifier
        .padding(end = 15.dp, bottom = 92.dp)
        .size(67.dp)
        .combinedClickable(onClick = {
          val chat = chatModel.getChat(call.contact.id)
          if (chat != null) {
            withBGApi {
              openChat(secondaryChatsCtx = null, chat.remoteHostId, chat.chatInfo)
            }
          }
        },
          onLongClick = { showMenu.value = true })
        .onRightClick { showMenu.value = true },
      contentAlignment = Alignment.Center
    ) {
      Box(Modifier.background(MaterialTheme.colors.background, CircleShape)) {
        ProfileImageForActiveCall(size = 56.dp, image = call.contact.profile.image)
      }
      Box(
        Modifier.padding().background(SimplexGreen, CircleShape).padding(4.dp)
          .align(Alignment.TopEnd)
      ) {
        if (call.hasVideo) {
          Icon(
            painterResource(MR.images.ic_videocam_filled),
            stringResource(MR.strings.icon_descr_video_call),
            Modifier.size(18.dp),
            tint = Color.White
          )
        } else {
          Icon(
            painterResource(MR.images.ic_call_filled),
            stringResource(MR.strings.icon_descr_audio_call),
            Modifier.size(18.dp),
            tint = Color.White
          )
        }
      }
      DefaultDropdownMenu(showMenu) {
        ItemAction(
          stringResource(MR.strings.icon_descr_hang_up),
          painterResource(MR.images.ic_call_end_filled),
          color = MaterialTheme.colors.error,
          onClick = {
            withBGApi { chatModel.callManager.endCall(call) }
            showMenu.value = false
          })
      }
    }
  }
}
