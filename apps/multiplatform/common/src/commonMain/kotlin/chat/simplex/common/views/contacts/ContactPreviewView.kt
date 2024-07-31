package chat.simplex.common.views.contacts

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.newchat.ContactType
import chat.simplex.common.views.newchat.chatContactType
import chat.simplex.res.MR

@Composable
fun ContactPreviewView(
    chat: Chat,
    disabled: Boolean,
) {
    val cInfo = chat.chatInfo
    val contactType = chatContactType(chat)

    @Composable
    fun VerifiedIcon() {
        Icon(painterResource(MR.images.ic_verified_user), null, Modifier.size(19.dp).padding(end = 3.dp, top = 1.dp), tint = MaterialTheme.colors.secondary)
    }

    @Composable
    fun chatPreviewTitle() {
        val deleting by remember(disabled, chat.id) { mutableStateOf(chatModel.deletedChats.value.contains(chat.remoteHostId to chat.chatInfo.id)) }

        val textColor = when {
            deleting -> MaterialTheme.colors.secondary
            contactType == ContactType.CARD -> MaterialTheme.colors.primary
            contactType == ContactType.REQUEST -> MaterialTheme.colors.primary
            contactType == ContactType.RECENT && chat.chatInfo.incognito -> Indigo
            else -> Color.Unspecified
        }

        when (cInfo) {
            is ChatInfo.Direct ->
                Row(verticalAlignment = Alignment.CenterVertically) {
                    if (cInfo.contact.verified) {
                        VerifiedIcon()
                    }
                    Text(
                        cInfo.chatViewName,
                        maxLines = 1,
                        overflow = TextOverflow.Ellipsis,
                        color = textColor
                    )
                }
            is ChatInfo.ContactRequest ->
                Row(verticalAlignment = Alignment.CenterVertically) {
                    Text(
                        cInfo.chatViewName,
                        maxLines = 1,
                        overflow = TextOverflow.Ellipsis,
                        color = textColor
                    )
                }
            else -> {}
        }
    }

    Row(
        modifier = Modifier.padding(PaddingValues(horizontal = DEFAULT_PADDING_HALF)),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Box(contentAlignment = Alignment.BottomEnd) {
            ChatInfoImage(cInfo, size = 42.dp)
        }

        Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))

        Box(modifier = Modifier.weight(10f, fill = true)) {
            chatPreviewTitle()
        }

        Spacer(Modifier.fillMaxWidth().weight(1f))

        if (chat.chatInfo is ChatInfo.ContactRequest) {
            Icon(
                painterResource(MR.images.ic_check),
                contentDescription = generalGetString(MR.strings.contact_type_new),
                tint = MaterialTheme.colors.secondary,
                modifier = Modifier
                    .size(17.dp)
            )
        }

        if (contactType == ContactType.CARD) {
            Icon(
                painterResource(MR.images.ic_mail),
                contentDescription = generalGetString(MR.strings.contact_type_new),
                tint = MaterialTheme.colors.secondary,
                modifier = Modifier
                    .size(17.dp)
            )
        }

        if (chat.chatInfo.chatSettings?.favorite == true) {
            Icon(
                painterResource(MR.images.ic_star_filled),
                contentDescription = generalGetString(MR.strings.favorite_chat),
                tint = MaterialTheme.colors.secondary,
                modifier = Modifier
                    .size(17.dp)
            )
            if (chat.chatInfo.incognito) {
                Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))
            }
        }


        if (chat.chatInfo.incognito) {
            Icon(
                painterResource(MR.images.ic_theater_comedy),
                contentDescription = null,
                tint = MaterialTheme.colors.secondary,
                modifier = Modifier
                    .size(21.dp)
            )
        }
    }
}