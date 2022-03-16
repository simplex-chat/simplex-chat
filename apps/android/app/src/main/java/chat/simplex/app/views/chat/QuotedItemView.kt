package chat.simplex.app.views.chat

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBack
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.ChatItem
import chat.simplex.app.views.chat.item.*

@Composable
fun QuotedItemView(quotedItem: MutableState<ChatItem?>) {
  val qi = quotedItem.value
  if (qi != null) {
    Row {
      Box(Modifier.fillMaxWidth().weight(1F)) {
        QuoteText(qi)
      }
      IconButton(onClick = { quotedItem.value = null }) {
        Icon(
          Icons.Outlined.Close,
          "Remove quote",
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.padding(10.dp)
        )
      }
    }
//      HStack {
//        quoteText(qi).lineLimit(3)
//        Spacer()
//        Button {
//          withAnimation { quotedItem = nil }
//        } label: {
//          Image(systemName: "multiply")
//        }
//      }
//        .padding(12)
//        .frame(maxWidth: .infinity)
//      .background(chatItemFrameColor(qi, colorScheme))
//      .padding(.top, 8)
//    } else {
//      EmptyView()
  }
}

@Composable
private fun QuoteText(qi: ChatItem) {
  val member = qi.memberDisplayName
  if (member == null) {
    Text(qi.content.text, maxLines = 3)
  } else {
    val annotatedText = buildAnnotatedString {
      withStyle(boldFont) { append(member) }
      append(": ${qi.content.text}")
    }
    Text(annotatedText, maxLines = 3)
  }
}
