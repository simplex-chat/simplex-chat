package chat.simplex.common.views.chat

import androidx.compose.runtime.*

// Android keyboards have their own emoji panel
@Composable
actual fun ComposeEmojiButton(pickerShown: MutableState<Boolean>, enabled: Boolean, onEmoji: (String) -> Unit) {}
