package chat.simplex.common.views.chat.item

import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState

@Composable
actual fun AndroidSaveOrOpenFileMenu(showMenu: MutableState<Boolean>, encrypted: Boolean, ext: String, openFile: () -> Unit, saveFile: () -> Unit) {}
