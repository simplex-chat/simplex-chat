package chat.simplex.common.views.usersettings.networkAndServers

import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import chat.simplex.common.model.UserOperatorServers
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ModalData.YourServersView(userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.your_servers))
  }
}