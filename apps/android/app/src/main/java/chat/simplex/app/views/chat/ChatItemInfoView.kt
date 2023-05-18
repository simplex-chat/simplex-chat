package chat.simplex.app.views.chat

import InfoRow
import SectionDividerSpaced
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.AppBarTitle

@Composable
fun ChatItemInfoView(ci: ChatItem, ciInfo: ChatItemInfo, devTools: Boolean) {
  Column(Modifier.fillMaxWidth().verticalScroll(rememberScrollState())) {
    AppBarTitle(stringResource(R.string.message_details))
    SectionView {
      InfoRow(stringResource(R.string.info_row_sent_at), ciInfo.itemTs.toString())
      if (!ci.chatDir.sent) {
        InfoRow(stringResource(R.string.info_row_received_at), ciInfo.createdAt.toString())
      }
      if (ciInfo.deleteAt != null) {
        InfoRow(stringResource(R.string.info_row_to_be_deleted_at), ciInfo.deleteAt.toString())
      }
      if (devTools) {
        InfoRow(stringResource(R.string.info_row_database_id), ciInfo.chatItemId.toString())
      }
    }
    if (ciInfo.itemVersions.isNotEmpty()) {
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
      SectionView {
        ciInfo.itemVersions.forEachIndexed { i, ciVersion ->
          ItemVersionView(ciVersion, current = i == 0)
        }
      }
    }
  }
}

@Composable
fun ItemVersionView(ciVersion: ChatItemVersion, current: Boolean) {

}
