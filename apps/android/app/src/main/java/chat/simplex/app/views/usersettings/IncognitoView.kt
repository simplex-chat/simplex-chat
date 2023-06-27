package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.generalGetString

@Composable
fun IncognitoView() {
  IncognitoLayout()
}

@Composable
fun IncognitoLayout() {
  Column {
    AppBarTitle(stringResource(R.string.settings_section_title_incognito))
    Column(
      Modifier
        .verticalScroll(rememberScrollState())
        .padding(horizontal = DEFAULT_PADDING),
      verticalArrangement = Arrangement.spacedBy(20.dp)
    ) {
      Text(generalGetString(R.string.incognito_info_protects))
      Text(generalGetString(R.string.incognito_info_allows))
      Text(generalGetString(R.string.incognito_info_share))
      Text(generalGetString(R.string.incognito_info_find))
      SectionBottomSpacer()
    }
  }
}
