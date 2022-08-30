package chat.simplex.app.views.usersettings

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun IncognitoView() {
  IncognitoLayout()
}

@Composable
fun IncognitoLayout() {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    Text(
      stringResource(R.string.settings_section_title_incognito),
      Modifier.padding(start = 8.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )

    Column(
      Modifier
        .verticalScroll(rememberScrollState())
        .padding(horizontal = 8.dp)
    ) {
      Column(
        Modifier.padding(bottom = 16.dp),
        verticalArrangement = Arrangement.spacedBy(20.dp)
      ) {
        Text(generalGetString(R.string.incognito_info_protects))
        Text(generalGetString(R.string.incognito_info_allows))
        Text(generalGetString(R.string.incognito_info_share))
        Text(generalGetString(R.string.incognito_info_find))
      }
    }
  }
}
