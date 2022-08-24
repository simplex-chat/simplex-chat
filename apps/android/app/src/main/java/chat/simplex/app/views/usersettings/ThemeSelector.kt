package chat.simplex.app.views.usersettings

import SectionItemViewSpaceBetween
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*

@Composable
fun ThemeSelectorView() {
  val darkTheme = isSystemInDarkTheme()
  val allThemes by remember { mutableStateOf(ThemeManager.allThemes(darkTheme)) }

  ThemeSelectorLayout(
    allThemes,
    onSelectTheme = {
      ThemeManager.applyTheme(it, darkTheme)
    },
  )
}

@Composable fun ThemeSelectorLayout(
  allThemes: List<Triple<Colors, DefaultTheme, String>>,
  onSelectTheme: (String) -> Unit,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    Text(
      stringResource(R.string.settings_section_title_themes).lowercase().capitalize(Locale.current),
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    val currentTheme by CurrentColors.collectAsState()
    SectionView(null) {
      LazyColumn(
        Modifier.padding(horizontal = 8.dp)
      ) {
        items(allThemes.size) { index ->
          val item = allThemes[index]
          val onClick = {
            onSelectTheme(item.second.name)
          }
          SectionItemViewSpaceBetween(onClick, padding = PaddingValues()) {
            Text(item.third)
            if (currentTheme.second == item.second) {
              Icon(Icons.Outlined.Check, item.third, tint = HighOrLowlight)
            }
          }
          Spacer(Modifier.padding(horizontal = 4.dp))
        }
      }
    }
  }
}

